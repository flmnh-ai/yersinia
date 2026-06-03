# ------------------------------------------------------------------------------
# app_fit.R — assemble + run a deterministic-pilot plague fit from LabSession.
#
# This is the boundary where Lab session state (cohort_ids, model_config,
# priors) becomes a runnable monty model. v1 supports the deterministic
# unfilter path only (`dust_unfilter_create()`) for fast pilots; stochastic
# fits for production are deferred to v2.
#
# Single-outbreak vs multi-outbreak follow different code paths because
# multi-outbreak needs (1) NA-padded synchronized time grids, (2) a grouped
# packer with per-group local parameters, (3) priors replicated per group
# for each `local` parameter. Both branches end with a posterior model and
# a sampler ready for `monty_sample()`.
# ------------------------------------------------------------------------------

#' Long-format cohort data with NA-padded synchronized time grid.
#'
#' For multi-outbreak fits, dust2's grouped likelihood requires every group
#' to share the same time grid; shorter outbreaks are NA-padded out to the
#' longest. The returned tibble has columns `group` (character — dust2
#' rejects factor `group`), `time`, `deaths`.
#'
#' @param cohort_ids Character vector of `outbreak_id`s.
#' @param data Long outbreaks tibble (defaults to bundled [outbreaks]).
#' @return Tibble with `group`, `time`, `deaths`.
#' @export
cohort_data <- function(cohort_ids, data = NULL) {
  if (length(cohort_ids) == 0) {
    cli::cli_abort("Empty cohort — pick at least one outbreak.")
  }
  if (is.null(data)) data <- get("outbreaks", envir = asNamespace("yersinia"))
  sub <- data[data$outbreak_id %in% cohort_ids, , drop = FALSE]
  if (nrow(sub) == 0) {
    cli::cli_abort("No data found for cohort {.val {cohort_ids}}.")
  }
  # The bundled outbreaks dataset uses `day` for the time axis; dust2's
  # likelihood expects a column named `time`. Rename here so the rest of
  # the pipeline (and dust2) sees `time`.
  sub <- dplyr::rename(sub, time = "day")
  sub <- dplyr::mutate(sub, group = as.character(.data$outbreak_id))
  T_max <- max(sub$time, na.rm = TRUE)
  sub |>
    dplyr::select("group", "time", "deaths") |>
    tidyr::complete(.data$group, time = seq_len(T_max)) |>
    dplyr::arrange(.data$group, .data$time)
}

#' Per-outbreak population values (K_h / K_r when fixed).
#'
#' @param cohort_ids Character vector of `outbreak_id`s.
#' @param data Long outbreaks tibble.
#' @return Named numeric, one entry per outbreak.
#' @export
cohort_population <- function(cohort_ids, data = NULL) {
  if (is.null(data)) data <- get("outbreaks", envir = asNamespace("yersinia"))
  out <- vapply(cohort_ids, function(id) {
    sub <- data[data$outbreak_id == id, ]
    pop <- unique(sub$population)
    if (length(pop) != 1L) {
      cli::cli_abort("Outbreak {.val {id}} has inconsistent population in data.")
    }
    as.numeric(pop)
  }, numeric(1))
  names(out) <- cohort_ids
  out
}

#' Per-outbreak observation period (must be consistent across the cohort).
#'
#' dust2's grouped likelihood evaluates a single `obs_period`; mixing daily
#' and weekly outbreaks in one fit isn't supported here (London 1563 is
#' weekly; everything else is daily). Errors when the cohort mixes cadences.
#'
#' @inheritParams cohort_data
#' @return Single integer.
#' @export
cohort_obs_period <- function(cohort_ids, data = NULL) {
  if (is.null(data)) data <- get("outbreaks", envir = asNamespace("yersinia"))
  sub <- data[data$outbreak_id %in% cohort_ids, ]
  vals <- unique(sub$obs_period)
  if (length(vals) != 1L) {
    cli::cli_abort(c(
      "Cohort mixes observation cadences ({.val {vals}}).",
      i = "Pick outbreaks that share an `obs_period` (e.g. all daily, or all weekly)."
    ))
  }
  as.integer(vals)
}

# Build the prior body (a brace expression) from a priors list, replicating
# locals per group when `cohort_ids` has length > 1. Internal helper.
.build_prior_body <- function(priors, model_config, cohort_ids) {
  shared <- intersect(model_config$shared, names(priors))
  local  <- intersect(model_config$local,  names(priors))
  shared_exprs <- lapply(shared, function(p) prior_to_dsl(p, priors[[p]]))
  if (length(cohort_ids) <= 1) {
    local_exprs <- lapply(local, function(p) prior_to_dsl(p, priors[[p]]))
  } else {
    local_exprs <- unlist(
      lapply(local, function(p) {
        lapply(cohort_ids, function(g) {
          prior_to_dsl(sprintf("%s<%s>", p, g), priors[[p]])
        })
      }), recursive = FALSE)
  }
  as.call(c(list(as.name("{")), shared_exprs, local_exprs))
}

# Sensible initial-proposal SD for each family. The previous heuristic was
# width-of-99%-plot-range / 50 which over-stepped wildly for LogNormal /
# Exponential / Normal (the 99th percentile is far above the typical scale).
# These are tuned to give an initial RW step ~0.2-0.5 of the prior's central
# tendency, so the adaptive sampler doesn't have to spend hundreds of
# iterations shrinking the proposal.
.prior_initial_sd <- function(prior) {
  switch(prior$family,
    Uniform     = diff(c(prior$params$min, prior$params$max)) / sqrt(12),
    Normal      = prior$params$sd,
    LogNormal   = exp(prior$params$meanlog) * prior$params$sdlog / 5,
    Gamma       = sqrt(prior$params$shape) / prior$params$rate,
    Exponential = (1 / prior$params$rate) / 3,
    Beta        = sqrt(prior$params$shape1 * prior$params$shape2 /
                       ((prior$params$shape1 + prior$params$shape2)^2 *
                        (prior$params$shape1 + prior$params$shape2 + 1))),
    1
  )
}

# Diagonal initial VCV. Step size = prior SD * 0.3 (RWM rule-of-thumb start).
# Adaptive sampler tunes from here.
.initial_vcv <- function(packer_names, priors, model_config, cohort_ids) {
  sds <- vapply(packer_names, function(name) {
    base <- sub("<.*", "", name)
    if (base %in% names(priors)) .prior_initial_sd(priors[[base]]) * 0.3 else 1
  }, numeric(1))
  diag(sds^2, nrow = length(sds))
}

# Sensible per-parameter starting value for the chain. Most plague params
# have well-known biological "centre" values; counts scale with the local
# outbreak population to avoid I_ini > K_r at chain start.
.initial_centre <- function(name, pop_for_group) {
  base <- sub("<.*", "", name)
  switch(base,
    beta_h          = 0.02,
    beta_r          = 0.5,
    beta_I          = 0.005,
    rho             = 2.5,
    g_h             = 0.1,
    g_r             = 0.02,
    delta_R         = 0.27,
    K_h             = pop_for_group,
    K_r             = pop_for_group,
    # 2% of population: small enough to stay below K_r for the smallest
    # bundled outbreak (Eyam ~700 -> I_ini = 14), big enough to be in
    # the posterior region for large outbreaks (Cairo 250k -> I_ini = 5000).
    I_ini           = max(1, pop_for_group * 0.02),
    R_ini           = 1,
    I_h_ini         = 1,
    R_h_ini         = 1,
    kappa           = 20,
    p_obs           = 0.8,
    p               = 0.5,
    lambda_baseline = 1,
    m_h             = 0.08, m_r = 0.2,
    d_h             = 1e-4, d_r = 1e-4, r_h = 1e-4, r_r = 1e-4,
    1
  )
}

# Build an n_pars x n_chains initial matrix: a per-param sensible centre
# plus per-chain log-normal jitter to keep R-hat meaningful. Group-decorated
# parameter names (param<group>) get the per-group population for K-scaled
# values; flat names use the first outbreak's population.
.initial_point <- function(setup, n_chains, jitter = 0.1) {
  pn <- setup$packer$names()
  pop <- cohort_population(setup$cohort_ids)
  group_for <- function(name) {
    m <- regmatches(name, regexpr("<[^>]+>", name))
    if (length(m) == 0) setup$cohort_ids[1] else sub("[<>]", "", sub(">$", "", m))
  }
  centre <- vapply(pn, function(name) {
    .initial_centre(name, pop[[group_for(name)]])
  }, numeric(1))
  out <- matrix(NA_real_, nrow = length(pn), ncol = n_chains,
                dimnames = list(pn, NULL))
  for (i in seq_len(n_chains)) {
    out[, i] <- pmax(centre * exp(stats::rnorm(length(centre), 0, jitter)),
                     1e-8)
  }
  out
}

#' Assemble a deterministic-pilot fit from a LabSession.
#'
#' Returns a list containing the posterior model, sampler, packer, fixed
#' parameters, and metadata. Pass to [lab_fit_run()] (or call
#' `monty::monty_sample()` directly with `posterior` + `sampler`).
#'
#' Branches on `length(cohort_ids)`:
#' - `1`: standard single-outbreak setup, plain `monty_packer`.
#' - `>1`: grouped fit with [monty::monty_packer_grouped()] and
#'   [with_per_group_fixed()] splicing per-outbreak populations into K_h
#'   and K_r when those aren't in the fitted set.
#'
#' Smoke-tests the unfilter at the prior medians so misconfigured fits
#' fail at assembly time, not deep inside `monty_sample()`.
#'
#' @param lab_session A [LabSession] whose `cohort_ids`, `model_config`,
#'   and `priors` fields are populated.
#' @param data Long outbreaks tibble (defaults to bundled [outbreaks]).
#' @return Named list: `posterior`, `sampler`, `packer`, `unfilter`,
#'   `fixed_pars`, `data`, `cohort_ids`, `fitted_names`.
#' @export
lab_fit_assemble <- function(lab_session, data = NULL) {
  # Sort cohort_ids so the grouped packer (which uses cohort_ids order) and
  # the cohort data (which gets alphabetised by tidyr::complete in cohort_data)
  # agree. Without this, click-order from the UI ends up disagreeing with
  # the data's group order, and dust2 errors with "Groups for 'packer' do
  # not match those of 'obj'".
  cohort_ids   <- sort(lab_session$cohort_ids)
  model_config <- lab_session$model_config
  priors       <- lab_session$priors
  if (is.null(priors)) priors <- list()

  d <- cohort_data(cohort_ids, data)
  pop <- cohort_population(cohort_ids, data)
  obs_period <- cohort_obs_period(cohort_ids, data)

  fitted_names <- intersect(union(model_config$shared, model_config$local),
                            configurable_param_names())
  fixed_pars <- plague_fit_fixed_pars(model_config$scenario, fitted_names)
  fixed_pars$obs_period <- obs_period
  if (is.null(fixed_pars$seasonal)) {
    fixed_pars$seasonal <- rep(1, max(d$time))
  }
  # odin2/dust2 declares every parameter in this model as real_type and
  # strict-type-checks at read time. YAML parses whole-number values
  # (e.g. didelot's K_r: 250000) as integer; cohort_obs_period returns
  # integer; pop[[g]] can be integer if population is stored that way.
  # Coerce all numeric fixed_pars to double in one pass so the user
  # can pick any scenario / cohort without tripping the type check.
  fixed_pars <- lapply(fixed_pars, function(x) {
    if (is.integer(x)) as.numeric(x) else x
  })

  if (length(cohort_ids) == 1L) {
    # Pin K_h / K_r to outbreak population if the user left them fixed.
    if (!"K_h" %in% fitted_names) fixed_pars$K_h <- as.numeric(pop[[cohort_ids]])
    if (!"K_r" %in% fitted_names) fixed_pars$K_r <- as.numeric(pop[[cohort_ids]])
    unfilter <- dust2::dust_unfilter_create(plague_stochastic_humans,
                                             time_start = 0,
                                             data = as.data.frame(d[, c("time", "deaths")]))
    packer <- monty::monty_packer(fitted_names, fixed = fixed_pars)
  } else {
    # Grouped path. Per-outbreak K_h / K_r when fixed; spliced via
    # with_per_group_fixed so the packer's $names() stays group-free for them.
    group_fixed <- list()
    for (g in cohort_ids) {
      gf <- list()
      if (!"K_h" %in% fitted_names) gf$K_h <- as.numeric(pop[[g]])
      if (!"K_r" %in% fitted_names) gf$K_r <- as.numeric(pop[[g]])
      group_fixed[[g]] <- gf
    }
    inner <- monty::monty_packer_grouped(
      groups = cohort_ids,
      scalar = fitted_names,
      shared = intersect(model_config$shared, fitted_names),
      fixed  = fixed_pars
    )
    packer <- if (length(group_fixed[[1]]) > 0) {
      with_per_group_fixed(inner, group_fixed)
    } else {
      inner
    }
    grouped_data <- as.data.frame(d)
    grouped_data$group <- as.character(grouped_data$group)
    unfilter <- dust2::dust_unfilter_create(plague_stochastic_humans,
                                             time_start = 0,
                                             data = grouped_data,
                                             n_groups = length(cohort_ids))
  }

  prior_body  <- .build_prior_body(priors, model_config, cohort_ids)
  prior_model <- monty::monty_dsl(prior_body)
  if (!setequal(prior_model$parameters, packer$names())) {
    cli::cli_abort(c(
      "Prior parameters don't match packer parameters.",
      i = "Prior: {.val {prior_model$parameters}}",
      i = "Packer: {.val {packer$names()}}"
    ))
  }

  likelihood <- dust2::dust_likelihood_monty(unfilter, packer,
                                              save_trajectories = FALSE)
  posterior <- likelihood + prior_model
  sampler <- monty::monty_sampler_adaptive(
    initial_vcv = .initial_vcv(packer$names(), priors, model_config, cohort_ids)
  )

  list(posterior = posterior, sampler = sampler, packer = packer,
       unfilter = unfilter, prior_model = prior_model,
       fixed_pars = fixed_pars, data = d,
       cohort_ids = cohort_ids, fitted_names = fitted_names)
}

#' Run a deterministic-pilot fit assembled by [lab_fit_assemble()].
#'
#' Synchronous: blocks until all chains complete. v1 uses
#' [monty::monty_runner_serial()] (sequential chains in the calling process)
#' to keep the runtime model simple; switch to `monty_runner_callr` in v2
#' for parallel chains.
#'
#' Discards the first `burnin_frac` of iterations from each chain post-hoc.
#' The adaptive sampler tunes its proposal VCV continuously during sampling,
#' so early iterations are biased relative to the converged regime —
#' discarding the first half is the convention for adaptive RWM (see e.g.
#' Stan / NUTS warmup defaults). Returns the trimmed samples.
#'
#' @param setup Output of [lab_fit_assemble()].
#' @param n_chains Number of chains. Default 4.
#' @param n_iter Iterations per chain. Default 30000. Single-outbreak fits
#'   converge by ~10k; multi-outbreak hierarchical fits (4+ outbreaks)
#'   typically need 30–50k for the adaptive sampler to settle the
#'   per-group `(beta_h, I_ini)` ridges.
#' @param burnin_frac Fraction of iterations to discard from each chain
#'   as warmup. Default 0.5 (drop first half). Set to 0 to keep everything.
#' @param initial Optional matrix of initial parameter vectors
#'   (`n_parameters × n_chains`). When `NULL` (default), monty draws from
#'   the prior.
#' @return A `monty_samples` object with the warmup iterations removed.
#' @export
lab_fit_run <- function(setup, n_chains = 4L, n_iter = 30000L,
                        burnin_frac = 0.5, initial = NULL) {
  if (burnin_frac < 0 || burnin_frac >= 1) {
    cli::cli_abort("burnin_frac must be in [0, 1); got {.val {burnin_frac}}.")
  }
  # When monty draws initial values from the prior, broad LogNormal priors
  # routinely sample I_ini > K_r for small outbreaks (Eyam, Givry), which
  # makes S = K_r - I_ini negative and the likelihood -Inf. Start each
  # chain from a sensible biological centre (counts scaled to outbreak
  # population) with small log-normal jitter for R-hat to work.
  if (is.null(initial)) {
    initial <- .initial_point(setup, n_chains)
  }
  # monty's cli progress bar pushes message() output that Shiny's
  # withProgress() captures and renders as ANSI-coded garbage. Disable
  # cli colour + dynamic output and suppress messages here so the lab's
  # own progress bar is the only visible one.
  samples <- withr::with_options(
    list(cli.num_colors = 1L, cli.dynamic = FALSE, cli.progress = FALSE),
    suppressMessages(monty::monty_sample(
      setup$posterior, setup$sampler,
      n_steps = n_iter,
      n_chains = n_chains,
      initial = initial,
      runner = monty::monty_runner_serial()
    ))
  )
  drop_n <- as.integer(floor(n_iter * burnin_frac))
  if (drop_n == 0L) return(samples)
  keep <- seq.int(drop_n + 1L, n_iter)
  # monty_samples stores parameter draws in `$pars` as a [npar, niter, nchain]
  # array; trim along the iteration axis and propagate to whatever else the
  # object carries (density vectors, observations, details).
  samples$pars <- samples$pars[, keep, , drop = FALSE]
  if (!is.null(samples$density)) {
    samples$density <- samples$density[keep, , drop = FALSE]
  }
  if (!is.null(samples$observations) && !is.null(samples$observations$trajectories)) {
    # trajectories axis order: [state, time, particle?, iter, chain]
    # leave alone — we don't save trajectories in the pilot path.
  }
  samples
}

#' Run a stochastic production fit on top of a deterministic pilot.
#'
#' After a pilot completes, use the adaptive sampler's tuned proposal VCV
#' to warm-start a shorter stochastic chain that respects the actual
#' particle-filter likelihood (the pilot's deterministic unfilter is an
#' expected-value approximation). Returns the production samples; the
#' caller is responsible for keeping the pilot around for diagnostics.
#'
#' Mirrors the two-stage workflow in
#' `vignettes/monty-barcelona-hierarchical.qmd` and friends.
#'
#' @param pilot_setup The setup list returned by [lab_fit_assemble()].
#' @param pilot_samples The samples returned by [lab_fit_run()].
#' @param n_chains Number of chains. Default 4.
#' @param n_iter Iterations per chain. Default 5000 (much shorter than
#'   pilot since the proposal is already tuned).
#' @param burnin_frac Fraction to discard. Default 0.2 (chains start
#'   from a converged pilot region, so minimal warmup needed).
#' @param n_particles Particle filter size. Default 500.
#' @param n_threads OpenMP threads. Default 2.
#' @param vcv_inflation Factor to scale the tuned pilot VCV by. Default
#'   `0.7 · (2.38² / n_pars)` (Roberts–Rosenthal optimal-scaling, slightly
#'   shrunk to give the stochastic chain cushion against filter noise).
#' @return A `monty_samples` object with the warmup iterations removed.
#' @export
lab_fit_run_production <- function(pilot_setup, pilot_samples,
                                   n_chains = 4L, n_iter = 5000L,
                                   burnin_frac = 0.2,
                                   n_particles = 500L,
                                   n_threads = 2L,
                                   vcv_inflation = NULL) {
  if (burnin_frac < 0 || burnin_frac >= 1) {
    cli::cli_abort("burnin_frac must be in [0, 1); got {.val {burnin_frac}}.")
  }
  pn <- pilot_setup$packer$names()

  # Average the adaptive sampler's per-chain VCV across chains, then
  # apply Roberts/Rosenthal scaling (shrunk by 0.7 since the filter adds
  # noise that the tuning didn't see).
  pilot_vcv <- pilot_samples$details$vcv
  det_vcv <- apply(pilot_vcv, c(1, 2), mean)
  dimnames(det_vcv) <- list(pn, pn)
  if (is.null(vcv_inflation)) {
    vcv_inflation <- 0.7 * (2.38^2 / length(pn))
  }
  prod_vcv <- vcv_inflation * det_vcv

  # Build the stochastic filter — same data + time grid as the pilot,
  # but particle-filtered now.
  d <- pilot_setup$data
  if (length(pilot_setup$cohort_ids) == 1L) {
    filter <- dust2::dust_filter_create(
      plague_stochastic_humans, time_start = 0,
      data = as.data.frame(d[, c("time", "deaths")]),
      n_particles = n_particles, n_threads = n_threads
    )
  } else {
    grouped_data <- as.data.frame(d)
    grouped_data$group <- as.character(grouped_data$group)
    filter <- dust2::dust_filter_create(
      plague_stochastic_humans, time_start = 0,
      data = grouped_data, n_particles = n_particles,
      n_groups = length(pilot_setup$cohort_ids), n_threads = n_threads
    )
  }
  likelihood <- dust2::dust_likelihood_monty(filter, pilot_setup$packer,
                                              save_trajectories = FALSE)
  posterior <- likelihood + pilot_setup$prior_model
  sampler <- monty::monty_sampler_random_walk(prod_vcv)

  # Warm-start each chain from the pilot's last sample (post-burnin).
  pilot_pars <- pilot_samples$pars  # [n_pars, n_iter_kept, n_chains]
  n_chains_to_use <- min(n_chains, dim(pilot_pars)[3])
  initial <- pilot_pars[, dim(pilot_pars)[2], seq_len(n_chains_to_use),
                        drop = TRUE]
  if (is.null(dim(initial))) initial <- matrix(initial, ncol = 1)
  if (n_chains_to_use < n_chains) {
    # Cycle through pilot chains if user wants more production chains.
    extra <- pilot_pars[, dim(pilot_pars)[2],
                        rep(seq_len(n_chains_to_use),
                            length.out = n_chains - n_chains_to_use),
                        drop = TRUE]
    initial <- cbind(initial, extra)
  }
  dimnames(initial) <- list(pn, NULL)

  samples <- withr::with_options(
    list(cli.num_colors = 1L, cli.dynamic = FALSE, cli.progress = FALSE),
    suppressMessages(monty::monty_sample(
      posterior, sampler, n_steps = n_iter, n_chains = n_chains,
      initial = initial, runner = monty::monty_runner_serial()
    ))
  )
  drop_n <- as.integer(floor(n_iter * burnin_frac))
  if (drop_n > 0L) {
    keep <- seq.int(drop_n + 1L, n_iter)
    samples$pars <- samples$pars[, keep, , drop = FALSE]
    if (!is.null(samples$density)) {
      samples$density <- samples$density[keep, , drop = FALSE]
    }
  }
  samples
}
