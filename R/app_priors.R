# ------------------------------------------------------------------------------
# app_priors.R — prior-distribution helpers for the Virtual Lab.
#
# A prior is a small list:
#
#   list(family = "Uniform", params = list(min = 0.001, max = 0.15))
#
# `family` names a row in `prior_families()`; `params` is a named list whose
# keys match the family's spec. The full priors object stored in LabSession is
# a *named* list keyed by parameter name:
#
#   priors = list(
#     beta_h = list(family = "Uniform", params = list(min = 0.001, max = 0.15)),
#     kappa  = list(family = "Exponential", params = list(rate = 0.1))
#   )
#
# Helpers convert this representation to:
#   - per-prior density tibbles for live plotting (prior_density)
#   - monty-DSL expressions for the sampler   (prior_to_dsl, priors_to_dsl)
# ------------------------------------------------------------------------------

#' Registry of supported prior distribution families.
#'
#' Each entry is a list with:
#' - `params`: named list of parameter specs `(default, min, max, label)`,
#'   in the order the family expects them when called via the monty DSL
#' - `density`: function `(x, p)` returning the PDF at `x`, where `p` is a
#'   named list of parameter values
#' - `range`: function `(p)` returning a length-2 numeric `c(low, high)` —
#'   sensible plot range for the family at parameter values `p`
#'
#' v1 supports six families: `Uniform`, `Normal`, `LogNormal`, `Gamma`,
#' `Exponential`, `Beta`. Names match the monty DSL exactly.
#'
#' @return Named list of family definitions.
#' @export
prior_families <- function() {
  list(
    Uniform = list(
      params = list(
        min = list(default = 0,   min = -Inf, max = Inf, label = "min"),
        max = list(default = 1,   min = -Inf, max = Inf, label = "max")
      ),
      density = function(x, p) stats::dunif(x, p$min, p$max),
      range   = function(p) {
        w <- p$max - p$min
        c(p$min - 0.05 * w, p$max + 0.05 * w)
      }
    ),
    Normal = list(
      params = list(
        mean = list(default = 0,  min = -Inf, max = Inf, label = "mean"),
        sd   = list(default = 1,  min = 0,    max = Inf, label = "sd")
      ),
      density = function(x, p) stats::dnorm(x, p$mean, p$sd),
      range   = function(p) c(p$mean - 4 * p$sd, p$mean + 4 * p$sd)
    ),
    LogNormal = list(
      params = list(
        meanlog = list(default = 0, min = -Inf, max = Inf, label = "meanlog"),
        sdlog   = list(default = 1, min = 0,    max = Inf, label = "sdlog")
      ),
      density = function(x, p) stats::dlnorm(x, p$meanlog, p$sdlog),
      range   = function(p) c(0, stats::qlnorm(0.99, p$meanlog, p$sdlog))
    ),
    Gamma = list(
      params = list(
        shape = list(default = 2, min = 0, max = Inf, label = "shape"),
        rate  = list(default = 1, min = 0, max = Inf, label = "rate")
      ),
      density = function(x, p) stats::dgamma(x, shape = p$shape, rate = p$rate),
      range   = function(p) c(0, stats::qgamma(0.99, p$shape, rate = p$rate))
    ),
    Exponential = list(
      params = list(
        rate = list(default = 1, min = 0, max = Inf, label = "rate")
      ),
      density = function(x, p) stats::dexp(x, p$rate),
      range   = function(p) c(0, stats::qexp(0.99, p$rate))
    ),
    Beta = list(
      params = list(
        shape1 = list(default = 2, min = 0, max = Inf, label = "shape1 (α)"),
        shape2 = list(default = 2, min = 0, max = Inf, label = "shape2 (β)")
      ),
      density = function(x, p) stats::dbeta(x, p$shape1, p$shape2),
      range   = function(p) c(0, 1)
    )
  )
}

#' Default prior for a single fittable parameter.
#'
#' Hand-picked defaults seeded from the literature and from the pilots in the
#' existing monty vignettes. Returns `NULL` when there is no canonical default
#' for `parameter` — the caller should fall back to `Uniform(0, 1)` or prompt
#' the user.
#'
#' @param parameter Name of a model parameter.
#' @return A prior list (`family`, `params`) or `NULL`.
#' @export
prior_default <- function(parameter) {
  pri <- function(family, ...) {
    list(family = family, params = list(...))
  }
  # Defaults aligned with the canonical `plague_fit_prior()` in monty_fit.R
  # and the wider per-outbreak priors used in `vignettes/monty-seasonal.qmd`
  # and the Barcelona hierarchical fits. The lab is multi-outbreak by design,
  # so we take the *wider* of the vignette priors when they diverge.
  #
  # Three families are used for shape reasons (not just bounds):
  # - LogNormal for counts (K_h, K_r, I_ini) spanning orders of magnitude;
  #   Uniform on a linear scale would put 95% of prior mass on the high
  #   end and fight the data for small outbreaks like Eyam.
  # - Exponential for params that are usually near zero (beta_I, I_h_ini,
  #   R_h_ini); Uniform there overweighs biologically rare regimes.
  # - Beta for survival probabilities on [0, 1]; smooth and concentrates
  #   around clinical-literature CFR values.
  switch(parameter,
    beta_h          = pri("Uniform",     min = 0.001, max = 0.5),
    beta_r          = pri("Uniform",     min = 0.1,   max = 3.0),
    beta_I          = pri("Exponential", rate = 50),                    # mean 0.02
    rho             = pri("Uniform",     min = 0.3,   max = 6.0),
    g_h             = pri("Beta",        shape1 = 2,  shape2 = 8),      # mode ~0.125, 90% CFR
    g_r             = pri("Beta",        shape1 = 1,  shape2 = 20),     # near 0
    delta_R         = pri("Uniform",     min = 0.05,  max = 1.5),
    K_h             = pri("LogNormal",   meanlog = 9.9, sdlog = 1.5),   # median 20k
    K_r             = pri("LogNormal",   meanlog = 9.9, sdlog = 1.5),   # median 20k
    I_ini           = pri("LogNormal",   meanlog = 6,   sdlog = 2),     # median 400
    R_ini           = pri("Uniform",     min = 0,     max = 1e4),
    I_h_ini         = pri("Exponential", rate = 0.005),                 # mean 200
    R_h_ini         = pri("Exponential", rate = 0.01),                  # mean 100
    kappa           = pri("Gamma",       shape = 2,   rate = 0.05),
    p_obs           = pri("Uniform",     min = 0,     max = 1),
    p               = pri("Uniform",     min = 0,     max = 1),
    lambda_baseline = pri("Exponential", rate = 0.1),
    m_h             = pri("Uniform",     min = 0.01,  max = 0.5),
    m_r             = pri("Uniform",     min = 0.01,  max = 0.5),
    d_h             = pri("Uniform",     min = 1e-4,  max = 0.1),
    d_r             = pri("Uniform",     min = 1e-4,  max = 0.1),
    r_h             = pri("Uniform",     min = 1e-4,  max = 0.1),
    r_r             = pri("Uniform",     min = 1e-4,  max = 0.1),
    NULL
  )
}

#' Build a default priors list for a set of fitted parameters.
#'
#' For each parameter in `fitted_params`, returns its [prior_default()] or
#' falls back to `Uniform(0, 1)` if no canonical default exists.
#'
#' @param fitted_params Character vector of parameter names.
#' @return Named list of priors keyed by parameter name.
#' @export
priors_default <- function(fitted_params) {
  out <- lapply(fitted_params, function(p) {
    prior_default(p) %||% list(family = "Uniform",
                                params = list(min = 0, max = 1))
  })
  names(out) <- fitted_params
  out
}

#' Evaluate a prior's density on a grid for plotting.
#'
#' Returns a tibble with columns `x` and `density`, suitable for ggplot.
#' The grid spans the family's [prior_families()] `range(params)` with `n`
#' points; densities outside the support of compactly-supported families
#' (Uniform, Beta) are zero by definition of the underlying R function.
#'
#' @param prior A prior list (`family`, `params`).
#' @param n Number of grid points. Default 200.
#' @return A tibble with columns `x` and `density`.
#' @export
prior_density <- function(prior, n = 200) {
  fam <- prior_families()[[prior$family]]
  if (is.null(fam)) {
    cli::cli_abort("Unknown prior family: {.val {prior$family}}")
  }
  rng <- fam$range(prior$params)
  if (!all(is.finite(rng)) || rng[1] >= rng[2]) {
    return(tibble::tibble(x = numeric(0), density = numeric(0)))
  }
  x <- seq(rng[1], rng[2], length.out = n)
  tibble::tibble(x = x, density = fam$density(x, prior$params))
}

#' Convert one prior to a monty-DSL expression.
#'
#' Produces an unevaluated expression of the form `name ~ Family(args)`,
#' where the args are taken from `prior$params` in the order the family
#' declares them. Suitable for splicing into [monty::monty_dsl()].
#'
#' @param name Parameter name (will become the LHS of `~`).
#' @param prior A prior list (`family`, `params`).
#' @return A length-1 unevaluated expression.
#' @export
prior_to_dsl <- function(name, prior) {
  fam <- prior_families()[[prior$family]]
  if (is.null(fam)) {
    cli::cli_abort("Unknown prior family: {.val {prior$family}}")
  }
  ordered_args <- prior$params[names(fam$params)]
  call <- as.call(c(list(as.name(prior$family)),
                    unname(ordered_args)))
  bquote(.(as.name(name)) ~ .(call))
}

#' Hard-bound interval implied by a prior, for piling detection.
#'
#' Returns a length-2 numeric `c(low, high)`. Only families with a *finite*
#' interval support produce both ends finite — `Uniform` and `Beta`. For
#' families with infinite or one-sided support (`Normal`, `LogNormal`,
#' `Gamma`, `Exponential`), the unbounded side is `Inf`/`-Inf`. The
#' [diagnose_bound_piling()] detector skips parameters with non-finite
#' bounds, so this function safely "no-ops" the diagnostic for unbounded
#' priors.
#'
#' @param prior A prior list (`family`, `params`).
#' @return Length-2 numeric `c(low, high)`.
#' @export
prior_bounds <- function(prior) {
  switch(prior$family,
    Uniform     = c(prior$params$min,  prior$params$max),
    Beta        = c(0, 1),
    Exponential = c(0, Inf),
    LogNormal   = c(0, Inf),
    Gamma       = c(0, Inf),
    Normal      = c(-Inf, Inf),
    c(-Inf, Inf)
  )
}

#' Bounds for every parameter in a packer, including per-group decorations.
#'
#' Maps each `packer_names` entry back to its base parameter (stripping any
#' `<group>` decoration), looks up that parameter's prior, and returns the
#' bounds via [prior_bounds()]. Suitable for passing to
#' [diagnose_bound_piling()].
#'
#' @param priors Named list of priors.
#' @param packer_names Character vector of (possibly decorated) parameter
#'   names from `packer$names()`.
#' @return Named list of `c(low, high)` keyed by `packer_names`.
#' @export
priors_to_bounds <- function(priors, packer_names) {
  bounds <- list()
  for (name in packer_names) {
    base <- sub("<.*", "", name)
    if (base %in% names(priors)) {
      bounds[[name]] <- prior_bounds(priors[[base]])
    }
  }
  bounds
}

#' Convert a full priors list to a list of monty-DSL expressions.
#'
#' @param priors Named list of priors (see file header).
#' @return List of unevaluated expressions in the same order as `priors`.
#' @export
priors_to_dsl <- function(priors) {
  Map(prior_to_dsl, names(priors), priors)
}
