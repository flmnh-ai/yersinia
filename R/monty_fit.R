# ------------------------------------------------------------------------------
# monty_fit.R — shared assembly for monty / dust2 / odin2 plague fits.
#
# Both monty.R (standalone SLURM driver) and vignettes/monty.qmd call the
# helpers here so the fitting pipeline has one definition. Anything that
# either caller can reasonably override (fitted parameter set, prior, VCV,
# number of particles/threads, fixed-parameter scenario) is a function arg.
# The odin2 model itself lives in inst/odin/plague_stochastic_humans.R and
# is exported by the package as `plague_stochastic_humans` (a dust2 system
# generator compiled at build time by odin2::odin_package()).
# ------------------------------------------------------------------------------

# Names that the humans odin2 model accepts. Scenario YAML often carries extra
# keys (e.g. `mu_r` for the spatial model); we filter against this list before
# handing parameters to dust2 so it doesn't reject the call.
plague_model_param_names <- function() {
  c("tau", "I_ini", "R_ini", "K_r", "K_h", "r_r", "r_h",
    "p", "d_r", "d_h", "beta_r", "beta_h", "beta_I", "rho",
    "m_r", "m_h", "g_r", "g_h", "delta_R", "kappa", "p_obs", "iota",
    "seasonal", "I_h_ini", "R_h_ini", "lambda_baseline", "obs_period")
}

#' Default fitted parameter names for plague humans fits.
#'
#' These are the parameters swept by the monty sampler in [plague_fit_setup()].
#' If you change this set, you also need to provide a matching `prior` and
#' `vcv` — the defaults returned by [plague_fit_prior()] and [plague_fit_vcv()]
#' are built for exactly this ordering.
#'
#' The model also supports `p_obs` (observation/ascertainment probability,
#' defaults to 1 = perfect reporting) which is deliberately NOT fitted by
#' default. To opt in, pass e.g.
#' `fitted_names = c(plague_fit_fitted_names(), "p_obs")` to
#' [plague_fit_setup()] with a matching prior
#' (`p_obs ~ Uniform(0, 1)` follows the 2009-flu example in the odin-monty
#' book) and an extended VCV.
#'
#' @return Character vector of parameter names.
#' @export
plague_fit_fitted_names <- function() {
  c("beta_h", "beta_r", "rho", "g_h", "K_r", "K_h", "I_ini", "kappa")
}

#' Build the fixed (non-fitted) parameter list for a plague humans fit.
#'
#' Loads a scenario via [load_scenario()], drops any parameter that the
#' sampler will vary, and filters to just the keys the odin2 humans model
#' accepts. The returned list is suitable for the `fixed` argument of
#' `monty::monty_packer()` or as `pars` for `dust2::dust_system_create()`.
#'
#' @param scenario Scenario name, path to YAML, or parameter list. Defaults
#'   to `"historical"` — the scenario the monty fit pipeline was calibrated
#'   against.
#' @param fitted_names Names of parameters that the sampler will set; these
#'   are stripped from the fixed set. Defaults to [plague_fit_fitted_names()].
#' @return Plain named list of fixed parameters.
#' @export
plague_fit_fixed_pars <- function(scenario = "historical",
                                  fitted_names = plague_fit_fitted_names()) {
  pars <- as.list(load_scenario(scenario))
  keep <- intersect(setdiff(names(pars), fitted_names), plague_model_param_names())
  pars[keep]
}

#' Build a particle filter for the humans plague model.
#'
#' @param data Data frame with columns `time` and `deaths` (see `outbreaks`).
#' @param n_particles Number of particles in the filter.
#' @param n_threads Number of OpenMP threads for the filter (default 2).
#' @return A `dust2` particle filter ready for
#'   [dust2::dust_likelihood_run()] or [dust2::dust_likelihood_monty()].
#' @export
plague_fit_filter <- function(data, n_particles = 2000, n_threads = 2) {
  dust2::dust_filter_create(plague_stochastic_humans, time_start = 0,
                            data = data,
                            n_particles = n_particles, n_threads = n_threads)
}

#' Default prior over the fitted plague humans parameters (monty DSL).
#'
#' Paired with [plague_fit_fitted_names()] — order and variables must match.
#'
#' @return A monty model returned by `monty::monty_dsl()`.
#' @export
plague_fit_prior <- function() {
  ## Bounds chosen to comfortably span Eyam (~700 humans) through Cairo
  ## (~250k humans) and Black-Death-era CFRs (40-95%) through later
  ## better-recorded outbreaks (60-85%). beta_h spans Didelot's 1801
  ## posterior (0.0145, CI [0.005, 0.046]) up through Cairo-1835-scale
  ## hand-fits (~0.04). I_ini is in absolute counts; widen if K_r > 100k.
  monty::monty_dsl({
    beta_h ~ Uniform(0.001, 0.15)   # carcass-to-human transmission (per day)
    beta_r ~ Uniform(0.1, 3.0)      # carcass-to-rat transmission (per day)
    rho ~ Uniform(0.3, 6.0)         # carcass infectivity range
    g_h ~ Uniform(0.05, 0.5)        # human survival probability (CFR 50-95%)
    K_r ~ Uniform(500, 300000)      # rat carrying capacity
    K_h ~ Uniform(500, 300000)      # human population
    I_ini ~ Uniform(10, 50000)      # initial infected rats
    kappa ~ Gamma(shape = 2, rate = 0.05) # NegBin overdispersion (mode 20, mean 40, sd ~28)
  })
}

#' Default diagonal VCV for the random-walk sampler.
#'
#' Diagonal entries are ordered to match [plague_fit_fitted_names()].
#' Replace with an empirical covariance after a pilot run for better mixing
#' (the monty vignette walks through that tuning step).
#'
#' @return Numeric matrix.
#' @export
plague_fit_vcv <- function() {
  diag(c(0.01^2,    # beta_h  (wider — covers Didelot to Cairo scale)
         0.2^2,     # beta_r
         0.5^2,     # rho
         0.08^2,    # g_h
         5000^2,    # K_r     (wider — multi-city scale)
         5000^2,    # K_h
         500^2,     # I_ini   (wider — counts span 4 orders of magnitude)
         5^2))      # kappa   (Gamma prior mean 40, sd ~20)
}

#' Assemble everything needed to call [monty::monty_sample()] on a plague fit.
#'
#' Wires the filter, packer (with fixed parameters), likelihood, prior, and
#' sampler together. Runs the filter once at the fixed defaults as a smoke
#' test — if that throws, the parameters and data don't agree and you want
#' the error now rather than inside `monty_sample()`.
#'
#' @param data Data frame with columns `time` and `deaths`.
#' @param scenario Fixed-parameter scenario (name, path, or list). Default
#'   `"historical"`.
#' @param fitted_names Parameters for the sampler to vary. Default
#'   [plague_fit_fitted_names()].
#' @param n_particles Particle filter size. Default 2000.
#' @param n_threads OpenMP threads for the filter. Default 2.
#' @param prior monty model; default [plague_fit_prior()].
#' @param vcv Sampler VCV; default [plague_fit_vcv()].
#' @param obs_period Integer length (in days) of the observation window over
#'   which `D_h` accumulates between data points. Default `1L` for daily
#'   data. Set to e.g. `7L` when fitting weekly counts (London 1563); the
#'   model's `D_h` accumulator then resets every 7 days so each comparison
#'   is between a weekly count and a 7-day sum of simulated plague deaths.
#'   All `data$time` values must be multiples of `obs_period`.
#' @param smoke_test If `TRUE` (default), call `dust_likelihood_run()` once
#'   to validate filter + fixed params + data before returning.
#' @return Named list: `posterior`, `sampler`, `filter`, `packer`,
#'   `fixed_pars`, `fitted_names`. The caller chooses `runner`, `n_steps`,
#'   `n_chains`, `burnin` when invoking `monty_sample()`.
#' @export
plague_fit_setup <- function(data,
                             scenario = "historical",
                             fitted_names = plague_fit_fitted_names(),
                             n_particles = 2000,
                             n_threads = 2,
                             prior = NULL,
                             vcv = NULL,
                             obs_period = 1L,
                             smoke_test = TRUE) {
  if ("obs_period" %in% fitted_names) {
    cli::cli_abort(
      "obs_period is a fixed observation-model knob, not a fittable parameter. \\
       Pass it via the obs_period argument instead of including it in fitted_names."
    )
  }
  fixed_pars <- plague_fit_fixed_pars(scenario, fitted_names)
  validate_obs_period(obs_period,
                      tau = fixed_pars$tau %||% 1,
                      data_time = data$time)
  fixed_pars$obs_period <- obs_period
  # Default seasonal forcing to 1 (no seasonality) when neither the scenario
  # nor the caller supplied one. Length matches the data extent so the odin
  # model's seasonal[time + 1] indexing is in-range for every observation.
  if (is.null(fixed_pars$seasonal)) {
    fixed_pars$seasonal <- rep(1, max(data$time))
  }
  filter <- plague_fit_filter(data, n_particles = n_particles,
                              n_threads = n_threads)
  if (isTRUE(smoke_test)) {
    dust2::dust_likelihood_run(filter, fixed_pars)
  }
  packer <- monty::monty_packer(fitted_names, fixed = fixed_pars)
  likelihood <- dust2::dust_likelihood_monty(filter, packer,
                                             save_trajectories = FALSE)
  if (is.null(prior)) prior <- plague_fit_prior()
  if (is.null(vcv)) vcv <- plague_fit_vcv()
  posterior <- likelihood + prior
  sampler <- monty::monty_sampler_random_walk(vcv)
  list(posterior = posterior,
       sampler = sampler,
       filter = filter,
       packer = packer,
       fixed_pars = fixed_pars,
       fitted_names = fitted_names)
}
