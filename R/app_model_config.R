# ------------------------------------------------------------------------------
# app_model_config.R — model-configuration helpers for the Virtual Lab.
#
# Pure helpers used by the model_config module + the lab_session defaults.
# A model config is a small list:
#
#   list(
#     scenario = "historical",      # base scenario name (load_scenario())
#     shared   = c("beta_r", ...),  # parameters fitted with one value across cohort
#     local    = c("beta_h", ...)   # parameters fitted per-outbreak
#   )
#
# Anything not in `shared` or `local` is pinned at the scenario value. If the
# same parameter appears in both lists, `local` wins — see model_config_resolve().
# ------------------------------------------------------------------------------

#' Names of model parameters configurable from the Virtual Lab UI.
#'
#' Excludes parameters that aren't user-fittable in practice:
#'
#' - `tau` (timestep — system, not biological)
#' - `seasonal` (vector — handled separately as climate input)
#' - `obs_period` (integer flag — derived from the cohort data)
#' - `iota` (resistance fecundity multiplier — always-fixed per CLAUDE.md)
#'
#' The returned set is ~23 scalar parameters: rates, capacities, initial
#' counts, and the likelihood dispersion `kappa`.
#'
#' @return Character vector of parameter names.
#' @export
configurable_param_names <- function() {
  setdiff(plague_model_param_names(),
          c("tau", "seasonal", "obs_period", "iota"))
}

#' Names of bundled scenario YAML files.
#'
#' Mirrors the canonical list in `load_named_scenario()`. v1 hardcodes the
#' five built-in scenarios; future versions will scan
#' `system.file("scenarios", package = "yersinia")` so user-bundled YAMLs
#' show up automatically.
#'
#' @return Character vector of scenario names.
#' @export
available_scenarios <- function() {
  c("defaults", "didelot", "historical", "keeling-gilligan",
    "modern-estimates")
}

#' Default model configuration for a fresh Virtual Lab session.
#'
#' Mirrors the Barcelona / Cairo vignette fitting workflows: biology +
#' likelihood-overdispersion are shared across the cohort; transmission to
#' humans and ignition + record-quality knobs are local (per outbreak).
#' Populations (`K_h`, `K_r`) are *not* in the fitted set — when omitted,
#' [lab_fit_assemble()] pins them per-group at each outbreak's known
#' `population` from the [outbreaks] dataset. Same applies to `g_h` — the
#' vignettes pin it at the scenario value because it's only weakly
#' identifiable from death counts alone.
#'
#' `rho` (flea redistribution exponent) is also intentionally *not* fitted
#' by default: it's confounded with `beta_r` via the rat-side R₀ formula
#' `R₀ ∝ beta_r · (1 − e^(−rho)) / delta_R`, so jointly fitting both
#' creates a ridge in the posterior that wrecks rhat. Pin rho at the
#' scenario value and let beta_r carry the rat-side identification.
#'
#' To fit K_h/K_r, g_h, or rho, add them via the Model tab.
#'
#' @param scenario Base scenario name. Default `"defaults"` — the curated
#'   baseline scenario designed for out-of-the-box outbreak fitting. Era-
#'   specific scenarios (`"historical"`, `"modern-estimates"`,
#'   `"didelot"`) are available for paper reproduction.
#' @return A model-config list.
#' @export
model_config_default <- function(scenario = "defaults") {
  list(
    scenario = scenario,
    shared   = c("beta_r", "kappa"),
    local    = c("beta_h", "I_ini", "lambda_baseline")
  )
}

#' Resolve a model config to per-parameter scope and value.
#'
#' Loads the named scenario, then for every configurable parameter reports
#' the scenario value, the chosen scope (`fixed | shared | local`), and the
#' value the model would actually see (`scenario_value` for `fixed`, `NA`
#' for `shared`/`local` since the sampler will set those).
#'
#' Parameters that appear in both `shared` and `local` are reported as
#' `local` — the multi-cohort case usually wants per-outbreak resolution
#' when the user has flagged conflict.
#'
#' @param config A model-config list (see file header).
#' @return A tibble with columns `parameter`, `scope`, `scenario_value`,
#'   `resolved_value`, sorted alphabetically.
#' @export
model_config_resolve <- function(config) {
  scenario_pars <- as.list(load_scenario(config$scenario))
  pars <- sort(configurable_param_names())

  scenario_value <- unname(vapply(pars, function(p) {
    v <- scenario_pars[[p]]
    if (is.null(v) || length(v) != 1L) NA_real_ else as.numeric(v)
  }, numeric(1)))

  scope <- unname(vapply(pars, function(p) {
    if (p %in% config$local) "local"
    else if (p %in% config$shared) "shared"
    else "fixed"
  }, character(1)))

  resolved_value <- ifelse(scope == "fixed", scenario_value, NA_real_)

  tibble::tibble(
    parameter = pars,
    scope = scope,
    scenario_value = scenario_value,
    resolved_value = resolved_value
  )
}
