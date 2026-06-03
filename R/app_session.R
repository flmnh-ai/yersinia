# ------------------------------------------------------------------------------
# app_session.R — LabSession R6 class.
#
# Holds the canonical state for a single Virtual Lab Shiny session: which
# outbreaks are in the cohort, how the model is configured, what the priors
# look like, the current fit object, the sandbox draw. State is backed by
# `shiny::reactiveVal()` so the Shiny UI observes changes; outside the app
# (tests, scripts) reads still work — they just don't subscribe to anything.
#
# Each subsequent module (priors, fit, sandbox) extends this class with one
# more field. Save/load goes through `to_list()` and [lab_session_from_list()]
# so RDS round-trips drop the reactive infrastructure and carry only values.
# ------------------------------------------------------------------------------

#' LabSession R6 class — Virtual Lab session state.
#'
#' Holds the reactive state of an interactive plague-fitting session. Used by
#' [lab_app()] and the Shiny modules. State is exposed as active bindings
#' backed by `shiny::reactiveVal()` so reads from inside reactive contexts
#' subscribe automatically; outside Shiny, reads return the current value.
#'
#' Current fields: `cohort_ids` (character vector of selected `outbreak_id`s
#' from the [outbreaks] dataset), `model_config` (list with `scenario`,
#' `shared`, `local` — see [model_config_default()]). Future versions will
#' add `priors`, `fit_state`, `sandbox_draw` as the corresponding modules
#' come online.
#'
#' Use [lab_session_from_list()] to reconstruct from a saved state list.
#'
#' @export
LabSession <- R6::R6Class(
  "LabSession",
  private = list(
    .cohort_ids   = NULL,  # reactiveVal once initialize() runs
    .model_config = NULL,
    .priors       = NULL,
    .fit_state    = NULL
  ),
  active = list(
    #' @field cohort_ids Character vector of selected `outbreak_id`s.
    #'   Reading from inside a reactive context subscribes; assigning
    #'   triggers downstream observers.
    cohort_ids = function(value) {
      if (missing(value)) private$.cohort_ids()
      else private$.cohort_ids(value)
    },

    #' @field model_config Model-config list (`scenario`, `shared`, `local`)
    #'   — see [model_config_default()] for the schema.
    model_config = function(value) {
      if (missing(value)) private$.model_config()
      else private$.model_config(value)
    },

    #' @field priors Named list of priors keyed by parameter name; each
    #'   entry is `list(family, params)`. See [priors_default()].
    priors = function(value) {
      if (missing(value)) private$.priors()
      else private$.priors(value)
    },

    #' @field fit_state Named list summarising the most recent fit:
    #'   `status` (`"idle" | "running" | "complete" | "error"`),
    #'   `samples` (a `monty_samples` object or `NULL`),
    #'   `n_chains`, `n_iter`, `duration` (seconds), `error` (message),
    #'   `started_at`, `completed_at`.
    fit_state = function(value) {
      if (missing(value)) private$.fit_state()
      else private$.fit_state(value)
    }
  ),
  public = list(
    #' @description Construct a new LabSession.
    #' @param cohort_ids Initial cohort (character vector of `outbreak_id`s).
    #'   Default empty.
    #' @param model_config Initial model config; defaults to
    #'   [model_config_default()].
    #' @param priors Named list of priors. Defaults to [priors_default()]
    #'   over the fitted parameters implied by `model_config`.
    #' @param fit_state Initial fit-state struct; defaults to an idle state.
    initialize = function(cohort_ids = character(0),
                          model_config = NULL,
                          priors = NULL,
                          fit_state = NULL) {
      if (is.null(model_config)) model_config <- model_config_default()
      if (is.null(priors)) {
        fitted <- union(model_config$shared, model_config$local)
        priors <- priors_default(fitted)
      }
      if (is.null(fit_state)) {
        fit_state <- list(status = "idle", samples = NULL, n_chains = NULL,
                          n_iter = NULL, duration = NULL, error = NULL,
                          started_at = NULL, completed_at = NULL)
      }
      private$.cohort_ids   <- shiny::reactiveVal(cohort_ids)
      private$.model_config <- shiny::reactiveVal(model_config)
      private$.priors       <- shiny::reactiveVal(priors)
      private$.fit_state    <- shiny::reactiveVal(fit_state)
      invisible(self)
    },

    #' @description Snapshot the session state as a plain list (no reactive
    #'   infrastructure), suitable for `saveRDS()`.
    to_list = function() {
      shiny::isolate({
        list(
          cohort_ids   = private$.cohort_ids(),
          model_config = private$.model_config(),
          priors       = private$.priors(),
          fit_state    = private$.fit_state()
        )
      })
    },

    #' @description Mutate this LabSession in place from a saved-state list
    #'   (e.g. one produced by `to_list()`). All reactiveVals fire, so any
    #'   subscribed Shiny observers update. Unknown keys are ignored so
    #'   older snapshots remain loadable.
    #' @param state Named list as returned by `to_list()`.
    apply_list = function(state) {
      if (!is.null(state$cohort_ids))   private$.cohort_ids(state$cohort_ids)
      if (!is.null(state$model_config)) private$.model_config(state$model_config)
      if (!is.null(state$priors))       private$.priors(state$priors)
      if (!is.null(state$fit_state))    private$.fit_state(state$fit_state)
      invisible(self)
    }
  )
)

#' Reconstruct a LabSession from a saved state list.
#'
#' Inverse of `LabSession$to_list()`. Unknown keys in `state` are ignored so
#' older snapshots remain loadable as the schema grows; missing keys fall
#' back to the same defaults [LabSession]'s constructor uses.
#'
#' @param state Named list as returned by `LabSession$to_list()`.
#' @return A new [LabSession] instance.
#' @export
lab_session_from_list <- function(state) {
  LabSession$new(
    cohort_ids   = state$cohort_ids   %||% character(0),
    model_config = state$model_config %||% NULL,
    priors       = state$priors       %||% NULL,
    fit_state    = state$fit_state    %||% NULL
  )
}

# Local null-coalescing so we don't pull in rlang for one operator.
`%||%` <- function(a, b) if (is.null(a)) b else a
