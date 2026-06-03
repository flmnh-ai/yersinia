# ------------------------------------------------------------------------------
# diagnostics.R — automatic flag detectors for monty fits.
#
# Each detector accepts a `monty_samples` object (or any object posterior knows
# how to coerce via `posterior::as_draws_array()`) and returns NULL when nothing
# is wrong, or a single diagnostic record:
#
#   list(
#     detector      = "chain_stuck",   # programmatic ID, stable across runs
#     severity      = "alert",          # "info" | "warn" | "alert"
#     message       = "...",            # human-readable headline
#     suggested_fix = "...",            # actionable remediation
#     details       = list(...)         # optional per-param data for tables/plots
#   )
#
# The Shiny advisor module renders these records; tests assert the schema is
# stable. Three detectors are wired in for v1, matching the MVP carve-out:
# chain_stuck (R-hat > 1.5), bound_piling (>20% of mass within 5% of any prior
# bound), low_kappa (posterior median < 5).
# ------------------------------------------------------------------------------

# Internal: coerce an input to posterior::draws_array, accepting monty_samples
# directly as well as anything posterior already knows how to coerce.
.as_draws <- function(x) {
  if (inherits(x, "draws_array")) return(x)
  posterior::as_draws_array(x)
}

# Build a single diagnostic record. Internal — keeps the record schema in
# one place so detectors don't drift.
.diag_record <- function(detector, severity, message, suggested_fix,
                         details = NULL) {
  list(detector = detector, severity = severity, message = message,
       suggested_fix = suggested_fix, details = details)
}

#' Detect non-mixing chains via R-hat.
#'
#' Uses [posterior::rhat()] on each parameter; flags the fit if any parameter's
#' R-hat exceeds `threshold`. The MVP threshold of 1.5 is intentionally loose
#' (the conventional convergence cutoff is 1.05–1.1) — we want to flag pilots
#' that are obviously broken, not over-warn on borderline cases.
#'
#' @param samples A `monty_samples` object or a `posterior::draws_array`.
#' @param threshold R-hat above which the fit is flagged. Default 1.5.
#' @return A diagnostic record (see file header) or `NULL` if all R-hats are
#'   below `threshold`.
#' @export
diagnose_chain_stuck <- function(samples, threshold = 1.5) {
  draws <- .as_draws(samples)
  vars <- posterior::variables(draws)
  if (length(vars) == 0) return(NULL)
  # posterior::rhat() on a draws_array collapses to one scalar across all
  # variables; iterate per-variable for the per-parameter R-hat we want.
  rh <- vapply(vars, function(v) {
    posterior::rhat(posterior::extract_variable_matrix(draws, v))
  }, numeric(1))
  bad_idx <- which(is.na(rh) | rh > threshold)
  if (length(bad_idx) == 0) return(NULL)
  bad <- rh[bad_idx]
  .diag_record(
    detector = "chain_stuck",
    severity = "alert",
    message = sprintf("R-hat > %.2f for %d parameter%s: %s", threshold,
                      length(bad), if (length(bad) == 1) "" else "s",
                      paste(sprintf("%s (%.2f)", names(bad), bad),
                            collapse = ", ")),
    suggested_fix = paste(
      "One or more chains is stuck. Try: (1) increase warmup/iterations,",
      "(2) inflate the proposal VCV by 2-5x, (3) check for multimodality",
      "via mcmc_pairs(), or (4) loosen any tight priors that may be walling",
      "off between-mode excursions."),
    details = list(rhat = rh, bad_params = names(bad), threshold = threshold)
  )
}

#' Detect posterior mass piling against prior bounds.
#'
#' For each parameter with finite `[low, high]` bounds in `bounds`, computes
#' the fraction of draws within `edge` (a fraction of the bound width) of
#' either bound. Flags any parameter where that fraction exceeds `threshold`.
#' Bound-piling indicates the prior is constraining the posterior — either
#' the prior is too tight, or the model wants a parameter the prior forbids.
#'
#' Parameters with non-finite bounds (e.g. `Exponential(rate=0.1)` with
#' `high = Inf`) are silently skipped — there's no upper edge to pile against.
#'
#' @param samples A `monty_samples` object or a `posterior::draws_array`.
#' @param bounds Named list keyed by parameter name; each entry is a length-2
#'   numeric `c(low, high)`. Parameters not in `bounds` are skipped.
#' @param threshold Fraction of draws within `edge` of a bound that triggers
#'   the flag. Default 0.20 (20%).
#' @param edge Fraction of bound width counted as "near the edge". Default
#'   0.05 (5%).
#' @return A diagnostic record or `NULL`.
#' @export
diagnose_bound_piling <- function(samples, bounds,
                                  threshold = 0.20, edge = 0.05) {
  if (length(bounds) == 0) return(NULL)
  draws <- .as_draws(samples)
  par_names <- posterior::variables(draws)
  hits <- list()
  for (par in intersect(names(bounds), par_names)) {
    b <- bounds[[par]]
    if (length(b) != 2 || any(!is.finite(b)) || b[2] <= b[1]) next
    width <- b[2] - b[1]
    low_band  <- b[1] + edge * width
    high_band <- b[2] - edge * width
    x <- as.numeric(posterior::extract_variable(draws, par))
    n <- length(x)
    frac_low  <- sum(x <= low_band)  / n
    frac_high <- sum(x >= high_band) / n
    if (frac_low >= threshold || frac_high >= threshold) {
      side <- if (frac_low >= frac_high) "lower" else "upper"
      hits[[par]] <- list(
        side = side,
        frac_low = frac_low, frac_high = frac_high,
        bound = b
      )
    }
  }
  if (length(hits) == 0) return(NULL)
  desc <- vapply(names(hits), function(p) {
    h <- hits[[p]]
    f <- if (h$side == "lower") h$frac_low else h$frac_high
    sprintf("%s (%.0f%% near %s bound %.3g)",
            p, 100 * f, h$side,
            if (h$side == "lower") h$bound[1] else h$bound[2])
  }, character(1))
  .diag_record(
    detector = "bound_piling",
    severity = "warn",
    message = sprintf("Posterior piles against prior bound for %d parameter%s: %s",
                      length(hits), if (length(hits) == 1) "" else "s",
                      paste(desc, collapse = ", ")),
    suggested_fix = paste(
      "The prior is constraining the posterior. Either widen the prior bound",
      "(if the data wants to go further), or treat the bound as a meaningful",
      "biological constraint and pin the parameter explicitly. Tightening the",
      "prior to match the pile-up is rarely correct — it papers over a",
      "data-prior conflict."),
    details = list(hits = hits, threshold = threshold, edge = edge)
  )
}

#' Detect low NegBinomial dispersion (`kappa`).
#'
#' The plague-fit likelihood is `deaths ~ NegBinomial(size = kappa, mu = ...)`.
#' Small `kappa` means highly overdispersed observations — the data is so
#' noisy that almost any trajectory is plausible, so the posterior over
#' transmission parameters becomes uninformative. The MVP threshold of 5 is
#' a rule of thumb from the Cairo / Barcelona pilots in this conversation:
#' kappa < 5 typically corresponded to broken or under-specified fits.
#'
#' Uses the posterior median of `kappa` across all chains and iterations.
#'
#' @param samples A `monty_samples` object or a `posterior::draws_array`.
#' @param par Name of the dispersion parameter. Default `"kappa"`.
#' @param threshold Median `kappa` below which the fit is flagged. Default 5.
#' @return A diagnostic record or `NULL` (also `NULL` if `par` isn't in the
#'   sampled parameters — kappa may be pinned in some configurations).
#' @export
diagnose_low_kappa <- function(samples, par = "kappa", threshold = 5) {
  draws <- .as_draws(samples)
  if (!par %in% posterior::variables(draws)) return(NULL)
  x <- as.numeric(posterior::extract_variable(draws, par))
  med <- median(x)
  if (!is.finite(med) || med >= threshold) return(NULL)
  .diag_record(
    detector = "low_kappa",
    severity = "warn",
    message = sprintf("Posterior median %s = %.2f < %.1f — observations are highly overdispersed",
                      par, med, threshold),
    suggested_fix = paste(
      "Low dispersion means the likelihood barely constrains the trajectory,",
      "so transmission parameters become weakly identified. Check (1) whether",
      "the death series has long zero-runs or large outliers driving the",
      "dispersion, (2) whether the model's mean trajectory actually tracks",
      "the data shape, and (3) whether `lambda_baseline` is absorbing what",
      "should be Poisson-Negative-Binomial transition behaviour."),
    details = list(median = med, par = par, threshold = threshold)
  )
}

#' Run all v1 diagnostics on a fit.
#'
#' Bundles [diagnose_chain_stuck()], [diagnose_bound_piling()], and
#' [diagnose_low_kappa()]. Returns a list of records (length 0 to 3); pass
#' `bounds = NULL` to skip bound-piling.
#'
#' @param samples A `monty_samples` object or a `posterior::draws_array`.
#' @param bounds Optional named list of `c(low, high)` bounds for each
#'   fitted parameter. Pass `NULL` to skip bound-piling detection.
#' @param rhat_threshold Passed to [diagnose_chain_stuck()].
#' @param bound_threshold Passed to [diagnose_bound_piling()] as `threshold`.
#' @param bound_edge Passed to [diagnose_bound_piling()] as `edge`.
#' @param kappa_par Passed to [diagnose_low_kappa()] as `par`.
#' @param kappa_threshold Passed to [diagnose_low_kappa()] as `threshold`.
#' @return List of diagnostic records (possibly empty).
#' @export
run_diagnostics <- function(samples, bounds = NULL,
                            rhat_threshold = 1.5,
                            bound_threshold = 0.20, bound_edge = 0.05,
                            kappa_par = "kappa", kappa_threshold = 5) {
  records <- list(
    diagnose_chain_stuck(samples, threshold = rhat_threshold),
    if (!is.null(bounds)) diagnose_bound_piling(samples, bounds,
                                                threshold = bound_threshold,
                                                edge = bound_edge),
    diagnose_low_kappa(samples, par = kappa_par, threshold = kappa_threshold)
  )
  Filter(Negate(is.null), records)
}
