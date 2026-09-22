# ------------------------------------------------------------------------------
# packer_helpers.R — composable wrappers around monty::monty_packer_grouped().
#
# A grouped packer's $unpack(theta) returns a nested list keyed by group, where
# each entry is the full per-group parameter list (scalars + per-group locals +
# global fixed). dust2's grouped filter consumes that nested list directly.
#
# These wrappers each modify the unpack closure to apply one transformation per
# group: splice in per-group fixed pars, convert R0 to beta_r, or compute the
# thermal multiplier on transmission from temperature. They are composable via
# |>, applied in declaration order. The order matters for
# transformations that consume what an earlier wrapper injects (e.g.
# with_R0_to_beta_r needs g_r/rho/delta_R already present).
#
# IMPORTANT: helper functions called by each wrapper's unpack closure are
# defined *inside* the wrapper, not in the package namespace. monty_runner_callr
# serialises closures with saveRDS to worker subprocesses; the closure carries
# its enclosing environment but NOT the package namespace. A helper defined at
# the package level would be invisible to the worker. See CLAUDE.md
# "monty_packer + monty_runner_callr gotcha".
# ------------------------------------------------------------------------------

#' Splice per-group fixed parameters into a grouped packer.
#'
#' For values that are *fixed but vary per group* (e.g. each outbreak's K_h,
#' K_r pinned to its Krauer-listed population), [monty::monty_packer_grouped()]'s
#' `fixed` argument doesn't help — it's global. This wrapper merges
#' `group_fixed[[g]]` into each group's pars during unpack.
#'
#' @param packer A grouped packer (from [monty::monty_packer_grouped()]) or a
#'   wrapper around one.
#' @param group_fixed Named list keyed by group; each entry is a named list of
#'   parameters to splice into that group's unpacked pars. Groups not in
#'   `group_fixed` are passed through unchanged.
#' @return A packer with wrapped `$unpack`. Class preserved so dust2's grouped
#'   filter accepts it.
#' @export
with_per_group_fixed <- function(packer, group_fixed) {
  out <- packer
  inner_unpack <- packer$unpack
  out$unpack <- function(x) {
    u <- inner_unpack(x)
    Map(function(g, gp) c(gp, group_fixed[[g]]), names(u), u)
  }
  class(out) <- class(packer)
  out
}

#' Convert R0 to beta_r per group during unpack.
#'
#' Sampling R0 directly (rather than beta_r) gives a more interpretable prior
#' and a flatter posterior geometry. The conversion uses the disease-free
#' equilibrium derivation in the carcass model (see CLAUDE.md "R0 formula"):
#' `beta_r = R0 * delta_R / ((1 - g_r) * (1 - exp(-rho)))`.
#'
#' Requires `g_r`, `rho`, `delta_R` to already be present in each group's pars
#' (typically via the packer's global `fixed` argument). Removes `R0` from the
#' unpacked pars after conversion.
#'
#' @param packer A grouped packer or wrapper around one. Must have `R0` as a
#'   shared scalar parameter.
#' @return A packer with wrapped `$unpack`.
#' @export
with_R0_to_beta_r <- function(packer) {
  out <- packer
  inner_unpack <- packer$unpack
  out$unpack <- function(x) {
    R0_to_beta_r <- function(R0, g_r, rho, delta_R) {
      R0 * delta_R / ((1 - g_r) * (1 - exp(-rho)))
    }
    u <- inner_unpack(x)
    lapply(u, function(pars) {
      pars$beta_r <- R0_to_beta_r(pars$R0, pars$g_r, pars$rho, pars$delta_R)
      pars$R0 <- NULL
      pars
    })
  }
  class(out) <- class(packer)
  out
}

#' Compute per-group thermal forcing on transmission.
#'
#' For each group, turns that group's daily temperature series into a per-day
#' multiplier on `beta_r` and `beta_h` via the odin model's `seasonal_beta`
#' input, using [thermal_response()] with fitted `T_opt`, `hw_cold`, `hw_hot`.
#' Carcass decay is left unforced: `seasonal` is set to all ones if the packer
#' has not already supplied it.
#'
#' Bad proposals (non-positive widths) yield a floored response rather than
#' an error, which drives the likelihood down and pushes the sampler away.
#'
#' @param packer A grouped packer or wrapper around one. Must carry `T_opt`,
#'   `hw_cold`, `hw_hot` as parameters.
#' @param group_temp Named list keyed by group; each entry is a numeric per-day
#'   temperature vector (degrees C, length = number of simulation days).
#' @return A packer with wrapped `$unpack`.
#' @export
with_thermal_beta <- function(packer, group_temp) {
  out <- packer
  inner_unpack <- packer$unpack
  # Captured by value so the closure carries it to callr workers (see header).
  thermal <- thermal_response
  out$unpack <- function(x) {
    u <- inner_unpack(x)
    Map(function(g, pars) {
      pars$seasonal_beta <- thermal(
        group_temp[[g]],
        T_opt   = pars$T_opt,
        hw_cold = pars$hw_cold,
        hw_hot  = pars$hw_hot)
      pars$T_opt <- NULL; pars$hw_cold <- NULL; pars$hw_hot <- NULL
      # Exact [[ ]]: `pars$seasonal` partial-matches `seasonal_beta`, which
      # was just set above, so the guard would never fire.
      if (is.null(pars[["seasonal"]])) {
        pars[["seasonal"]] <- rep(1, length(pars[["seasonal_beta"]]))
      }
      pars
    }, names(u), u)
  }
  class(out) <- class(packer)
  out
}
