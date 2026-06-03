# ------------------------------------------------------------------------------
# packer_helpers.R — composable wrappers around monty::monty_packer_grouped().
#
# A grouped packer's $unpack(theta) returns a nested list keyed by group, where
# each entry is the full per-group parameter list (scalars + per-group locals +
# global fixed). dust2's grouped filter consumes that nested list directly.
#
# These wrappers each modify the unpack closure to apply one transformation per
# group: splice in per-group fixed pars, convert R0 to beta_r, modulate seasonal
# forcing by alpha, or compute seasonal from temperature via Brière. They are
# composable via |>, applied in declaration order. The order matters for
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

#' Modulate per-group seasonal forcing by a fitted alpha exponent.
#'
#' Replaces each group's `seasonal` vector with `group_seasonal[[g]] ^ alpha`.
#' Lets the data inform the strength of seasonality — `alpha = 0` collapses
#' seasonal to flat 1, `alpha = 1` is the raw climate signal, intermediate
#' values dampen, larger values amplify. Removes `alpha` from unpacked pars.
#'
#' @param packer A grouped packer or wrapper around one. Must have `alpha` as
#'   a shared scalar parameter.
#' @param group_seasonal Named list keyed by group; each entry is a numeric
#'   vector of per-day seasonal multipliers (length = number of simulation
#'   days for that outbreak).
#' @return A packer with wrapped `$unpack`.
#' @export
with_alpha_seasonal <- function(packer, group_seasonal) {
  out <- packer
  inner_unpack <- packer$unpack
  out$unpack <- function(x) {
    u <- inner_unpack(x)
    Map(function(g, pars) {
      pars$seasonal <- group_seasonal[[g]] ^ pars$alpha
      pars$alpha <- NULL
      pars
    }, names(u), u)
  }
  class(out) <- class(packer)
  out
}

#' Compute per-group seasonal forcing from temperature via Brière.
#'
#' For each group, transforms the temperature time series into a per-day
#' multiplier on `delta_R` (carcass decay) using the Brière function
#' parameterised by fitted `T_min`, `T_max`, `q_briere`. The multiplier is
#' normalised so `delta_R` equals its scenario value at `T_ref` (default
#' 17.8°C, the typical mid-range plague-permissive temperature) — i.e.
#' calibrating outside the Brière transformation reaches the unmodulated
#' scenario `delta_R` at the reference temperature.
#'
#' If `T_ref` falls outside `[T_min, T_max]` (sampler exploring biologically
#' implausible cliffs), returns `cap` for all temperatures, which collapses
#' transmission and signals the proposal is bad. Per-temperature points
#' outside `[T_min, T_max]` are also capped. Removes `T_min`, `T_max`,
#' `q_briere` from unpacked pars.
#'
#' @param packer A grouped packer or wrapper around one. Must have `T_min`,
#'   `T_max`, `q_briere` as shared scalar parameters.
#' @param group_temp Named list keyed by group; each entry is a numeric
#'   per-day temperature vector (°C, length = number of simulation days).
#' @param T_ref Reference temperature at which the Brière multiplier equals
#'   1 (i.e. the scenario `delta_R` is unchanged). Default 17.8°C.
#' @param cap Maximum allowed multiplier — clamps the transmission-off
#'   signal when params are out of range. Default 1000.
#' @return A packer with wrapped `$unpack`.
#' @export
with_briere_seasonal <- function(packer, group_temp,
                                 T_ref = 17.8, cap = 1000) {
  out <- packer
  inner_unpack <- packer$unpack
  out$unpack <- function(x) {
    delta_R_briere <- function(temperature, T_min, T_max, q) {
      if (T_ref <= T_min || T_ref >= T_max) {
        return(rep(cap, length(temperature)))
      }
      L_ref <- (T_ref - T_min) * (T_max - T_ref)^q
      in_range <- temperature > T_min & temperature < T_max
      L_T <- ifelse(in_range,
                    (temperature - T_min) * (T_max - temperature)^q,
                    NA_real_)
      mult <- L_ref / L_T
      pmin(ifelse(is.na(mult), cap, mult), cap)
    }
    u <- inner_unpack(x)
    Map(function(g, pars) {
      pars$seasonal <- delta_R_briere(
        group_temp[[g]],
        T_min = pars$T_min,
        T_max = pars$T_max,
        q     = pars$q_briere
      )
      pars$T_min    <- NULL
      pars$T_max    <- NULL
      pars$q_briere <- NULL
      pars
    }, names(u), u)
  }
  class(out) <- class(packer)
  out
}
