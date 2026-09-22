#' Temperature-dependent multiplier for delta_R
#' (rate of loss of carcass infectiousness in a Didelot-style plague model)
#'
#' Returns a Brière-shaped multiplier capturing the combined temperature
#' effects on flea survival and Y. pestis biofilm blockage formation.
#' Multiply your fitted baseline delta_R rate by this output to obtain
#' delta_R(T) for use in the model:
#'
#'     delta_R(T) = delta * delta_R_multiplier(T)
#'
#' Functional form:
#'     L(T) = (T - T_min) * (T_max - T)^q   for T_min < T < T_max
#'     multiplier(T) = L(T_ref) / L(T)
#'
#' Defaults place the optimum (longest infectious period) at 17.8 C with
#' hard bounds at 5 C (cold cliff: Y. pestis blockage formation fails)
#' and 37 C (warm cliff: flea thermal death + biofilm dissolution).
#' These are anchored to: Krauer et al. 2021 (epidemic peak ~17 C),
#' Hinnebusch lab biofilm work (cold cliff), and Mellanby 1932 (thermal
#' death point of X. cheopis).
#'
#' Default T_ref = 17.8 means the fitted `delta` represents delta_R at the
#' thermal optimum, i.e. the *minimum* loss rate (longest carcass
#' infectiousness). All multipliers are then >= 1. To match Didelot et al.
#' 2017's calibration anchor instead, set T_ref = 14 — `delta` then
#' represents delta_R at Cairo winter mean temperature, comparable to
#' their posterior median of 0.267/day.
#'
#' @param temperature Numeric vector of temperatures (degrees Celsius)
#' @param T_min Lower thermal bound (default 5 C)
#' @param T_max Upper thermal bound (default 37 C)
#' @param q Warm-side asymmetry exponent (default 1.5)
#' @param T_ref Reference temperature where multiplier = 1 (default 17.8 C)
#' @param cap Maximum multiplier value, applied beyond thermal bounds
#'   (default 1000) to keep the likelihood numerically well-behaved
#' @return Numeric vector of multipliers, one per input temperature
delta_R_multiplier <- function(temperature,
                               T_min = 5,
                               T_max = 37,
                               q     = 1.5,
                               T_ref = 17.8,
                               cap   = 1000) {

  # Sanity checks on shape parameters (cheap; runs once per call)
  if (T_ref <= T_min || T_ref >= T_max) {
    stop("T_ref must lie strictly between T_min and T_max.")
  }

  # Reference value: L evaluated at T_ref
  L_ref <- (T_ref - T_min) * (T_max - T_ref)^q

  # Vectorised L(T), with NA outside the support
  in_range <- temperature > T_min & temperature < T_max
  L_T <- ifelse(in_range,
                (temperature - T_min) * (T_max - temperature)^q,
                NA_real_)

  # Multiplier, with cap applied beyond thermal bounds and on extreme values
  mult <- L_ref / L_T
  mult <- pmin(ifelse(is.na(mult), cap, mult), cap)

  return(mult)
}


# ==============================================================================
# Thermal response on TRANSMISSION (added 2026-09)
#
# These replace `delta_R_multiplier()` for the beta-forcing formulation. Three
# differences that matter:
#
#   1. They return a multiplier on beta_r / beta_h, not on delta_R. Carcass
#      lifetime stays fixed, so the thermal parameters no longer control the
#      epizootic's timescale as well as its intensity.
#   2. They are anchored at their own maximum, so w <= 1 everywhere and w = 1
#      at the optimum. beta_r / beta_h are then "transmission at the thermal
#      optimum" and R0 is peak R0 -- no reference-temperature bookkeeping, and
#      no drift in meaning as the shape parameters move.
#   3. `thermal_response()` is parameterised by what the data can see -- where
#      the curve peaks and how far you go either side before it halves --
#      rather than by the temperatures at which it hits zero, which for this
#      dataset are pure extrapolation (see docs/identifiability-audit.md).
# ==============================================================================

#' Thermal response on transmission, in visible coordinates
#'
#' Asymmetric Gaussian in log space, anchored so `max(w) = 1` at `T_opt`.
#'
#' @param temperature Numeric vector of temperatures (degrees Celsius).
#' @param T_opt Temperature at which transmission peaks.
#' @param hw_cold Degrees BELOW `T_opt` at which the response halves.
#' @param hw_hot Degrees ABOVE `T_opt` at which the response halves.
#' @param floor Smallest value returned; keeps the likelihood finite when a
#'   proposal pushes the response to numerical zero. Default 1e-8.
#' @return Numeric vector in `(0, 1]`, one per input temperature.
#' @export
thermal_response <- function(temperature, T_opt, hw_cold, hw_hot,
                             floor = 1e-8) {
  if (!is.finite(T_opt) || hw_cold <= 0 || hw_hot <= 0) {
    return(rep(floor, length(temperature)))
  }
  # half-width -> Gaussian sd
  k <- sqrt(2 * log(2))
  sd <- ifelse(temperature < T_opt, hw_cold / k, hw_hot / k)
  pmax(exp(-0.5 * ((temperature - T_opt) / sd)^2), floor)
}

#' Thermal response on transmission, Briere form
#'
#' Same anchoring (`max(w) = 1`), but in the familiar Briere coordinates, for
#' model comparison against [thermal_response()]. Note that `T_max` is not
#' identifiable from the mortality data in this repository -- no outbreak
#' reaches 29 C. Treat it as an assumption, not an estimate.
#'
#' @param temperature Numeric vector of temperatures (degrees Celsius).
#' @param T_min,T_max Lower and upper temperatures at which transmission is zero.
#' @param q Warm-side asymmetry exponent.
#' @param floor Smallest value returned. Default 1e-8.
#' @return Numeric vector in `(0, 1]`, one per input temperature.
#' @export
thermal_response_briere <- function(temperature, T_min, T_max, q,
                                    floor = 1e-8) {
  if (!is.finite(T_min) || !is.finite(T_max) || T_max <= T_min || q <= 0) {
    return(rep(floor, length(temperature)))
  }
  L <- function(x) ifelse(x > T_min & x < T_max, (x - T_min) * (T_max - x)^q, 0)
  # Analytic peak: dL/dT = 0 at (T_max + q T_min) / (1 + q)
  peak <- L((T_max + q * T_min) / (1 + q))
  if (!is.finite(peak) || peak <= 0) return(rep(floor, length(temperature)))
  pmax(L(temperature) / peak, floor)
}

#' Temperature at which a Briere response peaks
#'
#' Convenience for converting Briere coordinates to the optimum, e.g. when
#' summarising posterior draws.
#'
#' @param T_min,T_max,q Briere parameters.
#' @return The optimum temperature.
#' @export
briere_T_opt <- function(T_min, T_max, q) (T_max + q * T_min) / (1 + q)
