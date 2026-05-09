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
