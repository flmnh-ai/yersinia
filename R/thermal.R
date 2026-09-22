# ==============================================================================
# Thermal response on transmission
#
# One curve: Brière written in "visible coordinates". It multiplies beta_r and
# beta_h through the odin model's `seasonal_beta` input; carcass decay
# (delta_R) is not temperature-forced.
#
# Why this parameterisation. The textbook simplified Brière-2,
#
#     w(T) = a * (T - T_min) * (T_max - T)^q,
#
# has its peak wherever the algebra puts it. Here the peak is a parameter and
# the widths are distances from it to the zeros:
#
#     T_min = T_opt - hw_cold,   T_max = T_opt + hw_hot,   q = hw_hot / hw_cold,
#
# with `a` fixed so that w(T_opt) = 1. It is the same family of curves, one to
# one, but the mortality data pin down T_opt far better than the zeros, and in
# these coordinates that is one axis rather than a curved ridge. Anchoring at
# 1 also makes beta_r / beta_h mean "transmission at the thermal optimum".
#
# The "simplified" form (no leading T factor) is deliberate: that factor
# depends on where the temperature scale puts zero and would force w = 0 at
# 0 C, while the fitted cold zero is around -4.7 C.
#
# The zeros here are hard. The Stan fits smooth them with softplus shoulders
# because HMC needs a differentiable density; that is a sampler concern and
# stays in the Stan code.
# ==============================================================================

#' Thermal response on transmission (Brière, visible coordinates)
#'
#' Returns a per-temperature multiplier on transmission, equal to 1 at
#' `T_opt` and falling to zero at `T_opt - hw_cold` and `T_opt + hw_hot`.
#' This is simplified Brière-2 reparameterised so the peak and the distances
#' to the two zeros are the parameters:
#'
#' \deqn{w(x) = (1 + x / h_c) (1 - x / h_h)^{h_h / h_c}, \quad x = T - T_{opt}}
#'
#' Equivalent textbook coordinates: `T_min = T_opt - hw_cold`,
#' `T_max = T_opt + hw_hot`, exponent `q = hw_hot / hw_cold`.
#'
#' @param temperature Numeric vector of temperatures (degrees Celsius).
#' @param T_opt Temperature at which transmission peaks.
#' @param hw_cold Degrees from `T_opt` down to where transmission reaches zero.
#' @param hw_hot Degrees from `T_opt` up to where transmission reaches zero.
#' @param floor Smallest value returned, applied outside the zeros and for
#'   degenerate parameters, so a bad MCMC proposal lowers the likelihood
#'   instead of producing zero transmission or an error. Default 1e-8.
#' @return Numeric vector in `[floor, 1]`, one per input temperature.
#' @export
thermal_response <- function(temperature, T_opt, hw_cold, hw_hot,
                             floor = 1e-8) {
  if (!is.finite(T_opt) || !is.finite(hw_cold) || !is.finite(hw_hot) ||
      hw_cold <= 0 || hw_hot <= 0) {
    return(rep(floor, length(temperature)))
  }
  x <- temperature - T_opt
  inside <- x > -hw_cold & x < hw_hot
  w <- rep(0, length(x))
  # In logs: the exponent hw_hot / hw_cold can be large, and taking the power
  # directly underflows near the hot zero.
  w[inside] <- exp(log1p(x[inside] / hw_cold) +
                   (hw_hot / hw_cold) * log1p(-x[inside] / hw_hot))
  pmax(w, floor)
}
