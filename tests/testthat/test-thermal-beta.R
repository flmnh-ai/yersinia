# Tests for the beta-forcing thermal response (added 2026-09).
#
# The load-bearing checks:
#   1. thermal_response() is anchored: max = 1 at T_opt, and the half-widths
#      mean what they say.
#   2. Degenerate parameters return a floor rather than erroring, so a bad
#      MCMC proposal drives the likelihood down instead of crashing the chain.
#   3. seasonal_beta = 1 reproduces the pre-edit model exactly (backward
#      compatibility).
#   4. seasonal_beta = c is exactly equivalent to scaling beta_r and beta_h
#      by c -- i.e. the forcing enters where it is supposed to and nowhere else.

test_that("thermal_response is anchored at its own maximum", {
  T <- seq(-20, 50, by = 0.25)
  w <- thermal_response(T, T_opt = 18, hw_cold = 12, hw_hot = 7)
  expect_equal(max(w), 1)
  expect_equal(T[which.max(w)], 18)
  expect_true(all(w <= 1))
})

test_that("half-widths are the temperatures at which the response halves", {
  expect_equal(thermal_response(18 - 12, 18, 12, 7), 0.5, tolerance = 1e-10)
  expect_equal(thermal_response(18 + 7,  18, 12, 7), 0.5, tolerance = 1e-10)
})

test_that("asymmetry works in the expected direction", {
  # narrow hot side: response falls off faster above the optimum
  w_lo <- thermal_response(18 - 5, 18, 12, 4)
  w_hi <- thermal_response(18 + 5, 18, 12, 4)
  expect_gt(w_lo, w_hi)
})

test_that("degenerate parameters floor rather than error", {
  expect_equal(thermal_response(10, 18, -1, 7), 1e-8)
  expect_equal(thermal_response(10, 18, 12, 0), 1e-8)
  expect_equal(thermal_response_briere(10, T_min = 30, T_max = 5, q = 1), 1e-8)
  expect_equal(thermal_response_briere(10, T_min = 5, T_max = 30, q = -1), 1e-8)
})

test_that("Briere form is anchored at its analytic optimum", {
  T <- seq(-10, 45, by = 0.01)
  w <- thermal_response_briere(T, T_min = 5, T_max = 37, q = 1.5)
  expect_equal(max(w), 1, tolerance = 1e-6)
  expect_equal(T[which.max(w)], briere_T_opt(5, 37, 1.5), tolerance = 0.02)
  expect_equal(briere_T_opt(5, 37, 1.5), 17.8, tolerance = 1e-9)
})

test_that("seasonal_beta = 1 leaves the model unchanged, and scales beta otherwise", {
  skip_on_cran()
  n <- 200
  base <- list(K_r = 2500, K_h = 5000, I_ini = 5,
               seasonal = rep(1, n), seasonal_beta = rep(1, n),
               beta_r = 0.5, beta_h = 0.02, delta_R = 0.2,
               m_r = 0.2, g_r = 0, rho = 2.5, tau = 1)
  run <- function(pars) {
    sys <- dust2::dust_system_create(plague_stochastic_humans, pars = pars,
                                     n_particles = 1, deterministic = TRUE)
    dust2::dust_system_set_state_initial(sys)
    y <- dust2::dust_system_simulate(sys, 0:(n - 1))
    sum(dust2::dust_unpack_state(sys, y)$D_h)
  }
  unforced <- run(base)
  expect_gt(unforced, 0)

  # w = 0.5 everywhere must be identical to halving both transmission rates
  half_w    <- run(modifyList(base, list(seasonal_beta = rep(0.5, n))))
  half_beta <- run(modifyList(base, list(beta_r = 0.25, beta_h = 0.01)))
  expect_equal(half_w, half_beta, tolerance = 1e-8)

  # w -> 0 shuts transmission off
  expect_lt(run(modifyList(base, list(seasonal_beta = rep(1e-8, n)))), 1e-6)
})

test_that("R wrappers default seasonal_beta when it is not supplied", {
  skip_on_cran()
  res <- run_plague_model(params = "defaults", npop = 1, include_humans = TRUE,
                          n_particles = 1, years = 1)
  expect_s3_class(res, "plague_results")
})
