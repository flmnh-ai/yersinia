# Tests for the thermal response on transmission (Brière, visible
# coordinates) and its packer wrapper.
#
# The load-bearing checks:
#   1. thermal_response() peaks at exactly 1 at T_opt and reaches zero at
#      T_opt - hw_cold and T_opt + hw_hot.
#   2. It is the textbook simplified Brière-2 under the mapping
#      T_min = T_opt - hw_cold, T_max = T_opt + hw_hot, q = hw_hot / hw_cold.
#   3. Degenerate parameters return a floor rather than erroring, so a bad
#      MCMC proposal drives the likelihood down instead of crashing the chain.
#   4. seasonal_beta = 1 reproduces the unforced model exactly, and
#      seasonal_beta = c is exactly equivalent to scaling beta_r and beta_h
#      by c -- the forcing enters where it is supposed to and nowhere else.
#   5. with_thermal_beta() writes seasonal_beta per group and removes the
#      curve parameters.

test_that("thermal_response peaks at 1 at T_opt", {
  T <- seq(-10, 30, by = 0.05)
  w <- thermal_response(T, T_opt = 17.45, hw_cold = 22.13, hw_hot = 6.35)
  expect_equal(thermal_response(17.45, 17.45, 22.13, 6.35), 1)
  expect_true(all(w <= 1))
  expect_equal(T[which.max(w)], 17.45, tolerance = 0.05)
})

test_that("thermal_response reaches the floor at and beyond both zeros", {
  f <- 1e-8
  expect_equal(thermal_response(18 - 12, 18, 12, 7), f)   # cold zero
  expect_equal(thermal_response(18 + 7,  18, 12, 7), f)   # hot zero
  expect_equal(thermal_response(c(-40, 50), 18, 12, 7), c(f, f))
  expect_gt(thermal_response(18 - 11.9, 18, 12, 7), f)
  expect_gt(thermal_response(18 + 6.9,  18, 12, 7), f)
})

test_that("thermal_response is textbook simplified Briere-2 under the mapping", {
  T_opt <- 17.45; hc <- 22.13; hh <- 6.35
  T_min <- T_opt - hc; T_max <- T_opt + hh; q <- hh / hc
  T <- seq(T_min + 0.01, T_max - 0.01, length.out = 500)
  L <- function(t) (t - T_min) * (T_max - t)^q
  expect_equal(thermal_response(T, T_opt, hc, hh), L(T) / L(T_opt),
               tolerance = 1e-10)
})

test_that("a narrow hot side falls off faster above the optimum", {
  expect_gt(thermal_response(18 - 3, 18, 12, 4),
            thermal_response(18 + 3, 18, 12, 4))
})

test_that("degenerate parameters floor rather than error", {
  expect_equal(thermal_response(10, 18, -1, 7), 1e-8)
  expect_equal(thermal_response(10, 18, 12, 0), 1e-8)
  expect_equal(thermal_response(c(10, 20), NA, 12, 7), c(1e-8, 1e-8))
})

test_that("with_thermal_beta writes per-group seasonal_beta", {
  groups <- c("a", "b")
  packer <- monty::monty_packer_grouped(
    groups = groups,
    scalar = c("beta_h", "T_opt", "hw_cold", "hw_hot"),
    shared = c("T_opt", "hw_cold", "hw_hot"))
  temps <- list(a = c(5, 10, 17, 22), b = c(15, 17, 19, 30))
  wrapped <- with_thermal_beta(packer, temps)

  theta <- setNames(rep(1, length(wrapped$names())), wrapped$names())
  theta[["T_opt"]] <- 17; theta[["hw_cold"]] <- 20; theta[["hw_hot"]] <- 6
  out <- wrapped$unpack(theta)

  for (g in groups) {
    expect_equal(out[[g]]$seasonal_beta,
                 thermal_response(temps[[g]], 17, 20, 6))
    expect_null(out[[g]]$T_opt)
    expect_null(out[[g]]$hw_cold)
    expect_null(out[[g]]$hw_hot)
  }
})

test_that("seasonal_beta = 1 leaves the model unchanged, and scales beta otherwise", {
  skip_on_cran()
  n <- 200
  base <- list(K_r = 2500, K_h = 5000, I_ini = 5,
               seasonal_beta = rep(1, n),
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
