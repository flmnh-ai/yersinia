# Synthetic posterior::draws_array fixtures exercising each detector's
# fire / no-fire paths and the runner. Avoids constructing a real monty
# samples object so tests stay fast and dependency-light.

# Helper: build a draws_array with given per-parameter draws.
# `pars` is a named list of either:
#   - numeric vector (length = n_iter * n_chains) flattened in iter-major order
#   - matrix (n_iter × n_chains)
make_draws <- function(pars, n_iter = NULL, n_chains = NULL) {
  if (is.matrix(pars[[1]])) {
    n_iter <- nrow(pars[[1]])
    n_chains <- ncol(pars[[1]])
  }
  arr <- array(NA_real_, dim = c(n_iter, n_chains, length(pars)),
               dimnames = list(iteration = NULL, chain = NULL,
                               variable = names(pars)))
  for (i in seq_along(pars)) {
    v <- pars[[i]]
    if (is.matrix(v)) arr[, , i] <- v
    else arr[, , i] <- matrix(v, nrow = n_iter, ncol = n_chains)
  }
  posterior::as_draws_array(arr)
}

set.seed(42)

# ---- diagnose_chain_stuck ---------------------------------------------------

test_that("chain_stuck fires when chains disagree", {
  # Two chains with disjoint locations — high R-hat.
  n_iter <- 200
  ch1 <- rnorm(n_iter, mean = 0,  sd = 0.1)
  ch2 <- rnorm(n_iter, mean = 10, sd = 0.1)
  draws <- make_draws(list(theta = cbind(ch1, ch2)))
  rec <- diagnose_chain_stuck(draws, threshold = 1.5)
  expect_false(is.null(rec))
  expect_equal(rec$detector, "chain_stuck")
  expect_equal(rec$severity, "alert")
  expect_match(rec$message, "theta")
  expect_true("rhat" %in% names(rec$details))
  expect_true(rec$details$rhat["theta"] > 1.5)
})

test_that("chain_stuck returns NULL when chains agree", {
  n_iter <- 500
  m <- matrix(rnorm(n_iter * 4), nrow = n_iter, ncol = 4)
  draws <- make_draws(list(theta = m, kappa = m + 5))
  expect_null(diagnose_chain_stuck(draws, threshold = 1.5))
})

# ---- diagnose_bound_piling --------------------------------------------------

test_that("bound_piling fires when posterior piles at lower bound", {
  # Draws clustered near 0.001, prior bound [0.001, 0.15].
  n_iter <- 200
  m <- matrix(0.001 + abs(rnorm(n_iter * 2, sd = 0.0005)),
              nrow = n_iter, ncol = 2)
  draws <- make_draws(list(beta_h = m))
  rec <- diagnose_bound_piling(draws, bounds = list(beta_h = c(0.001, 0.15)),
                               threshold = 0.20, edge = 0.05)
  expect_false(is.null(rec))
  expect_equal(rec$detector, "bound_piling")
  expect_equal(rec$severity, "warn")
  expect_match(rec$message, "beta_h")
  expect_match(rec$message, "lower")
  expect_equal(rec$details$hits$beta_h$side, "lower")
})

test_that("bound_piling returns NULL when posterior is well inside bounds", {
  n_iter <- 200
  m <- matrix(rnorm(n_iter * 4, mean = 0.05, sd = 0.005),
              nrow = n_iter, ncol = 4)
  draws <- make_draws(list(beta_h = m))
  expect_null(diagnose_bound_piling(draws, bounds = list(beta_h = c(0, 0.15))))
})

test_that("bound_piling skips parameters with non-finite bounds", {
  n_iter <- 200
  m <- matrix(rexp(n_iter * 4, rate = 1), nrow = n_iter, ncol = 4)
  draws <- make_draws(list(lambda = m))
  expect_null(diagnose_bound_piling(draws,
                                    bounds = list(lambda = c(0, Inf))))
})

test_that("bound_piling identifies upper-bound piling specifically", {
  n_iter <- 200
  # Draws clustered near upper bound 12.
  m <- matrix(12 - abs(rnorm(n_iter * 2, sd = 0.05)), nrow = n_iter, ncol = 2)
  draws <- make_draws(list(R0 = m))
  rec <- diagnose_bound_piling(draws, bounds = list(R0 = c(0.5, 12)))
  expect_false(is.null(rec))
  expect_equal(rec$details$hits$R0$side, "upper")
})

test_that("bound_piling returns NULL when bounds is empty", {
  n_iter <- 100
  draws <- make_draws(list(theta = matrix(rnorm(n_iter * 2),
                                          nrow = n_iter, ncol = 2)))
  expect_null(diagnose_bound_piling(draws, bounds = list()))
  expect_null(diagnose_bound_piling(draws, bounds = NULL))
})

test_that("bound_piling skips parameters not present in samples", {
  n_iter <- 100
  draws <- make_draws(list(theta = matrix(rnorm(n_iter * 2),
                                          nrow = n_iter, ncol = 2)))
  # absent_par isn't in draws — should skip without error.
  expect_null(diagnose_bound_piling(draws,
                                    bounds = list(absent_par = c(0, 1))))
})

# ---- diagnose_low_kappa -----------------------------------------------------

test_that("low_kappa fires when median is below threshold", {
  n_iter <- 200
  m <- matrix(rnorm(n_iter * 2, mean = 2, sd = 0.5), nrow = n_iter, ncol = 2)
  draws <- make_draws(list(kappa = m))
  rec <- diagnose_low_kappa(draws, threshold = 5)
  expect_false(is.null(rec))
  expect_equal(rec$detector, "low_kappa")
  expect_equal(rec$severity, "warn")
  expect_match(rec$message, "kappa")
  expect_lt(rec$details$median, 5)
})

test_that("low_kappa returns NULL when median is at or above threshold", {
  n_iter <- 200
  m <- matrix(rnorm(n_iter * 2, mean = 20, sd = 1), nrow = n_iter, ncol = 2)
  draws <- make_draws(list(kappa = m))
  expect_null(diagnose_low_kappa(draws, threshold = 5))
})

test_that("low_kappa returns NULL when kappa is not in samples", {
  n_iter <- 100
  draws <- make_draws(list(theta = matrix(rnorm(n_iter * 2),
                                          nrow = n_iter, ncol = 2)))
  expect_null(diagnose_low_kappa(draws, threshold = 5))
})

test_that("low_kappa accepts a custom parameter name", {
  n_iter <- 200
  m <- matrix(rnorm(n_iter * 2, mean = 1, sd = 0.2), nrow = n_iter, ncol = 2)
  draws <- make_draws(list(my_disp = m))
  rec <- diagnose_low_kappa(draws, par = "my_disp", threshold = 5)
  expect_false(is.null(rec))
  expect_match(rec$message, "my_disp")
})

# ---- run_diagnostics --------------------------------------------------------

test_that("run_diagnostics returns all firing records, drops NULLs", {
  n_iter <- 200
  # Construct a fit that triggers all three detectors.
  ch1 <- rnorm(n_iter, mean = 0,    sd = 0.1)
  ch2 <- rnorm(n_iter, mean = 10,   sd = 0.1)            # chain_stuck
  beta_pile <- matrix(0.001 + abs(rnorm(n_iter * 2, sd = 0.0005)),
                      nrow = n_iter, ncol = 2)            # bound_piling
  kappa_low <- matrix(rnorm(n_iter * 2, mean = 1.5, sd = 0.2),
                      nrow = n_iter, ncol = 2)            # low_kappa
  draws <- make_draws(list(theta = cbind(ch1, ch2),
                           beta_h = beta_pile,
                           kappa = kappa_low))
  recs <- run_diagnostics(draws, bounds = list(beta_h = c(0.001, 0.15)))
  expect_length(recs, 3)
  expect_setequal(vapply(recs, `[[`, character(1), "detector"),
                  c("chain_stuck", "bound_piling", "low_kappa"))
  for (r in recs) {
    expect_true(all(c("detector", "severity", "message",
                      "suggested_fix", "details") %in% names(r)))
  }
})

test_that("run_diagnostics returns empty list on a clean fit", {
  n_iter <- 500
  m <- matrix(rnorm(n_iter * 4, mean = 0.05, sd = 0.005),
              nrow = n_iter, ncol = 4)
  k <- matrix(rnorm(n_iter * 4, mean = 20, sd = 1), nrow = n_iter, ncol = 4)
  draws <- make_draws(list(beta_h = m, kappa = k))
  recs <- run_diagnostics(draws, bounds = list(beta_h = c(0, 0.15)))
  expect_length(recs, 0)
})

test_that("run_diagnostics skips bound_piling when bounds is NULL", {
  n_iter <- 200
  ch1 <- rnorm(n_iter, mean = 0,  sd = 0.1)
  ch2 <- rnorm(n_iter, mean = 10, sd = 0.1)
  draws <- make_draws(list(theta = cbind(ch1, ch2)))
  recs <- run_diagnostics(draws, bounds = NULL)
  expect_true(all(vapply(recs, `[[`, character(1), "detector") != "bound_piling"))
})
