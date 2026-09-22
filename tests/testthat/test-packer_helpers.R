# Tests for the composable packer wrappers in R/packer_helpers.R.

make_inner_packer <- function(groups, local_names, shared_names, fixed_pars) {
  monty::monty_packer_grouped(
    groups = groups,
    scalar = c(local_names, shared_names),
    shared = shared_names,
    fixed  = fixed_pars
  )
}

# Common fixture: 3 outbreaks with shared R0 and thermal-curve parameters.
make_fixture <- function() {
  groups <- c("Barcelona_15", "Malta_13", "Debrecen_44")
  shared_names <- c("R0", "T_opt", "hw_cold", "hw_hot", "kappa")
  local_names  <- c("beta_h", "I_ini", "lambda_baseline")
  fixed_pars <- list(
    g_r = 0.05, g_h = 0.1, rho = 2.5, delta_R = 0.2,
    p_obs = 0.8, beta_I = 0, R_ini = 0
  )
  group_fixed_pars <- list(
    Barcelona_15 = list(K_h = 30000, K_r = 30000),
    Malta_13     = list(K_h = 90000, K_r = 90000),
    Debrecen_44  = list(K_h = 12000, K_r = 12000)
  )
  group_temp <- list(
    Barcelona_15 = rep(c(10, 12, 16, 20, 24, 26, 27, 26, 22, 18, 14, 11), each = 31)[1:365],
    Malta_13     = rep(c(13, 13, 15, 18, 22, 26, 28, 28, 25, 22, 18, 15), each = 31)[1:365],
    Debrecen_44  = rep(c(-2, 0, 5, 11, 17, 20, 22, 21, 16, 10, 4, 0), each = 31)[1:365]
  )
  list(groups = groups, shared_names = shared_names, local_names = local_names,
       fixed_pars = fixed_pars, group_fixed_pars = group_fixed_pars,
       group_temp = group_temp)
}

make_theta <- function(packer, overrides = list()) {
  nm <- packer$names()
  theta <- setNames(rep(1, length(nm)), nm)
  for (k in names(overrides)) theta[[k]] <- overrides[[k]]
  theta
}

test_that("with_per_group_fixed splices group-specific values", {
  fx <- make_fixture()
  inner <- make_inner_packer(fx$groups, fx$local_names, fx$shared_names, fx$fixed_pars)
  wrapped <- with_per_group_fixed(inner, fx$group_fixed_pars)

  theta <- make_theta(wrapped)
  out <- wrapped$unpack(theta)

  expect_equal(out$Barcelona_15$K_h, 30000)
  expect_equal(out$Barcelona_15$K_r, 30000)
  expect_equal(out$Malta_13$K_h, 90000)
  expect_equal(out$Debrecen_44$K_r, 12000)
  # Class preserved so dust2 grouped filter accepts it.
  expect_identical(class(wrapped), class(inner))
})

test_that("with_R0_to_beta_r matches the closed-form derivation", {
  fx <- make_fixture()
  inner <- make_inner_packer(fx$groups, fx$local_names, fx$shared_names, fx$fixed_pars)
  wrapped <- with_per_group_fixed(inner, fx$group_fixed_pars) |>
    with_R0_to_beta_r()

  theta <- make_theta(wrapped, list(R0 = 5))
  out <- wrapped$unpack(theta)

  expected_beta_r <- 5 * 0.2 / ((1 - 0.05) * (1 - exp(-2.5)))
  expect_equal(out$Barcelona_15$beta_r, expected_beta_r)
  expect_null(out$Barcelona_15$R0)
})

test_that("composed wrappers survive callr serialization", {
  # monty_runner_callr ships unpack closures to workers with saveRDS; the
  # closure must round-trip without losing access to its helpers.
  fx <- make_fixture()
  inner <- make_inner_packer(fx$groups, fx$local_names, fx$shared_names, fx$fixed_pars)
  wrapped <- inner |>
    with_per_group_fixed(fx$group_fixed_pars) |>
    with_R0_to_beta_r() |>
    with_thermal_beta(fx$group_temp)

  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(wrapped$unpack, tmp)
  restored_unpack <- readRDS(tmp)

  theta <- make_theta(wrapped, list(R0 = 5, T_opt = 17, hw_cold = 20, hw_hot = 6))
  expect_equal(restored_unpack(theta), wrapped$unpack(theta))
})
