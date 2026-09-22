# Lock the composable packer wrappers to the inlined implementations in
# vignettes/monty-multicity-mechanistic.qmd and monty-barcelona-hierarchical.qmd.
# If a vignette implementation changes intentionally, update both the wrapper
# and the corresponding hand-coded reference here.

make_inner_packer <- function(groups, local_names, shared_names, fixed_pars) {
  monty::monty_packer_grouped(
    groups = groups,
    scalar = c(local_names, shared_names),
    shared = shared_names,
    fixed  = fixed_pars
  )
}

# Reference (inlined) versions copied verbatim from the vignettes — the
# composable wrappers must produce identical unpack output.

ref_with_group_fixed_alpha <- function(packer, group_fixed, group_seasonal) {
  R0_to_beta_r <- function(R0, g_r, rho, delta_R) {
    R0 * delta_R / ((1 - g_r) * (1 - exp(-rho)))
  }
  out <- packer
  inner_unpack <- packer$unpack
  out$unpack <- function(x) {
    u <- inner_unpack(x)
    Map(function(g, gp) {
      pars <- c(gp, group_fixed[[g]])
      pars$beta_r <- R0_to_beta_r(pars$R0, pars$g_r, pars$rho, pars$delta_R)
      pars$R0     <- NULL
      pars$seasonal <- group_seasonal[[g]] ^ pars$alpha
      pars$alpha    <- NULL
      pars
    }, names(u), u)
  }
  class(out) <- class(packer)
  out
}

ref_with_group_fixed_briere <- function(packer, group_fixed, group_temp,
                                        T_ref = 17.8, cap = 1000) {
  R0_to_beta_r <- function(R0, g_r, rho, delta_R) {
    R0 * delta_R / ((1 - g_r) * (1 - exp(-rho)))
  }
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
  out <- packer
  inner_unpack <- packer$unpack
  out$unpack <- function(x) {
    u <- inner_unpack(x)
    Map(function(g, gp) {
      pars <- c(gp, group_fixed[[g]])
      pars$beta_r <- R0_to_beta_r(pars$R0, pars$g_r, pars$rho, pars$delta_R)
      pars$R0     <- NULL
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

# Common fixture: 3 outbreaks, the hierarchical-A shared/local set.
make_fixture_alpha <- function() {
  groups <- c("Barcelona_15", "Malta_13", "Debrecen_44")
  shared_names <- c("R0", "alpha", "kappa")
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
  group_seasonal <- list(
    Barcelona_15 = seq(0.8, 1.2, length.out = 365),
    Malta_13     = seq(1.0, 1.4, length.out = 365),
    Debrecen_44  = seq(0.5, 1.5, length.out = 365)
  )
  list(groups = groups, shared_names = shared_names, local_names = local_names,
       fixed_pars = fixed_pars, group_fixed_pars = group_fixed_pars,
       group_seasonal = group_seasonal)
}

make_fixture_briere <- function() {
  groups <- c("Barcelona_15", "Malta_13", "Debrecen_44")
  shared_names <- c("R0", "T_min", "T_max", "q_briere", "kappa")
  local_names  <- c("beta_h", "beta_I", "I_ini", "lambda_baseline")
  fixed_pars <- list(
    g_r = 0.05, g_h = 0.1, rho = 2.5, delta_R = 0.2,
    p_obs = 0.8, R_ini = 0
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
  fx <- make_fixture_alpha()
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
  fx <- make_fixture_alpha()
  inner <- make_inner_packer(fx$groups, fx$local_names, fx$shared_names, fx$fixed_pars)
  wrapped <- with_per_group_fixed(inner, fx$group_fixed_pars) |>
    with_R0_to_beta_r()

  theta <- make_theta(wrapped, list(R0 = 5))
  out <- wrapped$unpack(theta)

  expected_beta_r <- 5 * 0.2 / ((1 - 0.05) * (1 - exp(-2.5)))
  expect_equal(out$Barcelona_15$beta_r, expected_beta_r)
  expect_null(out$Barcelona_15$R0)
})

test_that("composed wrappers reproduce inlined alpha-seasonal version", {
  fx <- make_fixture_alpha()
  inner1 <- make_inner_packer(fx$groups, fx$local_names, fx$shared_names, fx$fixed_pars)
  inner2 <- make_inner_packer(fx$groups, fx$local_names, fx$shared_names, fx$fixed_pars)

  composed <- inner1 |>
    with_per_group_fixed(fx$group_fixed_pars) |>
    with_R0_to_beta_r() |>
    with_alpha_seasonal(fx$group_seasonal)

  reference <- ref_with_group_fixed_alpha(inner2, fx$group_fixed_pars, fx$group_seasonal)

  for (R0 in c(2, 5, 8)) for (alpha in c(0.5, 1, 1.5)) {
    theta <- make_theta(composed, list(R0 = R0, alpha = alpha))
    expect_equal(composed$unpack(theta), reference$unpack(theta),
                 info = sprintf("R0=%g, alpha=%g", R0, alpha))
  }
})

test_that("composed wrappers reproduce inlined Brière version", {
  fx <- make_fixture_briere()
  inner1 <- make_inner_packer(fx$groups, fx$local_names, fx$shared_names, fx$fixed_pars)
  inner2 <- make_inner_packer(fx$groups, fx$local_names, fx$shared_names, fx$fixed_pars)

  composed <- inner1 |>
    with_per_group_fixed(fx$group_fixed_pars) |>
    with_R0_to_beta_r() |>
    with_briere_seasonal(fx$group_temp)

  reference <- ref_with_group_fixed_briere(inner2, fx$group_fixed_pars, fx$group_temp)

  cases <- list(
    list(R0 = 5,  T_min = 5,  T_max = 30, q_briere = 1.5),  # standard
    list(R0 = 3,  T_min = -5, T_max = 32, q_briere = 2.0),  # wider range
    list(R0 = 7,  T_min = 18, T_max = 25, q_briere = 1.0),  # T_ref=17.8 below T_min → cap
    list(R0 = 4,  T_min = 0,  T_max = 17, q_briere = 1.5)   # T_ref above T_max → cap
  )
  for (cs in cases) {
    theta <- make_theta(composed, cs)
    expect_equal(composed$unpack(theta), reference$unpack(theta),
                 info = paste(names(cs), "=", unlist(cs), collapse = ", "))
  }
})

test_that("Brière out-of-range produces capped seasonal vector", {
  fx <- make_fixture_briere()
  inner <- make_inner_packer(fx$groups, fx$local_names, fx$shared_names, fx$fixed_pars)
  wrapped <- inner |>
    with_per_group_fixed(fx$group_fixed_pars) |>
    with_R0_to_beta_r() |>
    with_briere_seasonal(fx$group_temp, T_ref = 17.8, cap = 999)

  # T_ref=17.8 inside [18, 25] is FALSE since 17.8 <= T_min(=18); whole vector caps.
  theta <- make_theta(wrapped, list(R0 = 5, T_min = 18, T_max = 25, q_briere = 1))
  out <- wrapped$unpack(theta)
  expect_true(all(out$Barcelona_15$seasonal == 999))
  expect_length(out$Barcelona_15$seasonal, length(fx$group_temp$Barcelona_15))
})

test_that("composed wrappers survive callr serialization", {
  # The whole motivation for inlining helpers — the closure must round-trip
  # through saveRDS without losing access to its helpers.
  fx <- make_fixture_alpha()
  inner <- make_inner_packer(fx$groups, fx$local_names, fx$shared_names, fx$fixed_pars)
  wrapped <- inner |>
    with_per_group_fixed(fx$group_fixed_pars) |>
    with_R0_to_beta_r() |>
    with_alpha_seasonal(fx$group_seasonal)

  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(wrapped$unpack, tmp)
  restored_unpack <- readRDS(tmp)

  theta <- make_theta(wrapped, list(R0 = 5, alpha = 1))
  expect_equal(restored_unpack(theta), wrapped$unpack(theta))
})
