# prior_families registry, prior_default, priors_default, prior_density,
# prior_to_dsl, priors_to_dsl.

test_that("prior_families covers the v1 distributions", {
  fam <- prior_families()
  expect_setequal(names(fam),
                  c("Uniform", "Normal", "LogNormal", "Gamma",
                    "Exponential", "Beta"))
  for (f in fam) {
    expect_true(all(c("params", "density", "range") %in% names(f)))
  }
})

test_that("prior_default returns a sensible prior for canonical parameters", {
  for (p in c("beta_h", "beta_r", "rho", "kappa", "K_h", "I_ini", "p_obs",
              "lambda_baseline")) {
    pr <- prior_default(p)
    expect_false(is.null(pr), info = p)
    expect_true(pr$family %in% names(prior_families()))
  }
})

test_that("prior_default returns NULL for unknown parameters", {
  expect_null(prior_default("not_a_real_parameter"))
})

test_that("priors_default fills unknowns with Uniform(0, 1)", {
  out <- priors_default(c("beta_h", "made_up_param"))
  expect_named(out, c("beta_h", "made_up_param"))
  expect_equal(out$made_up_param, list(family = "Uniform",
                                       params = list(min = 0, max = 1)))
})

test_that("prior_density evaluates on a sensible grid", {
  pr <- list(family = "Uniform", params = list(min = 0, max = 1))
  d <- prior_density(pr, n = 100)
  expect_named(d, c("x", "density"))
  expect_equal(nrow(d), 100)
  # Mass within support is ~1 (length of grid * mean density ~ 1).
  in_support <- d$x >= 0 & d$x <= 1
  expect_true(all(d$density[in_support] == 1))
})

test_that("prior_density handles Normal and Exponential", {
  d_norm <- prior_density(list(family = "Normal",
                               params = list(mean = 0, sd = 1)))
  expect_true(all(d_norm$density >= 0))
  # Peak should be near x = 0.
  expect_lt(abs(d_norm$x[which.max(d_norm$density)]), 0.2)

  d_exp <- prior_density(list(family = "Exponential",
                              params = list(rate = 1)))
  expect_true(all(d_exp$density >= 0))
  # Density at x=0 is ~1 for Exponential(1).
  expect_equal(d_exp$density[1], 1, tolerance = 0.01)
})

test_that("prior_to_dsl returns a syntactically valid expression", {
  expr <- prior_to_dsl("beta_h",
                       list(family = "Uniform",
                            params = list(min = 0.001, max = 0.15)))
  expect_true(is.call(expr))
  # LHS should be `beta_h`, RHS should call Uniform.
  expect_equal(as.character(expr[[2]]), "beta_h")
  expect_equal(as.character(expr[[3]][[1]]), "Uniform")
  expect_equal(as.numeric(expr[[3]][[2]]), 0.001)
  expect_equal(as.numeric(expr[[3]][[3]]), 0.15)
})

test_that("prior_to_dsl preserves family-specified arg order", {
  # Even if we reorder the params list, the DSL output uses family order.
  expr <- prior_to_dsl("kappa",
                       list(family = "Gamma",
                            params = list(rate = 1, shape = 2)))  # reversed
  expect_equal(as.character(expr[[3]][[1]]), "Gamma")
  expect_equal(as.numeric(expr[[3]][[2]]), 2)  # shape (Gamma's first)
  expect_equal(as.numeric(expr[[3]][[3]]), 1)  # rate
})

test_that("priors_to_dsl converts a full list", {
  priors <- list(
    beta_h = list(family = "Uniform", params = list(min = 0, max = 0.15)),
    kappa  = list(family = "Exponential", params = list(rate = 0.1))
  )
  exprs <- priors_to_dsl(priors)
  expect_length(exprs, 2)
  expect_equal(as.character(exprs[[1]][[2]]), "beta_h")
  expect_equal(as.character(exprs[[2]][[2]]), "kappa")
  expect_equal(as.character(exprs[[1]][[3]][[1]]), "Uniform")
  expect_equal(as.character(exprs[[2]][[3]][[1]]), "Exponential")
})

test_that("prior_to_dsl errors on unknown family", {
  expect_error(
    prior_to_dsl("x", list(family = "Cauchy", params = list(loc = 0, scale = 1))),
    "Unknown prior family"
  )
})

test_that("prior_bounds returns finite bounds only for compactly-supported families", {
  expect_equal(prior_bounds(list(family = "Uniform",
                                 params = list(min = 0, max = 1))),
               c(0, 1))
  expect_equal(prior_bounds(list(family = "Beta",
                                 params = list(shape1 = 2, shape2 = 2))),
               c(0, 1))
  # Half-bounded.
  expect_equal(prior_bounds(list(family = "Exponential",
                                 params = list(rate = 1))),
               c(0, Inf))
  expect_equal(prior_bounds(list(family = "Gamma",
                                 params = list(shape = 2, rate = 1))),
               c(0, Inf))
  # Unbounded.
  expect_equal(prior_bounds(list(family = "Normal",
                                 params = list(mean = 0, sd = 1))),
               c(-Inf, Inf))
})

test_that("priors_to_bounds expands per-group decorated names", {
  priors <- list(
    beta_h = list(family = "Uniform", params = list(min = 0.001, max = 0.15)),
    kappa  = list(family = "Exponential", params = list(rate = 0.1))
  )
  packer_names <- c("beta_h<Eyam_1665>", "beta_h<Givry_1348>", "kappa")
  out <- priors_to_bounds(priors, packer_names)
  expect_named(out, packer_names)
  expect_equal(out$`beta_h<Eyam_1665>`,  c(0.001, 0.15))
  expect_equal(out$`beta_h<Givry_1348>`, c(0.001, 0.15))
  expect_equal(out$kappa, c(0, Inf))
})

test_that("priors_to_bounds skips packer names with no matching prior", {
  priors <- list(beta_h = list(family = "Uniform",
                               params = list(min = 0, max = 1)))
  out <- priors_to_bounds(priors, c("beta_h", "ghost_param"))
  expect_named(out, "beta_h")
})
