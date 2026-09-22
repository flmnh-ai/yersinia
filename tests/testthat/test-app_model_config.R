# model_config helpers: configurable_param_names, available_scenarios,
# model_config_default, model_config_resolve.

test_that("configurable_param_names excludes system + always-fixed parameters", {
  configurable <- configurable_param_names()
  for (excluded in c("tau", "seasonal_beta", "obs_period", "iota")) {
    expect_false(excluded %in% configurable)
  }
  # Sanity: classic fitted parameters are configurable.
  for (included in c("beta_h", "beta_r", "rho", "kappa", "K_h")) {
    expect_true(included %in% configurable)
  }
})

test_that("available_scenarios matches the bundled YAMLs", {
  scenarios <- available_scenarios()
  expect_setequal(scenarios,
                  c("defaults", "didelot", "historical",
                    "keeling-gilligan", "modern-estimates"))
  for (s in scenarios) {
    expect_no_error(load_scenario(s))
  }
})

test_that("model_config_default returns a complete config", {
  cfg <- model_config_default()
  expect_named(cfg, c("scenario", "shared", "local"))
  expect_true(cfg$scenario %in% available_scenarios())
  expect_true(all(cfg$shared %in% configurable_param_names()))
  expect_true(all(cfg$local  %in% configurable_param_names()))
})

test_that("model_config_resolve assigns scope correctly", {
  cfg <- list(scenario = "historical",
              shared = c("beta_r", "rho"),
              local  = c("beta_h"))
  tbl <- model_config_resolve(cfg)
  expect_s3_class(tbl, "tbl_df")
  expect_named(tbl, c("parameter", "scope", "scenario_value", "resolved_value"))

  expect_equal(tbl$scope[tbl$parameter == "beta_r"], "shared")
  expect_equal(tbl$scope[tbl$parameter == "rho"],    "shared")
  expect_equal(tbl$scope[tbl$parameter == "beta_h"], "local")
  # Anything not chosen should be fixed.
  expect_equal(tbl$scope[tbl$parameter == "K_r"], "fixed")
})

test_that("model_config_resolve resolved_value is scenario_value for fixed and NA otherwise", {
  cfg <- list(scenario = "historical",
              shared = c("kappa"),
              local  = c("beta_h"))
  tbl <- model_config_resolve(cfg)

  fixed_rows  <- tbl$scope == "fixed"
  fitted_rows <- tbl$scope %in% c("shared", "local")

  expect_true(all(tbl$resolved_value[fixed_rows] == tbl$scenario_value[fixed_rows] |
                  is.na(tbl$resolved_value[fixed_rows])))
  expect_true(all(is.na(tbl$resolved_value[fitted_rows])))
})

test_that("model_config_resolve gives local precedence over shared on conflict", {
  cfg <- list(scenario = "historical",
              shared = c("beta_h"),
              local  = c("beta_h"))
  tbl <- model_config_resolve(cfg)
  expect_equal(tbl$scope[tbl$parameter == "beta_h"], "local")
})
