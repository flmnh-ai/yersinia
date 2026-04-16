test_that("basic model runs without error", {
  results <- run_plague_model(
    scenario = "defaults",
    npop = 1,
    n_particles = 10,
    years = 1
  )
  expect_s3_class(results, "plague_results")
  expect_true(all(results$value >= 0))
  # Compartments should be S, I, R, Q (carcass model)
  expect_true(all(c("S", "I", "R", "Q") %in% unique(results$compartment)))
})

test_that("human model runs without error", {
  results <- run_plague_model(
    scenario = "defaults",
    npop = 1,
    include_humans = TRUE,
    n_particles = 10,
    years = 1
  )
  expect_s3_class(results, "plague_results")
  expect_true(all(results$value >= 0))
  # Should have rat + human compartments
  expect_true(all(c("S", "I", "R", "Q", "Sh", "Ih", "Rh", "Dh") %in% unique(results$compartment)))
})

test_that("R0 calculation is correct", {
  params <- load_scenario("defaults")
  R0 <- calculate_R0(params)
  expect_true(is.numeric(R0))
  expect_true(R0 > 0)
})

test_that("all scenarios load successfully", {
  scenarios <- c("defaults", "keeling-gilligan", "modern-estimates", "historical", "didelot")
  for (s in scenarios) {
    params <- load_scenario(s)
    expect_s3_class(params, "scenario_parameters")
    # All should have core carcass model params
    expect_true(all(c("beta_r", "rho", "delta_R", "m_r", "g_r") %in% names(params)))
  }
})
