test_that("basic model runs without error", {
  results <- run_plague_model(
    params = "defaults",
    npop = 1,
    n_particles = 10,
    years = 1
  )
  expect_s3_class(results, "plague_results")
  expect_true(all(results$value >= 0))
})

test_that("R0 calculation is correct", {
  params <- load_scenario("defaults")
  R0 <- calculate_R0(params)
  expect_true(is.numeric(R0))
  expect_true(R0 > 0)
})
