# lab_fit_forward_sim — posterior predictive trajectories for the hero plot.

test_that("lab_fit_forward_sim returns a long tibble with mu column", {
  samples <- shiny::isolate({
    lab <- LabSession$new(
      cohort_ids = "Eyam_1665",
      model_config = list(scenario = "historical",
                          shared = c("beta_r", "kappa"),
                          local  = character(0))
    )
    setup <- lab_fit_assemble(lab)
    list(setup = setup,
         samples = lab_fit_run(setup, n_chains = 1L, n_iter = 50L))
  })

  out <- lab_fit_forward_sim(samples$setup, samples$samples, n_draws = 5L)
  expect_true(all(c("draw", "group", "time", "mu") %in% names(out)))
  expect_equal(length(unique(out$draw)), 5L)
  # Groups carry the resolved id, not the legacy string the session was
  # built with.
  expect_equal(unique(out$group), outbreak_resolve_id("Eyam_1665"))
  expect_true(all(is.finite(out$mu)))
})

test_that("lab_fit_forward_sim handles multi-outbreak fits", {
  samples <- shiny::isolate({
    lab <- LabSession$new(
      cohort_ids = c("Eyam_1665", "Givry_1348"),
      model_config = list(scenario = "historical",
                          shared = c("beta_r", "kappa"),
                          local  = character(0))
    )
    setup <- lab_fit_assemble(lab)
    list(setup = setup,
         samples = lab_fit_run(setup, n_chains = 1L, n_iter = 50L))
  })

  out <- lab_fit_forward_sim(samples$setup, samples$samples, n_draws = 3L)
  expect_setequal(unique(out$group),
                  outbreak_resolve_id(c("Eyam_1665", "Givry_1348")))
  expect_equal(length(unique(out$draw)), 3L)
})
