# cohort_data, cohort_population, cohort_obs_period, lab_fit_assemble,
# lab_fit_run. Includes a real (tiny) end-to-end fit on Eyam 1665.

test_that("cohort_data returns synchronized NA-padded long format", {
  d <- cohort_data(c("Eyam_1665", "Givry_1348"))
  expect_named(d, c("group", "time", "deaths"))
  expect_type(d$group, "character")
  T_max <- max(d$time)
  # Each group should have exactly T_max rows (NA-padded).
  counts <- table(d$group)
  expect_true(all(counts == T_max))
})

test_that("cohort_data errors on empty cohort", {
  expect_error(cohort_data(character(0)), "Empty cohort")
})

test_that("cohort_population returns one entry per cohort id", {
  pop <- cohort_population(c("Eyam_1665", "Cairo_1835"))
  expect_named(pop, c("Eyam_1665", "Cairo_1835"))
  expect_true(all(pop > 0))
  expect_true(pop[["Cairo_1835"]] > pop[["Eyam_1665"]])  # sanity
})

test_that("cohort_obs_period errors when cadences mix", {
  # London 1563 is weekly (obs_period = 7); others are daily (= 1).
  expect_error(cohort_obs_period(c("Eyam_1665", "London_1563")),
               "mixes observation cadences")
  # Same-cadence cohorts return the single value.
  expect_equal(cohort_obs_period(c("Eyam_1665", "Givry_1348")), 1L)
})

# Single-outbreak assembly + run

test_that("lab_fit_assemble builds a runnable single-outbreak setup", {
  shiny::isolate({
    lab <- LabSession$new(
      cohort_ids = "Eyam_1665",
      model_config = list(scenario = "historical",
                          shared = c("beta_r", "kappa"),
                          local  = character(0))
    )
    setup <- lab_fit_assemble(lab)
    expect_named(setup, c("posterior", "sampler", "packer", "unfilter",
                          "prior_model", "fixed_pars", "data",
                          "cohort_ids", "fitted_names"))
    expect_setequal(setup$fitted_names, c("beta_r", "kappa"))
    # Unfitted K_h pinned at outbreak population (Eyam ~700).
    expect_equal(setup$fixed_pars$K_h,
                 cohort_population("Eyam_1665")[["Eyam_1665"]])
    # Packer parameter names match the prior model's parameters.
    expect_setequal(setup$packer$names(), c("beta_r", "kappa"))
  })
})

test_that("lab_fit_run produces samples on a tiny single-outbreak fit", {
  shiny::isolate({
    lab <- LabSession$new(
      cohort_ids = "Eyam_1665",
      model_config = list(scenario = "historical",
                          shared = c("beta_r", "kappa"),
                          local  = character(0))
    )
    setup <- lab_fit_assemble(lab)
    # Default burnin discards the first half — 100 iters -> 50 kept.
    samples <- lab_fit_run(setup, n_chains = 1L, n_iter = 100L)
    expect_s3_class(samples, "monty_samples")
    draws <- posterior::as_draws_array(samples)
    expect_setequal(posterior::variables(draws), c("beta_r", "kappa"))
    expect_equal(posterior::ndraws(draws), 50L)

    # burnin_frac = 0 keeps everything.
    samples_full <- lab_fit_run(setup, n_chains = 1L, n_iter = 50L,
                                burnin_frac = 0)
    expect_equal(posterior::ndraws(posterior::as_draws_array(samples_full)),
                 50L)
  })
})

# Multi-outbreak assembly

test_that("lab_fit_assemble builds a grouped setup for a multi-outbreak cohort", {
  shiny::isolate({
    lab <- LabSession$new(
      cohort_ids = c("Eyam_1665", "Givry_1348"),
      model_config = list(scenario = "historical",
                          shared = c("beta_r", "kappa"),
                          local  = c("beta_h"))
    )
    setup <- lab_fit_assemble(lab)
    # Packer should expose decorated local + bare shared names.
    expect_true("beta_r" %in% setup$packer$names())
    expect_true("kappa"  %in% setup$packer$names())
    # Group names are the resolved outbreak ids. lab_fit_assemble() resolves
    # the session's cohort once at the boundary, so the packer, the data's
    # `group` column and cohort_population()'s names all agree -- they used
    # to be able to drift when each helper resolved separately.
    expect_true("beta_h<41>" %in% setup$packer$names())  # Eyam 1665
    expect_true("beta_h<49>" %in% setup$packer$names())  # Givry 1348
    expect_setequal(setup$cohort_ids, c("41", "49"))
    # K_h NOT in fitted -> pinned per outbreak via with_per_group_fixed,
    # so it's NOT in packer$names() (group_fixed splice happens in unpack).
    expect_false("K_h" %in% setup$packer$names())
  })
})
