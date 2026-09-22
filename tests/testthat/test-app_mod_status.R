# status bar module — Run/Refit click triggers fit, error paths surface.

test_that("status_bar_server sets error fit_state when no cohort", {
  lab <- LabSession$new()
  shiny::testServer(
    status_bar_server, args = list(lab_session = lab),
    {
      session$setInputs(run = 1, n_chains = 1L, n_iter = 10L)
      expect_equal(lab$fit_state$status, "error")
      expect_match(lab$fit_state$error, "Select")
    }
  )
})

test_that(".status_msg renders idle / running / complete / error states", {
  expect_match(as.character(.status_msg(NULL)), "Ready")
  expect_match(as.character(.status_msg(list(status = "running"))), "Running")
  expect_match(as.character(.status_msg(list(status = "complete",
                                              n_chains = 2, n_iter = 100,
                                              duration = 1.5))),
               "Fit complete")
  expect_match(as.character(.status_msg(list(status = "error",
                                              error = "boom"))),
               "boom")
})

test_that("LabSession$to_list round-trips through saveRDS / readRDS", {
  lab <- LabSession$new(cohort_ids = "Eyam_1665")
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(lab$to_list(), tmp)
  restored <- readRDS(tmp)
  expect_equal(restored$cohort_ids, "Eyam_1665")
  expect_named(restored,
               c("cohort_ids", "model_config", "priors", "fit_state"))
})

test_that("LabSession$apply_list overwrites all session fields", {
  lab <- LabSession$new()
  lab$apply_list(list(
    cohort_ids = c("Eyam_1665", "Givry_1348"),
    model_config = list(scenario = "historical",
                        shared = c("beta_r"), local = character(0)),
    priors = list(beta_r = list(family = "Uniform",
                                params = list(min = 0, max = 1))),
    fit_state = list(status = "complete", n_chains = 2L)
  ))
  expect_equal(shiny::isolate(lab$cohort_ids),
               c("Eyam_1665", "Givry_1348"))
  expect_equal(shiny::isolate(lab$model_config$scenario), "historical")
  expect_named(shiny::isolate(lab$priors), "beta_r")
  expect_equal(shiny::isolate(lab$fit_state$status), "complete")
})

test_that("status_bar_server runs a tiny pilot when cohort is set", {
  lab <- LabSession$new(
    cohort_ids = "Eyam_1665",
    model_config = list(scenario = "historical",
                        shared = c("beta_r", "kappa"),
                        local  = character(0))
  )
  shiny::testServer(
    status_bar_server, args = list(lab_session = lab),
    {
      session$setInputs(run = 1, n_chains = 1L, n_iter = 30L)
      expect_equal(lab$fit_state$status, "complete")
      expect_false(is.null(lab$fit_state$samples))
      expect_false(is.null(lab$fit_state$setup))
    }
  )
})
