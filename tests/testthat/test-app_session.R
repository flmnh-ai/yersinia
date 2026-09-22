# LabSession R6: construction, mutation, save/load round-trip.

test_that("LabSession constructs with empty cohort by default", {
  shiny::isolate({
    s <- LabSession$new()
    expect_s3_class(s, "LabSession")
    expect_equal(s$cohort_ids, character(0))
  })
})

test_that("LabSession accepts initial cohort_ids", {
  shiny::isolate({
    s <- LabSession$new(cohort_ids = c("Barcelona_1490", "Eyam_1665"))
    expect_equal(s$cohort_ids, c("Barcelona_1490", "Eyam_1665"))
  })
})

test_that("LabSession cohort_ids is writable via assignment", {
  shiny::isolate({
    s <- LabSession$new()
    s$cohort_ids <- c("London_1563")
    expect_equal(s$cohort_ids, "London_1563")
    s$cohort_ids <- character(0)
    expect_equal(s$cohort_ids, character(0))
  })
})

test_that("to_list / from_list round-trip preserves cohort_ids", {
  shiny::isolate({
    original <- LabSession$new(cohort_ids = c("Cairo_1835", "Malta_1813"))
    state <- original$to_list()
    expect_equal(state$cohort_ids, c("Cairo_1835", "Malta_1813"))

    restored <- lab_session_from_list(state)
    expect_s3_class(restored, "LabSession")
    expect_equal(restored$cohort_ids, c("Cairo_1835", "Malta_1813"))
  })
})

test_that("from_list tolerates unknown keys (forward compatibility)", {
  shiny::isolate({
    state <- list(cohort_ids = c("Eyam_1665"),
                  future_field_we_dont_know_about = list(foo = 1))
    s <- lab_session_from_list(state)
    expect_equal(s$cohort_ids, "Eyam_1665")
  })
})

test_that("from_list with missing cohort_ids defaults to empty", {
  shiny::isolate({
    s <- lab_session_from_list(list())
    expect_equal(s$cohort_ids, character(0))
  })
})

# model_config field

test_that("LabSession initializes model_config from defaults", {
  shiny::isolate({
    s <- LabSession$new()
    expect_equal(s$model_config, model_config_default())
  })
})

test_that("LabSession accepts an explicit model_config", {
  shiny::isolate({
    cfg <- list(scenario = "didelot",
                shared = c("beta_r"), local = c("beta_h"))
    s <- LabSession$new(model_config = cfg)
    expect_equal(s$model_config, cfg)
  })
})

test_that("model_config is writable via assignment", {
  shiny::isolate({
    s <- LabSession$new()
    new_cfg <- list(scenario = "keeling-gilligan",
                    shared = c("kappa"), local = character(0))
    s$model_config <- new_cfg
    expect_equal(s$model_config, new_cfg)
  })
})

test_that("to_list / from_list round-trip preserves model_config", {
  shiny::isolate({
    cfg <- list(scenario = "didelot",
                shared = c("beta_r", "rho"),
                local  = c("beta_h", "K_h"))
    original <- LabSession$new(cohort_ids = c("Cairo_1835"),
                               model_config = cfg)
    state <- original$to_list()
    expect_equal(state$model_config, cfg)

    restored <- lab_session_from_list(state)
    expect_equal(restored$cohort_ids, "Cairo_1835")
    expect_equal(restored$model_config, cfg)
  })
})
