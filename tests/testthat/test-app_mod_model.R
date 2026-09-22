# model accordion module — scenario + scope edits write to lab_session.

test_that("model_accordion_server initialises widgets from session", {
  lab <- LabSession$new(model_config = list(
    scenario = "didelot",
    shared   = c("beta_h", "kappa"),
    local    = c("K_h")
  ))
  shiny::testServer(
    model_accordion_server, args = list(lab_session = lab),
    {
      # Set the inputs manually to mimic the update*Input calls in mount.
      session$setInputs(scenario = "didelot",
                        shared = c("beta_h", "kappa"),
                        local  = "K_h")
      cfg <- lab$model_config
      expect_equal(cfg$scenario, "didelot")
      expect_setequal(cfg$shared, c("beta_h", "kappa"))
      expect_equal(cfg$local, "K_h")
    }
  )
})

test_that("model_accordion_server reflects scenario changes", {
  lab <- LabSession$new()
  shiny::testServer(
    model_accordion_server, args = list(lab_session = lab),
    {
      session$setInputs(scenario = "modern-estimates",
                        shared = c("beta_r"),
                        local  = character(0))
      expect_equal(lab$model_config$scenario, "modern-estimates")
    }
  )
})
