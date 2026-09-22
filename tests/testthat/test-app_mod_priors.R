# priors accordion module — fitted-set reconciliation + per-param edits.

test_that(".prior_shorthand renders Uniform/Normal params", {
  expect_match(.prior_shorthand(list(family = "Uniform",
                                     params = list(min = 0, max = 1))),
               "^Uniform\\(0, 1\\)$")
  expect_match(.prior_shorthand(list(family = "Normal",
                                     params = list(mean = 0, sd = 1))),
               "^Normal\\(0, 1\\)$")
  expect_equal(.prior_shorthand(NULL), "—")
})

test_that("priors_accordion_server reconciles priors to the fitted set", {
  lab <- LabSession$new(model_config = list(
    scenario = "historical",
    shared   = c("beta_h", "rho"),
    local    = character(0)
  ))
  shiny::testServer(
    priors_accordion_server, args = list(lab_session = lab),
    {
      session$flushReact()
      pr <- lab$priors
      expect_setequal(names(pr), c("beta_h", "rho"))
      expect_equal(pr$beta_h$family, "Uniform")
    }
  )
})

test_that("priors_accordion_server drops priors when fitted set shrinks", {
  lab <- LabSession$new(
    model_config = list(scenario = "historical",
                        shared = c("beta_h", "rho"), local = character(0)),
    priors = list(
      beta_h = list(family = "Uniform", params = list(min = 0, max = 1)),
      rho    = list(family = "Uniform", params = list(min = 0.5, max = 6)),
      ghost  = list(family = "Uniform", params = list(min = 0, max = 1))
    )
  )
  shiny::testServer(
    priors_accordion_server, args = list(lab_session = lab),
    {
      session$flushReact()
      expect_setequal(names(lab$priors), c("beta_h", "rho"))
    }
  )
})
