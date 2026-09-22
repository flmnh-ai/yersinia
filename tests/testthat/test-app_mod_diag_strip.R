# diagnostics strip module — empty state hidden, populated chips render.

test_that("diag_strip_server renders empty-state hint when no fit", {
  lab <- LabSession$new()
  shiny::testServer(diag_strip_server, args = list(lab_session = lab), {
    session$flushReact()
    html <- as.character(output$chips$html %||% output$chips %||% "")
    expect_match(html, "run a pilot")
  })
})

test_that(".diag_plot_height scales with parameter count", {
  # row_px was raised 110 -> 150: at 110 there was no room for a strip label,
  # tick values and an axis title stacked in one panel.
  expect_equal(.diag_plot_height(1), 150)
  expect_equal(.diag_plot_height(3), 150)
  expect_equal(.diag_plot_height(4), 300)
  # Whatever the constant, the shape of the scaling is the contract: three
  # panels per row, one row's worth of height per started row.
  expect_equal(.diag_plot_height(7), 3 * .diag_plot_height(1))
})

test_that(".diag_chip_class maps severities to CSS classes", {
  expect_match(.diag_chip_class("alert"), "yl-diag-alert")
  expect_match(.diag_chip_class("warn"),  "yl-diag-warn")
  expect_match(.diag_chip_class("info"),  "yl-diag-info")
  expect_match(.diag_chip_class("anything-else"), "yl-diag-chip")
})

test_that("diag_strip_server renders chips after a tiny pilot", {
  samples <- shiny::isolate({
    lab <- LabSession$new(
      cohort_ids = "Eyam_1665",
      model_config = list(scenario = "historical",
                          shared = c("beta_r", "kappa"),
                          local  = character(0))
    )
    setup <- lab_fit_assemble(lab)
    lab_fit_run(setup, n_chains = 1L, n_iter = 50L)
  })
  lab <- LabSession$new(
    cohort_ids = "Eyam_1665",
    model_config = list(scenario = "historical",
                        shared = c("beta_r", "kappa"),
                        local  = character(0)),
    fit_state = list(status = "complete", samples = samples, n_chains = 1L,
                     n_iter = 50L)
  )
  shiny::testServer(diag_strip_server, args = list(lab_session = lab), {
    session$flushReact()
    html <- as.character(output$chips$html %||% output$chips)
    expect_true(grepl("yl-diag-chip", html))
  })
})
