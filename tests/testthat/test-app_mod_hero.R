# hero plot module — empty state, cohort-only, cohort + fit.

test_that(".hero_plot renders an empty-state when data is NULL", {
  p <- .hero_plot(NULL)
  expect_s3_class(p, "ggplot")
})

test_that(".hero_plot renders data-only when no posterior", {
  d <- cohort_data("Eyam_1665")
  p <- .hero_plot(d, posterior_long = NULL)
  expect_s3_class(p, "ggplot")
  # No geom_line layer when no posterior.
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_false("GeomLine" %in% geoms)
  expect_true("GeomPoint" %in% geoms)
})

test_that(".hero_plot adds posterior lines when supplied", {
  d <- cohort_data("Eyam_1665")
  pp <- tibble::tibble(draw = 1, group = "Eyam_1665",
                       time = seq_len(10), mu = runif(10))
  p <- .hero_plot(d, posterior_long = pp)
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomLine" %in% geoms)
})

test_that("hero_server renders the plot for an empty session", {
  lab <- LabSession$new()
  shiny::testServer(hero_server, args = list(lab_session = lab), {
    session$flushReact()
    expect_silent(output$plot)
  })
})
