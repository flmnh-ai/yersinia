# cohort module — chips + thumbnail grid + click-to-toggle.

test_that(".sparkline_svg produces a polyline SVG for a deaths series", {
  svg <- .sparkline_svg(c(0, 1, 5, 3, 0))
  expect_s3_class(svg, "html")
  expect_match(as.character(svg), "polyline")
  expect_match(as.character(svg), "<svg")
})

test_that(".sparkline_svg returns empty HTML for too-short series", {
  expect_equal(as.character(.sparkline_svg(numeric(0))), "")
  expect_equal(as.character(.sparkline_svg(c(NA, NA))), "")
})

test_that("cohort_server tile click adds an outbreak", {
  lab <- LabSession$new()
  shiny::testServer(cohort_server, args = list(lab_session = lab), {
    session$setInputs(tile_click = list(id = "Eyam_1665", t = 1))
    expect_equal(lab$cohort_ids, "Eyam_1665")
  })
})

test_that("cohort_server tile click on already-selected outbreak removes it", {
  lab <- LabSession$new(cohort_ids = c("Eyam_1665", "Givry_1348"))
  shiny::testServer(cohort_server, args = list(lab_session = lab), {
    session$setInputs(tile_click = list(id = "Eyam_1665", t = 1))
    expect_equal(lab$cohort_ids, "Givry_1348")
  })
})

test_that("cohort_server remove_<id> input drops the outbreak", {
  lab <- LabSession$new(cohort_ids = c("Eyam_1665", "Givry_1348"))
  shiny::testServer(cohort_server, args = list(lab_session = lab), {
    session$setInputs(remove_Eyam_1665 = 1)
    expect_equal(lab$cohort_ids, "Givry_1348")
  })
})
