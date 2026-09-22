# Outbreak explorer module: filters, sorting, card selection, and the refusal
# of unfittable records.

# The flag helper returns a tag list, so assert on the rendered markup.
flags_html <- function(row) paste(vapply(.explore_flags(row),
                                         as.character, character(1)),
                                  collapse = " ")

test_that(".explore_flags badges unfittable, flagged and digitised records", {
  s <- outbreak_summary()
  expect_match(flags_html(s[s$label == "Klaipeda 1710", ]), "impossible")

  eyam <- s[s$label == "Eyam 1665", ]
  expect_match(flags_html(eyam), "high attack rate")
  expect_false(grepl("not fittable", flags_html(eyam)))

  unfit <- s[!s$fittable, ][1, ]
  html <- flags_html(unfit)
  expect_match(html, "not fittable")
  # The reason travels with the badge — a greyed card that can't say why is
  # just a broken card.
  expect_match(html, unfit$unfit_reason, fixed = TRUE)
})

test_that(".explore_flags is empty for a clean, fittable record", {
  s <- outbreak_summary()
  clean <- s[s$fittable & is.na(s$attack_flag) & s$sourcetype == "table", ][1, ]
  expect_equal(length(.explore_flags(clean)), 0L)
})

test_that(".explore_tile marks selection and unfittability", {
  s <- outbreak_summary()
  ns <- shiny::NS("explore")
  fit <- s[s$fittable, ][1, ]
  html <- as.character(.explore_tile(ns, fit, TRUE, c(1, 5, 2)))
  expect_match(html, "yl-tile-selected")
  expect_false(grepl("yl-tile-unfittable", html))
  expect_match(html, "card_click")
  # The tile carries a sparkline, a place and a year, and nothing else --
  # keeping it that way is the point of the design.
  expect_match(html, "polyline")
  expect_match(html, fit$location, fixed = TRUE)
  expect_false(grepl("yl-badge", html))

  unfit <- s[!s$fittable, ][1, ]
  html2 <- as.character(.explore_tile(ns, unfit, FALSE, c(1, 5, 2)))
  expect_match(html2, "yl-tile-unfittable")
  # Still clickable: the detail card is the only thing left to do with it.
  expect_match(html2, "card_click")
  # And the reason is on the tooltip, since it isn't on the tile.
  expect_match(html2, unfit$unfit_reason, fixed = TRUE)
})

test_that("explore_server clicking a fittable tile adds it to the cohort", {
  lab <- LabSession$new()
  shiny::testServer(explore_server, args = list(lab_session = lab), {
    session$setInputs(era = c(1348, 1878), cadence = "daily",
                      type = c("plague mortality", "all-cause mortality"),
                      only_fittable = TRUE, hide_flagged = FALSE,
                      sourcetype = c("table", "graph"),
                      search = "", sort = "year")
    id <- filtered()$outbreak_id[1]
    session$setInputs(card_click = list(id = id, t = 1))
    expect_equal(lab$cohort_ids, id)
    # Clicking again removes it.
    session$setInputs(card_click = list(id = id, t = 2))
    expect_equal(lab$cohort_ids, character(0))
  })
})

test_that("explore_server refuses to select an unfittable tile but focuses it", {
  lab <- LabSession$new()
  shiny::testServer(explore_server, args = list(lab_session = lab), {
    session$setInputs(era = c(1348, 1878), cadence = "monthly",
                      type = c("plague mortality", "all-cause mortality"),
                      only_fittable = FALSE, hide_flagged = FALSE,
                      sourcetype = c("table", "graph"),
                      search = "", sort = "year")
    # Monthly records are never fittable, so this filter is all-unfittable.
    expect_false(any(filtered()$fittable))
    id <- filtered()$outbreak_id[1]
    session$setInputs(card_click = list(id = id, t = 1))
    expect_equal(lab$cohort_ids, character(0))
    expect_equal(focused(), id)
  })
})

test_that("filtering hides tiles without dropping them from the cohort", {
  lab <- LabSession$new()
  shiny::testServer(explore_server, args = list(lab_session = lab), {
    session$setInputs(era = c(1348, 1878), cadence = "daily",
                      type = c("plague mortality", "all-cause mortality"),
                      only_fittable = TRUE, hide_flagged = FALSE,
                      sourcetype = c("table", "graph"),
                      search = "", sort = "year")
    first <- filtered()$outbreak_id[1]
    session$setInputs(card_click = list(id = first, t = 1))
    expect_equal(lab$cohort_ids, first)
    # Narrow the era to something that excludes it. The filter is a view, not
    # a delete.
    session$setInputs(era = c(1870, 1878))
    expect_false(first %in% filtered()$outbreak_id)
    expect_true(first %in% lab$cohort_ids)
  })
})

test_that("filters compose and reset", {
  lab <- LabSession$new()
  shiny::testServer(explore_server, args = list(lab_session = lab), {
    session$setInputs(era = c(1348, 1878),
                      cadence = c("daily", "weekly", "biweekly", "monthly"),
                      type = c("plague mortality", "all-cause mortality"),
                      only_fittable = FALSE, hide_flagged = FALSE,
                      sourcetype = c("table", "graph"),
                      search = "", sort = "year")
    expect_equal(nrow(filtered()), 130L)
    session$setInputs(only_fittable = TRUE)
    expect_equal(nrow(filtered()), 52L)
    session$setInputs(hide_flagged = TRUE)
    expect_true(all(is.na(filtered()$attack_flag)))
    session$setInputs(search = "Barcelona")
    expect_true(all(grepl("Barcelona", filtered()$label)))
    # An empty cadence selection means "nothing", not "everything" — an
    # unchecked-all box that silently showed all 130 would be a lie.
    session$setInputs(search = "", cadence = character(0))
    expect_equal(nrow(filtered()), 0L)
  })
})

test_that("sort orders ascending by year and descending by magnitude", {
  lab <- LabSession$new()
  shiny::testServer(explore_server, args = list(lab_session = lab), {
    session$setInputs(era = c(1348, 1878),
                      cadence = c("daily", "weekly", "biweekly", "monthly"),
                      type = c("plague mortality", "all-cause mortality"),
                      only_fittable = FALSE, hide_flagged = FALSE,
                      sourcetype = c("table", "graph"),
                      search = "", sort = "year")
    expect_false(is.unsorted(filtered()$year))
    session$setInputs(sort = "total_deaths")
    expect_false(is.unsorted(rev(filtered()$total_deaths)))
    # NAs sort last, not first, whichever direction we're going.
    session$setInputs(sort = "population")
    pop <- filtered()$population
    expect_true(all(is.na(pop[(sum(!is.na(pop)) + 1):length(pop)])))
  })
})

test_that("an emptied transcription filter means none, like the others", {
  lab <- LabSession$new()
  shiny::testServer(explore_server, args = list(lab_session = lab), {
    session$setInputs(era = c(1348, 1878),
                      cadence = c("daily", "weekly", "biweekly", "monthly"),
                      type = c("plague mortality", "all-cause mortality"),
                      only_fittable = FALSE, hide_flagged = FALSE,
                      sourcetype = c("table", "graph"),
                      search = "", sort = "year")
    expect_equal(nrow(filtered()), 130L)
    session$setInputs(sourcetype = "table")
    expect_true(all(filtered()$sourcetype == "table"))
    expect_equal(nrow(filtered()), 71L)
    session$setInputs(sourcetype = character(0))
    expect_equal(nrow(filtered()), 0L)
  })
})

test_that("clicking a source filters to it and clicking again clears", {
  lab <- LabSession$new()
  shiny::testServer(explore_server, args = list(lab_session = lab), {
    session$setInputs(era = c(1348, 1878),
                      cadence = c("daily", "weekly", "biweekly", "monthly"),
                      type = c("plague mortality", "all-cause mortality"),
                      only_fittable = FALSE, hide_flagged = FALSE,
                      sourcetype = c("table", "graph"),
                      search = "", sort = "year")
    session$setInputs(source_click = list(source = "Frandsen 2010", t = 1))
    expect_equal(nrow(filtered()), 23L)
    expect_true(all(filtered()$source == "Frandsen 2010"))
    session$setInputs(source_click = list(source = "Frandsen 2010", t = 2))
    expect_equal(nrow(filtered()), 130L)
  })
})

test_that("clicking a place filters to it and clicking again clears", {
  lab <- LabSession$new()
  shiny::testServer(explore_server, args = list(lab_session = lab), {
    session$setInputs(era = c(1348, 1878),
                      cadence = c("daily", "weekly", "biweekly", "monthly"),
                      type = c("plague mortality", "all-cause mortality"),
                      only_fittable = FALSE, hide_flagged = FALSE,
                      sourcetype = c("table", "graph"),
                      search = "", sort = "year")
    session$setInputs(map_click = list(place = "London", t = 1))
    expect_true(all(filtered()$location == "London"))
    expect_equal(nrow(filtered()), 10L)
    session$setInputs(map_click = list(place = "London", t = 2))
    expect_equal(nrow(filtered()), 130L)
  })
})

test_that("shared sparkline scale uses the visible maximum, not the global one", {
  s <- outbreak_summary()
  big <- max(s$peak_deaths)
  small <- s[s$peak_deaths < big / 50, ][1, ]
  own <- as.character(.explore_tile(shiny::NS("e"), small, FALSE,
                                    c(0, small$peak_deaths, 0)))
  shared <- as.character(.explore_tile(shiny::NS("e"), small, FALSE,
                                       c(0, small$peak_deaths, 0),
                                       ymax = big))
  # Under its own scale the series reaches the top of the box; under a shared
  # ceiling 50x larger it barely leaves the baseline. That difference is the
  # entire point of the toggle.
  #
  # Parse x,y in pairs and keep only y: pooling both coordinates makes the
  # minimum the x-padding in every case, which compares equal and passes
  # whatever the scaling does.
  peak_y <- function(h) {
    pts <- regmatches(h, regexpr('(?<=points=")[^"]+', h, perl = TRUE))
    xy <- as.numeric(unlist(strsplit(trimws(pts), "[ ,]+")))
    min(xy[seq(2, length(xy), by = 2)])
  }
  expect_lt(peak_y(own), peak_y(shared))
  expect_equal(peak_y(own), 3)
})

test_that("a series above a shared ceiling is clamped, not drawn outside", {
  svg <- as.character(.sparkline_svg(c(0, 100, 0), ymax = 10))
  pts <- unlist(strsplit(regmatches(svg, regexpr("(?<=points=\")[^\"]+", svg,
                                                 perl = TRUE)), "[ ,]"))
  expect_true(all(as.numeric(pts) >= 0))
})

test_that("the table view carries the filtered set with provenance columns", {
  lab <- LabSession$new()
  shiny::testServer(explore_server, args = list(lab_session = lab), {
    session$setInputs(era = c(1348, 1878),
                      cadence = c("daily", "weekly", "biweekly", "monthly"),
                      type = c("plague mortality", "all-cause mortality"),
                      only_fittable = FALSE, hide_flagged = FALSE,
                      sourcetype = c("table", "graph"),
                      search = "", sort = "year", scale = "own")
    df <- table_df()
    expect_equal(nrow(df), 130L)
    expect_true(all(c("Source", "Transcribed from", "Unfittable because",
                      "Peak day-of-year", "Krauer id") %in% names(df)))
    session$setInputs(only_fittable = TRUE)
    expect_equal(nrow(table_df()), 52L)
  })
})
