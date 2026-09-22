# Explorer map and timeline: projection, mark geometry, and the shared
# in-view / selected / filtered-out language.

vis_tbl <- function(visible = TRUE, selected = FALSE) {
  s <- outbreak_summary()
  s$visible <- visible
  s$selected <- selected
  s
}

test_that(".explore_project puts the catalogue inside the viewBox", {
  s <- outbreak_summary()
  p <- .explore_project(as.numeric(s$lon), as.numeric(s$lat))
  g <- .explore_geo
  expect_true(all(p$x >= 0 & p$x <= g$w))
  expect_true(all(p$y >= 0 & p$y <= g$h))
  # North is up and east is right — a flipped y is the classic screen-vs-map
  # mistake and it looks plausible until you notice Egypt above Sweden.
  north <- which.max(as.numeric(s$lat))
  south <- which.min(as.numeric(s$lat))
  expect_lt(p$y[north], p$y[south])
  east <- which.max(as.numeric(s$lon))
  west <- which.min(as.numeric(s$lon))
  expect_gt(p$x[east], p$x[west])
})

test_that("the vendored coastline is present and is a closed path", {
  d <- .explore_land_path()
  expect_true(nzchar(d))
  expect_match(d, "^M")
  # Every subpath closes, or the fill leaks across the map.
  expect_equal(lengths(regmatches(d, gregexpr("M", d))),
               lengths(regmatches(d, gregexpr("Z", d))))
})

test_that(".explore_map_svg scales place marks by area, not radius", {
  places <- data.frame(
    location = c("One", "Four"), lat = c(45, 45), lon = c(0, 10),
    n = c(1, 4), visible = TRUE, selected = FALSE, stringsAsFactors = FALSE)
  html <- as.character(.explore_map_svg(places, shiny::NS("e")))
  r <- as.numeric(regmatches(html, gregexpr('(?<=r=")[0-9.]+', html,
                                            perl = TRUE))[[1]])
  # Four records should cover four times the ink, so the radius doubles.
  expect_equal(max(r) / min(r), 2, tolerance = 0.01)
  expect_match(html, "map_click")
})

test_that("map and timeline dim filtered-out records rather than dropping them", {
  s <- vis_tbl(visible = FALSE)
  tl <- as.character(.explore_timeline_svg(s, shiny::NS("e")))
  # All 130 are still drawn — the distribution has to stay readable through
  # a filter, otherwise the view stops being a picture of the catalogue.
  expect_equal(length(gregexpr("<circle", tl)[[1]]), nrow(s))
  expect_match(tl, "yl-tl-dim")

  places <- data.frame(location = "X", lat = 45, lon = 0, n = 1,
                       visible = FALSE, selected = FALSE,
                       stringsAsFactors = FALSE)
  expect_match(as.character(.explore_map_svg(places, shiny::NS("e"))),
               "yl-geo-dim")
})

test_that("timeline marks selection and unfittability distinctly", {
  s <- vis_tbl()
  s$selected <- s$outbreak_id == s$outbreak_id[1]
  tl <- as.character(.explore_timeline_svg(s, shiny::NS("e")))
  expect_match(tl, "yl-tl-sel")
  expect_match(tl, "yl-tl-unfit")
  # Timeline dots reuse the tile's own click input, so a dot and a tile are
  # the same action rather than two code paths that can drift apart.
  expect_match(tl, "card_click")
})

test_that("timeline stacks colliding years instead of overplotting them", {
  s <- vis_tbl()
  tl <- as.character(.explore_timeline_svg(s, shiny::NS("e")))
  cy <- as.numeric(regmatches(tl, gregexpr('(?<=cy=")[0-9.]+', tl,
                                           perl = TRUE))[[1]])
  # London alone appears 10 times; if nothing stacked, every dot would share
  # one baseline.
  expect_gt(length(unique(round(cy, 1))), 1L)
})

test_that("place names with quotes or accents survive into the SVG", {
  places <- data.frame(
    location = "Condé-sur-Noireau", lat = 48.8, lon = -0.55, n = 1,
    visible = TRUE, selected = FALSE, stringsAsFactors = FALSE)
  html <- as.character(.explore_map_svg(places, shiny::NS("e")))
  expect_match(html, "Cond", fixed = TRUE)
  expect_false(grepl("<script", html, fixed = TRUE))
})

# ---- Provenance -------------------------------------------------------------

test_that("the source bar list ranks by count and accounts for every record", {
  s <- vis_tbl()
  html <- as.character(.explore_sources_ui(s, shiny::NS("e"), top = 12L))
  # Frandsen 2010 is the largest single contributor, at 23 of the 130.
  expect_match(html, "Frandsen 2010", fixed = TRUE)
  expect_match(html, "source_click")
  # The tail is stated rather than dropped: 45 sources, 12 listed.
  expect_match(html, "more source")
  n_listed <- length(gregexpr("yl-src-row", html)[[1]])
  expect_equal(n_listed, 12L)
})

test_that("source bars show filtered-out records as unfilled track", {
  s <- vis_tbl(visible = FALSE)
  html <- as.character(.explore_sources_ui(s, shiny::NS("e")))
  # Nothing visible -> every "visible" fill is zero width, but the outer
  # fill still shows the source's full size.
  expect_match(html, 'class="yl-src-fill-vis" style="width:0.0%"', fixed = TRUE)
  expect_match(html, "yl-src-fill\" style=\"width:100.0%", fixed = TRUE)
})

test_that("provenance note fires only for source-concentrated cohorts", {
  s <- outbreak_summary()
  frandsen <- s[s$source == "Frandsen 2010", ][1:4, ]
  note <- as.character(.explore_provenance_note(frandsen))
  expect_match(note, "one source")
  expect_match(note, "Frandsen 2010", fixed = TRUE)
  expect_match(note, "less independent evidence")

  # A cohort spread across sources gets no note.
  spread <- s[!duplicated(s$source), ][1:5, ]
  expect_null(.explore_provenance_note(spread))
  # Nor does a single outbreak: one record from one source is not a warning.
  expect_null(.explore_provenance_note(frandsen[1, ]))
  expect_null(.explore_provenance_note(s[0, ]))
})

test_that("the catalogue's source concentration is what the note assumes", {
  # These numbers are asserted because the copy in the UI quotes them, and a
  # data refresh that moves them should fail here rather than silently make
  # the interface lie.
  s <- outbreak_summary()
  expect_equal(length(unique(s$source)), 45L)
  expect_equal(sum(s$source == "Frandsen 2010"), 23L)
  # Frandsen's 23 are one epidemic wave, digitised from figures, and all but
  # one are unfittable -- which is why the timeline spike is not the
  # opportunity it looks like.
  fr <- s[s$source == "Frandsen 2010", ]
  expect_true(all(fr$year >= 1708 & fr$year <= 1713))
  expect_true(all(fr$sourcetype == "graph"))
  expect_equal(sum(fr$fittable), 1L)
  # The fittable subset is thinner in sources than in records.
  expect_equal(length(unique(s$source[s$fittable])), 24L)
})

# ---- Seasonal phase ---------------------------------------------------------

test_that("peak day-of-year exists for every record and tracks latitude", {
  s <- outbreak_summary()
  expect_false(any(is.na(s$peak_doy)))
  expect_true(all(s$peak_doy >= 1 & s$peak_doy <= 366))
  # The signal the phase view exists to show. Asserted because the view's own
  # label quotes the number.
  expect_equal(round(cor(s$peak_doy, as.numeric(s$lat)), 2), 0.64)
  north <- s$peak_doy[as.numeric(s$lat) > 52]
  south <- s$peak_doy[as.numeric(s$lat) < 45]
  expect_gt(mean(north) - mean(south), 40)
})

test_that(".explore_phase_svg plots every record inside its axes", {
  s <- vis_tbl()
  html <- as.character(.explore_phase_svg(s, shiny::NS("e")))
  expect_equal(length(gregexpr("<circle", html)[[1]]), nrow(s))
  cx <- as.numeric(regmatches(html, gregexpr('(?<=cx=")[0-9.]+', html,
                                             perl = TRUE))[[1]])
  cy <- as.numeric(regmatches(html, gregexpr('(?<=cy=")[0-9.]+', html,
                                             perl = TRUE))[[1]])
  expect_true(all(cx >= 58 & cx <= 1000))
  expect_true(all(cy >= 14 & cy <= 420 - 34))
  # Latitude increases upward, as on the map.
  expect_match(html, "60°N")
  # The whole plot carries one accessible summary, since 130 individually
  # labelled dots are not a description of anything.
  expect_match(html, 'role="img"')
  expect_match(html, "r = 0.64", fixed = TRUE)
})

# ---- Accessibility ----------------------------------------------------------

test_that("interactive marks are keyboard-operable and named", {
  s <- vis_tbl()
  for (html in list(as.character(.explore_timeline_svg(s, shiny::NS("e"))),
                    as.character(.explore_phase_svg(s, shiny::NS("e"))))) {
    # onclick on a bare <circle> is a mouse-only control; these need to be
    # reachable and activatable without one.
    expect_match(html, 'tabindex="0"')
    expect_match(html, 'role="button"')
    expect_match(html, "aria-label=")
    expect_match(html, "event.key==='Enter'", fixed = TRUE)
    expect_match(html, "preventDefault", fixed = TRUE)
  }
})

test_that("mark states differ by more than hue", {
  s <- vis_tbl()
  s$selected <- s$outbreak_id == s$outbreak_id[1]
  s$visible[2] <- FALSE
  html <- as.character(.explore_phase_svg(s, shiny::NS("e")))
  # Selected gets a ring, filtered-out goes hollow. Both are encoded as
  # classes here; the CSS carries the stroke/fill that makes them readable
  # without colour vision.
  expect_match(html, "yl-ph-sel")
  expect_match(html, "yl-ph-dim")
})
