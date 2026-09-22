# ------------------------------------------------------------------------------
# app_explore_views.R — the explorer's map and timeline.
#
# Both are hand-rolled inline SVG, for the same reasons the tiles are: no extra
# dependency, no plot round-trip, and they inherit the `--yl-*` tokens so they
# match the grid without a second palette.
#
# WHY NOT LEAFLET
#
# A tile-server map would need network at runtime, which the lab does not
# otherwise require and which an offline or firewalled session would not have.
# The coastline is instead vendored: `inst/extdata/europe-land-110m.path` is a
# pre-projected SVG path built once from Natural Earth's 110m land polygons
# (via the `world-atlas` TopoJSON), clipped to the catalogue's own extent and
# simplified to 10 rings / ~6.6 kB. It ships with the package and renders
# offline. `data-raw/build-land-path.R` regenerates it.
#
# WHAT EACH VIEW IS FOR
#
# The map answers "where", which for this catalogue is really "how lopsided is
# it" — 32 of the 130 records are English and 17 Danish, and that is a fact
# about which archives survived and got transcribed, not about where plague
# was. The timeline answers "when", and shows the same thing in the other
# axis: the catalogue thins to almost nothing before 1500 and after 1750.
# Neither is decoration; both are the caveat you need before fitting anything
# cohort-wide.
# ------------------------------------------------------------------------------

# Projection box. Equirectangular with a 45-degree standard parallel — the
# catalogue spans 30-59 N, so the cosine correction at 45 keeps the middle of
# the range close to true and nothing is badly stretched. The bounds are the
# data's own extent (lon -9.1 to 46.6, lat 30.0 to 59.4) plus a margin.
.explore_geo <- list(lon0 = -12, lon1 = 50, lat0 = 28, lat1 = 62,
                     w = 1000, h = 776)

# Pre-projected coastline, read once per session. Missing file is not fatal:
# the map still plots its points, just without a coastline under them.
.explore_land_path <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    f <- system.file("extdata", "europe-land-110m.path", package = "yersinia")
    cache <<- if (nzchar(f) && file.exists(f)) {
      paste(readLines(f, warn = FALSE), collapse = "")
    } else ""
    cache
  }
})

#' Project longitude/latitude into the explorer map's SVG viewBox.
#'
#' @param lon,lat Numeric vectors of the same length, in degrees.
#' @return A list with numeric `x` and `y` in viewBox units.
#' @keywords internal
.explore_project <- function(lon, lat) {
  g <- .explore_geo
  list(x = (lon - g$lon0) / (g$lon1 - g$lon0) * g$w,
       y = (g$lat1 - lat) / (g$lat1 - g$lat0) * g$h)
}

# Escape a string for use in an SVG/HTML attribute.
.esc <- function(x) htmltools::htmlEscape(x, attribute = TRUE)

# A clickable mark in an SVG view.
#
# `onclick` on a bare <circle> is a mouse-only control: not reachable by
# keyboard, no focus ring, and nothing for a screen reader to announce. Every
# interactive mark therefore carries tabindex/role/aria-label and handles
# Enter and Space, which is what a <button> would have given us for free and
# what the tile grid does get for free.
.svg_mark <- function(shape_attrs, input_id, payload, label, class) {
  set <- sprintf("Shiny.setInputValue('%s', {%s, t: Math.random()})",
                 input_id, payload)
  sprintf(
    paste0('<circle class="%s" %s tabindex="0" role="button" ',
           'aria-label="%s" onclick="%s" ',
           'onkeydown="if(event.key===\'Enter\'||event.key===\' \')',
           '{event.preventDefault();%s}">',
           '<title>%s</title></circle>'),
    class, shape_attrs, .esc(label), set, set, .esc(label))
}

#' Explorer map — one circle per place, area proportional to its record count.
#'
#' Area, not radius: a place with four outbreaks should look four times the
#' one with one, and scaling the radius by the count would make it sixteen.
#'
#' @param places Data frame with `location`, `lat`, `lon`, `n`, `visible`,
#'   `selected`.
#' @param ns Module namespace function.
#' @param focus_place Currently filtered place, or `NULL`.
#' @return An `htmltools::HTML()` SVG string.
#' @keywords internal
.explore_map_svg <- function(places, ns, focus_place = NULL) {
  g <- .explore_geo
  p <- .explore_project(as.numeric(places$lon), as.numeric(places$lat))
  r <- 3.6 * sqrt(pmax(1, places$n))
  # Three states, and colour is not the only thing separating them: filtered
  # -out marks are hollow (a fill channel) and selected ones carry a ring (a
  # stroke channel), so the view still reads with no colour vision at all.
  cls <- ifelse(!places$visible, "yl-geo-dot yl-geo-dim",
         ifelse(places$selected, "yl-geo-dot yl-geo-sel", "yl-geo-dot"))
  cls <- ifelse(!is.null(focus_place) & places$location %in% focus_place,
                paste(cls, "yl-geo-focus"), cls)
  # Biggest first, so a place with many records never hides a neighbour with
  # one underneath it.
  ord <- order(-r)
  dots <- .svg_mark(
    sprintf('cx="%.1f" cy="%.1f" r="%.1f"', p$x[ord], p$y[ord], r[ord]),
    ns("map_click"),
    sprintf("place: '%s'", .esc(places$location[ord])),
    sprintf("%s \u2014 %d outbreak%s", places$location[ord], places$n[ord],
            ifelse(places$n[ord] == 1, "", "s")),
    cls[ord])
  htmltools::HTML(sprintf(
    paste0('<svg xmlns="http://www.w3.org/2000/svg" class="yl-map" ',
           'viewBox="0 0 %d %d" preserveAspectRatio="xMidYMid meet">',
           '<path class="yl-map-land" d="%s" fill-rule="evenodd"/>%s</svg>'),
    g$w, g$h, .explore_land_path(), paste(dots, collapse = "")
  ))
}

#' Explorer timeline — one dot per outbreak, stacked where years collide.
#'
#' A beeswarm rather than a histogram of counts: the unit the user selects is
#' an outbreak, so the mark should be one too. Bars would show the same
#' distribution but could not be clicked to pick a record out of it.
#'
#' @param s Summary table with `outbreak_id`, `year`, `label`, `visible`,
#'   `selected`, `fittable`.
#' @param ns Module namespace function.
#' @return An `htmltools::HTML()` SVG string.
#' @keywords internal
.explore_timeline_svg <- function(s, ns) {
  y0 <- 1340; y1 <- 1880
  w <- 1000; rr <- 5.2
  # Sort by year up front: the stack level is then assigned in year order
  # within each bin, so the column grows bottom-up in reading order and the
  # layout is stable between renders.
  s <- s[order(s$year, s$outbreak_id), , drop = FALSE]
  xs <- (s$year - y0) / (y1 - y0) * (w - 24) + 12
  # Stack within 6-year bins so touching dots never overlap.
  bin <- floor((s$year - y0) / 6)
  k <- stats::ave(seq_len(nrow(s)), bin, FUN = seq_along) - 1L

  # Columns wrap at CAP rather than growing without limit, because one bin
  # dwarfs every other: 30 of the 130 records fall in 1708-1713 (the Great
  # Northern War plague), 15 of them in 1711 alone. Letting that single column
  # set the height squashes five centuries into a strip at the bottom of the
  # view. Wrapping spends horizontal space instead, which the timeline has to
  # give, and the cluster still reads as the densest thing on the chart.
  CAP <- 8L
  step <- 2 * rr + 1.4
  dx <- 2 * rr * 0.72
  ncol <- stats::ave(k, bin, FUN = function(z) rep(ceiling(length(z) / CAP),
                                                   length(z)))
  lvl <- k %% CAP
  col <- k %/% CAP
  xs <- xs + (col - (ncol - 1) / 2) * dx
  # as.integer, not just arithmetic: `h` feeds %d format slots below, and a
  # double there is a runtime error rather than a rounding surprise.
  h <- as.integer(ceiling(24 + (max(lvl) + 1) * step + 20))
  base <- h - 20
  ys <- base - lvl * step
  cls <- ifelse(!s$visible, "yl-tl-dot yl-tl-dim",
         ifelse(s$selected, "yl-tl-dot yl-tl-sel",
         ifelse(!s$fittable, "yl-tl-dot yl-tl-unfit", "yl-tl-dot")))
  dots <- .svg_mark(
    sprintf('cx="%.1f" cy="%.1f" r="%.1f"', xs, ys, rr),
    ns("card_click"), sprintf("id: '%s'", .esc(s$outbreak_id)),
    s$label, cls)
  ticks <- seq(1350, 1850, by = 50)
  tx <- (ticks - y0) / (y1 - y0) * (w - 24) + 12
  grid <- sprintf(
    '<line class="yl-tl-grid" x1="%.1f" y1="8" x2="%.1f" y2="%.1f"/>',
    tx, tx, base + 4)
  labs <- sprintf(
    '<text class="yl-tl-lab" x="%.1f" y="%d" text-anchor="middle">%d</text>',
    tx, h - 4, ticks)
  htmltools::HTML(sprintf(
    paste0('<svg xmlns="http://www.w3.org/2000/svg" class="yl-timeline" ',
           'viewBox="0 0 %d %d" preserveAspectRatio="xMidYMax meet">',
           '%s<line class="yl-tl-axis" x1="8" y1="%.1f" x2="%d" y2="%.1f"/>',
           '%s%s</svg>'),
    w, h, paste(grid, collapse = ""), base + 4, w - 8, base + 4,
    paste(dots, collapse = ""), paste(labs, collapse = "")
  ))
}


#' Explorer phase view — peak day-of-year against latitude.
#'
#' The descriptive form of the thermal hypothesis. Each outbreak is placed at
#' the day of year its largest observed count falls on (x) and the latitude of
#' its place (y). Across the catalogue the two correlate at **r = 0.64**:
#' north of 52 N the mean peak is day 242 (late August), south of 45 N it is
#' day 180 (late June) — a 62-day gradient, visible without fitting anything.
#'
#' Cartesian rather than a circular calendar. A ring is prettier and honours
#' the wrap-around, but the whole point here is to read a *gradient against
#' latitude*, and a ring has nowhere to put the second axis. Nothing in this
#' catalogue peaks in January or February, so the seam costs us nothing.
#'
#' Two cautions the view should be read with, both visible in it: the northern
#' cluster is largely Frandsen's 23 Baltic records, one source digitised from
#' figures, so the gradient is not 130 independent observations of it; and a
#' monthly record's "peak day" is the first of its peak month, which is a
#' coarser quantity than a daily record's.
#'
#' @param s Summary table with `peak_doy`, `lat`, `label`, `visible`,
#'   `selected`, `fittable`, `total_deaths`.
#' @param ns Module namespace function.
#' @return An `htmltools::HTML()` SVG string.
#' @keywords internal
.explore_phase_svg <- function(s, ns) {
  s <- s[!is.na(s$peak_doy) & !is.na(s$lat), , drop = FALSE]
  w <- 1000; h <- 420
  ml <- 58; mr <- 16; mt <- 14; mb <- 34
  lat <- as.numeric(s$lat)
  lat0 <- 28; lat1 <- 62
  px <- ml + (s$peak_doy - 1) / 365 * (w - ml - mr)
  py <- mt + (lat1 - lat) / (lat1 - lat0) * (h - mt - mb)
  # Area by deaths, like the map: a big outbreak should read as more ink, and
  # radius-scaling would exaggerate it quadratically.
  r <- 3 + 5.5 * sqrt(pmax(0, s$total_deaths) / max(1, max(s$total_deaths)))
  cls <- ifelse(!s$visible, "yl-ph-dot yl-ph-dim",
         ifelse(s$selected, "yl-ph-dot yl-ph-sel",
         ifelse(!s$fittable, "yl-ph-dot yl-ph-unfit", "yl-ph-dot")))
  ord <- order(-r)
  dots <- .svg_mark(
    sprintf('cx="%.1f" cy="%.1f" r="%.1f"', px[ord], py[ord], r[ord]),
    ns("card_click"), sprintf("id: '%s'", .esc(s$outbreak_id[ord])),
    sprintf("%s \u2014 peaks %s, %.1f\u00b0N",
            s$label[ord],
            format(as.Date(s$peak_doy[ord] - 1, origin = "2001-01-01"),
                   "%e %b"),
            lat[ord]),
    cls[ord])

  # Month gridlines, labelled at the midpoint of each month rather than the
  # boundary, so a label sits under the band it names.
  starts <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366)
  gx <- ml + (starts - 1) / 365 * (w - ml - mr)
  grid <- sprintf(
    '<line class="yl-ax-grid" x1="%.1f" y1="%d" x2="%.1f" y2="%.1f"/>',
    gx, mt, gx, h - mb)
  mid <- (utils::head(gx, -1) + gx[-1]) / 2
  mlab <- sprintf(
    '<text class="yl-ax-lab" x="%.1f" y="%d" text-anchor="middle">%s</text>',
    mid, h - mb + 20, substr(month.abb, 1, 1))
  lats <- seq(30, 60, by = 10)
  ly <- mt + (lat1 - lats) / (lat1 - lat0) * (h - mt - mb)
  lgrid <- sprintf(
    '<line class="yl-ax-grid" x1="%d" y1="%.1f" x2="%d" y2="%.1f"/>',
    ml, ly, w - mr, ly)
  llab <- sprintf(
    paste0('<text class="yl-ax-lab" x="%d" y="%.1f" text-anchor="end" ',
           'dominant-baseline="middle">%d\u00b0N</text>'),
    ml - 8, ly, lats)
  htmltools::HTML(sprintf(
    paste0('<svg xmlns="http://www.w3.org/2000/svg" class="yl-phase" ',
           'viewBox="0 0 %d %d" preserveAspectRatio="xMidYMid meet" ',
           'role="img" aria-label="%s">%s%s%s%s%s</svg>'),
    w, h,
    .esc(paste("Peak day of year against latitude for", nrow(s),
               "outbreaks. Northern outbreaks peak later:",
               "the two correlate at r = 0.64.")),
    paste(grid, collapse = ""), paste(lgrid, collapse = ""),
    paste(dots, collapse = ""), paste(mlab, collapse = ""),
    paste(llab, collapse = "")))
}

#' Explorer provenance view — outbreaks per source, as a bar list.
#'
#' HTML rather than SVG: these are labelled horizontal bars, which CSS grid
#' does better than hand-placed `<rect>`s, and the labels need to wrap and
#' ellipsis like text because that is what they are.
#'
#' Why this view exists at all. The catalogue's 130 records come from **45
#' sources**, and they are not evenly spread: Frandsen (2010) alone supplies
#' 23 of them, every one between 1708 and 1713, every one digitised from a
#' graph. The top five sources supply 59 records between them — 45% of the
#' catalogue. Among the 52 *fittable* records there are only 24 distinct
#' sources, and Ferran (1907) is 11 of them.
#'
#' That matters for fitting, not just for tidiness. A cohort likelihood
#' multiplies per-outbreak terms as though each record were an independent
#' observation of plague. Records sharing a source share a transcriber, a set
#' of editorial decisions about what counted as a plague death, and often one
#' archive — so a cohort drawn mostly from one citation has far less
#' independent evidence in it than its record count suggests. The view makes
#' that visible before the fit rather than after it.
#'
#' @param s Summary table with `source`, `visible`, `selected`.
#' @param ns Module namespace function.
#' @param focus_source Currently filtered source, or `NULL`.
#' @param top How many sources to list individually.
#' @return A `shiny::tagList()`.
#' @keywords internal
.explore_sources_ui <- function(s, ns, focus_source = NULL, top = 12L) {
  tab <- as.data.frame(table(s$source), stringsAsFactors = FALSE)
  names(tab) <- c("source", "n")
  tab <- tab[order(-tab$n, tab$source), , drop = FALSE]
  shown <- utils::head(tab, top)
  rest <- tab[-seq_len(nrow(shown)), , drop = FALSE]
  vis_n <- vapply(shown$source,
                  function(x) sum(s$visible & s$source == x), integer(1))
  sel_n <- vapply(shown$source,
                  function(x) sum(s$selected & s$source == x), integer(1))
  wmax <- max(shown$n)
  rows <- lapply(seq_len(nrow(shown)), function(i) {
    src <- shown$source[i]
    on <- !is.null(focus_source) && identical(focus_source, src)
    shiny::tags$button(
      type = "button",
      class = paste(c("yl-src-row", if (on) "yl-src-on"), collapse = " "),
      title = sprintf("%s \u2014 %d outbreak%s%s", src, shown$n[i],
                      if (shown$n[i] == 1) "" else "s",
                      if (sel_n[[i]] > 0)
                        sprintf(", %d in the cohort", sel_n[[i]]) else ""),
      onclick = sprintf(
        "Shiny.setInputValue('%s', {source: '%s', t: Math.random()})",
        ns("source_click"), .esc(src)),
      shiny::tags$span(class = "yl-src-name", src),
      shiny::tags$span(
        class = "yl-src-bar",
        # Two nested fills: the full bar is the source's whole contribution,
        # the inner one how much of it survives the current filters. One bar
        # then answers both "how big is this source" and "how much of it am I
        # looking at".
        shiny::tags$span(class = "yl-src-fill",
                         style = sprintf("width:%.1f%%", 100 * shown$n[i] / wmax)),
        shiny::tags$span(class = "yl-src-fill-vis",
                         style = sprintf("width:%.1f%%", 100 * vis_n[[i]] / wmax))
      ),
      shiny::tags$span(class = "yl-src-n", shown$n[i])
    )
  })
  if (nrow(rest) > 0) {
    rows <- c(rows, list(shiny::tags$div(
      class = "yl-src-rest",
      sprintf("+ %d more source%s contributing %d outbreak%s",
              nrow(rest), if (nrow(rest) == 1) "" else "s",
              sum(rest$n), if (sum(rest$n) == 1) "" else "s"))))
  }
  shiny::tags$div(class = "yl-src-list", do.call(shiny::tagList, rows))
}

#' One-line provenance warning for a selected cohort.
#'
#' Returns `NULL` when the cohort is empty, has fewer than two outbreaks, or
#' is spread across enough sources to be unremarkable.
#'
#' @param s Summary table for the selected outbreaks only.
#' @return A `shiny::tags$div()` or `NULL`.
#' @keywords internal
.explore_provenance_note <- function(s) {
  if (nrow(s) < 2) return(NULL)
  tab <- sort(table(s$source), decreasing = TRUE)
  top_src <- names(tab)[1]; top_n <- as.integer(tab[1])
  frac <- top_n / nrow(s)
  # Two thirds from one citation is the point where "several outbreaks" stops
  # being a fair description of the evidence.
  if (frac < 2/3) return(NULL)
  shiny::tags$div(
    class = "yl-prov-note",
    shiny::icon("circle-info"),
    sprintf(paste("%d of the %d outbreaks selected come from one source",
                  "(%s). They share a transcriber and an editorial view of",
                  "what counted as a plague death, so the cohort carries",
                  "less independent evidence than its size suggests."),
            top_n, nrow(s), top_src)
  )
}
