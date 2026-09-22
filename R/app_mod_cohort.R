# ------------------------------------------------------------------------------
# app_mod_cohort.R — Shiny module: the current cohort as a strip of chips.
#
# One chip per outbreak in the cohort, each with an `x` to remove it.
# Selection state lives in `lab_session$cohort_ids`; outbreaks are added from
# the Explore panel. `.sparkline_svg()` also lives here and is used by Explore.
#
# 2026-09-22: the thumbnail grid is gone. Choosing from 130 records is the
# Explore panel's job and it has the width for it; this card is now just the
# standing answer to "what is in the cohort right now", which is what a 340px
# sidebar is the right size for. The chip strip and its remove buttons stay.
#
# It reads `outbreaks_all`, not the curated nine: a chip can now be any of the
# 130, and the per-id remove observers below are registered at module mount,
# so anything missing from this table gets a chip whose x does nothing.
# ------------------------------------------------------------------------------

# Hand-rolled SVG sparkline: a filled wash under a 2px line, with a dot on the
# peak. Width/height are the viewBox, not a commitment — the CSS stretches it
# to whatever the tile is.
#
# Two details that are load-bearing rather than decorative:
#
#   * `vector-effect="non-scaling-stroke"`. The SVG is drawn in a 160-unit box
#     and displayed at whatever width the grid hands it, with
#     `preserveAspectRatio="none"` so the series always spans the tile. Without
#     the vector-effect, that scaling stretches the stroke too, so the same
#     1.5px line renders visibly thicker in a wide tile than a narrow one and
#     the grid looks inconsistent for no reason anyone can name.
#   * `currentColor` everywhere. It is what lets a selected (inverted) tile
#     redraw its own sparkline in white without a second code path.
#
# The area is a ~14% wash rather than a saturated block: the line carries the
# shape, the fill only gives it weight against the card. There is deliberately
# no peak marker — under non-uniform scaling a circle renders as an ellipse,
# and a dot that changes shape with the tile width is worse than no dot.
#' Inline SVG sparkline for one outbreak's death series.
#'
#' @param y Numeric vector of counts; `NA`s are dropped.
#' @param width,height Dimensions of the SVG viewBox. The rendered size is
#'   whatever CSS gives it -- these set the coordinate space and the aspect
#'   the path is drawn in.
#' @param ymax Optional common ceiling for the y axis. `NULL` (the default)
#'   scales each series to its own maximum, which makes *shape* comparable
#'   across tiles but not magnitude -- Eyam's 260 deaths and Cairo's 33,532
#'   draw the same height. Passing a shared value turns the grid into small
#'   multiples in the proper sense, at the cost of flattening the small
#'   series to near-nothing. The explorer offers both.
#' @return An `htmltools::HTML()` string, empty for series shorter than two
#'   observed points.
#' @keywords internal
.sparkline_svg <- function(y, width = 160, height = 40, ymax = NULL) {
  y <- y[!is.na(y)]
  if (length(y) < 2L) return(htmltools::HTML(""))
  pad <- 3
  x <- seq_along(y) - 1L
  x_norm <- x / max(x) * (width - 2 * pad) + pad
  y_max <- if (is.null(ymax)) max(y, na.rm = TRUE) else ymax
  if (!is.finite(y_max) || y_max <= 0) y_max <- 1
  # A shared ceiling must not let a taller-than-ceiling series draw outside
  # the box; clamping is the honest failure mode.
  y <- pmin(y, y_max)
  y_norm <- height - pad - (y / y_max) * (height - 2 * pad)
  base <- height - pad
  pts <- paste(round(x_norm, 2), round(y_norm, 2), sep = ",", collapse = " ")
  area <- sprintf("%s,%s %s %s,%s",
                  round(x_norm[1], 2), base, pts,
                  round(x_norm[length(x_norm)], 2), base)
  svg <- sprintf(
    paste0(
      '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" ',
      'viewBox="0 0 %d %d" preserveAspectRatio="none" class="yl-spark">',
      '<polygon class="yl-spark-area" points="%s" fill="currentColor" ',
      'fill-opacity="0.14" stroke="none"/>',
      '<polyline class="yl-spark-line" points="%s" fill="none" ',
      'stroke="currentColor" stroke-width="1.75" stroke-linejoin="round" ',
      'stroke-linecap="round" vector-effect="non-scaling-stroke"/>',
      '</svg>'
    ),
    width, height, width, height, area, pts
  )
  htmltools::HTML(svg)
}

# A single chip showing one selected outbreak with an inline remove button.
.cohort_chip <- function(ns, id, label) {
  shiny::tags$span(
    class = "yl-chip",
    shiny::tags$span(class = "yl-chip-label", label),
    shiny::actionLink(
      inputId = ns(paste0("remove_", id)),
      label = shiny::icon("xmark"),
      class = "yl-chip-x",
      title = paste("Remove", label)
    )
  )
}

#' Cohort module — UI.
#'
#' @param id Module namespace id.
#' @return A `bslib::card()` with the cohort chips.
#' @export
cohort_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Cohort"),
    bslib::card_body(
      shiny::uiOutput(ns("chips"))
    )
  )
}

#' Cohort module — server.
#'
#' Selection state is `lab_session$cohort_ids` (character vector of
#' `outbreak_id`s). The UI is rebuilt whenever that vector changes.
#'
#' @param id Module namespace id.
#' @param lab_session A [LabSession] instance.
#' @param data Long outbreaks tibble. Defaults to the bundled [outbreaks].
#' @return Invisibly, the moduleServer result.
#' @export
cohort_server <- function(id, lab_session, data = NULL) {
  if (is.null(data)) data <- get("outbreaks_all", envir = asNamespace("yersinia"))
  summary_tbl <- outbreak_summary(data)
  series_by_id <- split(data$deaths, data$outbreak_id)

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$chips <- shiny::renderUI({
      ids <- lab_session$cohort_ids
      if (length(ids) == 0) {
        return(shiny::tags$span(
          class = "yl-chips-empty",
          "No outbreaks selected \u2014 pick some in the Explore panel."))
      }
      # outbreak_label() resolves legacy ids on the way through, so a cohort
      # restored from a pre-2026-09 session shows "Eyam 1665" rather than the
      # raw string it was saved as.
      labels <- outbreak_label(ids, data)
      do.call(shiny::tagList,
              Map(function(id, lab) .cohort_chip(ns, id, lab), ids, labels))
    })

    # Remove-chip clicks. Inputs are dynamically named remove_<id> and the
    # observers are registered once at mount, so every id that can appear in
    # a chip needs one here -- including the nine legacy spellings, which a
    # saved session or fit-library entry can still be carrying. Without them
    # a restored cohort renders chips whose x does nothing at all.
    removable <- union(summary_tbl$outbreak_id,
                       get("outbreak_aliases",
                           envir = asNamespace("yersinia"))$legacy_id)
    for (oid_ in removable) {
      local({
        oid <- oid_
        shiny::observeEvent(input[[paste0("remove_", oid)]], {
          lab_session$cohort_ids <- setdiff(
            shiny::isolate(lab_session$cohort_ids), oid)
        })
      })
    }
  })
}
