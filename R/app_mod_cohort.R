# ------------------------------------------------------------------------------
# app_mod_cohort.R — Shiny module: cohort selection as chips + thumbnail grid.
#
# Replaces the old DT-based cohort_picker. Two parts:
#
#   1. Selected chips strip at the top — one chip per outbreak in the cohort,
#      with an `x` to remove.
#   2. Thumbnail grid below — every outbreak as a clickable tile showing a
#      hand-rolled SVG sparkline + name/year. Clicking toggles inclusion.
#
# Sparklines are inline SVG (no extra dependency, no plotOutput overhead).
# Selection state lives in `lab_session$cohort_ids`.
# ------------------------------------------------------------------------------

# Hand-rolled SVG sparkline polyline. Width/height in CSS pixels.
.sparkline_svg <- function(y, width = 120, height = 36) {
  y <- y[!is.na(y)]
  if (length(y) < 2L) return(htmltools::HTML(""))
  x <- seq_along(y) - 1L
  x_norm <- x / max(x) * (width - 2) + 1
  y_max <- max(y, na.rm = TRUE)
  if (y_max <= 0) y_max <- 1
  y_norm <- height - 2 - (y / y_max) * (height - 4)
  pts <- paste(round(x_norm, 2), round(y_norm, 2), sep = ",", collapse = " ")
  svg <- sprintf(
    paste0(
      '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" ',
      'viewBox="0 0 %d %d" preserveAspectRatio="none">',
      '<polyline points="%s" fill="none" stroke="currentColor" ',
      'stroke-width="1.5" stroke-linejoin="round"/></svg>'
    ),
    width, height, width, height, pts
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

# A single picker tile. Selected tiles get the .yl-tile-selected class.
.cohort_tile <- function(ns, row, selected, deaths_series) {
  cls <- if (selected) "yl-tile yl-tile-selected" else "yl-tile"
  shiny::tags$button(
    type = "button",
    class = cls,
    id = ns(paste0("tile_", row$outbreak_id)),
    onclick = sprintf(
      "Shiny.setInputValue('%s', {id: '%s', t: Math.random()})",
      ns("tile_click"), row$outbreak_id
    ),
    shiny::tags$div(class = "yl-tile-spark", .sparkline_svg(deaths_series)),
    shiny::tags$div(class = "yl-tile-meta",
      shiny::tags$strong(row$location),
      shiny::tags$small(as.character(row$year))
    )
  )
}

#' Cohort module — UI.
#'
#' @param id Module namespace id.
#' @return A `bslib::card()` with the cohort chips + picker grid.
#' @export
cohort_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Cohort"),
    bslib::card_body(
      shiny::uiOutput(ns("chips")),
      shiny::tags$hr(),
      shiny::uiOutput(ns("grid"))
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
  if (is.null(data)) data <- get("outbreaks", envir = asNamespace("yersinia"))
  summary_tbl <- outbreak_summary(data)
  series_by_id <- split(data$deaths, data$outbreak_id)

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$chips <- shiny::renderUI({
      ids <- lab_session$cohort_ids
      if (length(ids) == 0) {
        return(shiny::tags$span(class = "yl-chips-empty",
                                "No outbreaks selected yet."))
      }
      labels <- vapply(ids, function(i) {
        row <- summary_tbl[summary_tbl$outbreak_id == i, , drop = FALSE]
        if (nrow(row) == 0) i else paste0(row$location, " ", row$year)
      }, character(1))
      do.call(shiny::tagList,
              Map(function(id, lab) .cohort_chip(ns, id, lab), ids, labels))
    })

    output$grid <- shiny::renderUI({
      selected <- lab_session$cohort_ids
      tiles <- lapply(seq_len(nrow(summary_tbl)), function(i) {
        row <- summary_tbl[i, ]
        .cohort_tile(ns, row, row$outbreak_id %in% selected,
                     series_by_id[[row$outbreak_id]])
      })
      shiny::tags$div(class = "yl-tile-grid", do.call(shiny::tagList, tiles))
    })

    # Tile click — toggle.
    shiny::observeEvent(input$tile_click, {
      id <- input$tile_click$id
      if (is.null(id)) return()
      current <- shiny::isolate(lab_session$cohort_ids)
      if (id %in% current) {
        lab_session$cohort_ids <- setdiff(current, id)
      } else {
        lab_session$cohort_ids <- c(current, id)
      }
    })

    # Remove-chip clicks. Inputs are dynamically named remove_<id>; observe
    # them once at module mount per known id.
    for (oid_ in summary_tbl$outbreak_id) {
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
