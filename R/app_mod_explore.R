# ------------------------------------------------------------------------------
# app_mod_explore.R — Shiny module: the outbreak explorer.
#
# The app's opening screen. Krauer's catalogue is 130 records across 84 places
# and five centuries, of which only 52 can be fitted, so the first question a
# user has is "what is in here and which of it is usable" — not "which of these
# nine do I want". This panel answers the first question; `app_mod_cohort.R`'s
# chip strip in the sidebar carries the answer through to the rest of the app.
#
# Three parts:
#
#   1. Filter rail — search, era, country, cadence, cause-of-death, two
#      screening toggles, and a sort control. All filters compose; the count
#      line always says how many of the 130 survive them.
#   1b. Map and timeline — "where" and "when", drawn from the same filtered
#      set as the grid, with everything filtered out left faint rather than
#      removed so the view never stops being a picture of the whole
#      catalogue. Clicking a place filters to it; clicking a timeline dot
#      selects that outbreak, exactly as clicking its tile would.
#   2. Tile grid — one tile per outbreak: sparkline, place, year, total
#      deaths and an attack-rate meter. Clicking toggles cohort membership
#      and opens the detail card.
#   3. Detail card — the outbreak last clicked, in full: its whole series,
#      its numbers, its flags and its provenance.
#
# WHY TILES RATHER THAN A TABLE
#
# The thing being compared here is epidemic *shape* — one peak or two, sharp
# or long-tailed, where in the season it sits. A sparkline is the only column
# that carries that, and in a table it is one narrow cell among nine, ordered
# by whichever header was last clicked. The tile makes the sparkline the whole
# object and the name its caption. The filter rail does the narrowing a
# table's headers would have done, and an explicit sort control does the
# ordering; everything else about a record lives in the detail card, one click
# away, rather than crowding 130 tiles at once.
#
# The tile markup is deliberately the same as `app_mod_cohort.R`'s — same
# classes, same structure — so the sidebar chips and this grid stay one visual
# language rather than two.
#
# WHY UNFITTABLE RECORDS ARE SHOWN AT ALL
#
# 78 of the 130 cannot reach a sampler: no population (K_h and K_r are pinned
# to it) or no whole-day reporting window (the D_h accumulator resets on it).
# Hiding them would make the explorer quietly disagree with the catalogue it
# claims to show, and the reason a record is unusable is itself worth seeing —
# it is a fact about the historical record, not an implementation detail. So
# they are shown greyed, clicking one still opens its detail (that is the only
# thing left to do with it), and only the *selection* is refused, with the
# reason.
# ------------------------------------------------------------------------------

# Cause-of-death is a long string in the data and a narrow line on a card.
# Krauer has exactly two values.
.explore_type_short <- function(x) {
  ifelse(x == "plague mortality", "plague", "all-cause")
}

# Compact count for a tile: 842 -> "842", 8018 -> "8.0k", 33532 -> "34k".
.explore_num <- function(x) {
  if (length(x) == 0 || is.na(x)) return("\u2014")
  if (x >= 1e6) return(sprintf("%.1fM", x / 1e6))
  if (x >= 1e4) return(sprintf("%.0fk", x / 1e3))
  if (x >= 1e3) return(sprintf("%.1fk", x / 1e3))
  format(round(x))
}

# The attack-rate meter under a tile's caption.
#
# Scaled to a 50%-of-population reference, not to 100%: real attack rates run
# 1-20%, so a 0-100% scale would leave almost every bar a stub and the channel
# would carry nothing. At 50% full-width the range spreads out and the handful
# of records above it are exactly the ones `attack_flag` already marks, so
# they clamp at full width wearing the warning colour rather than silently
# rescaling every other tile. Absent when there is no population to divide by,
# which is its own signal.
.explore_meter <- function(row) {
  if (is.na(row$attack_rate)) return(NULL)
  frac <- min(1, row$attack_rate / 0.5)
  cls <- if (is.na(row$attack_flag)) "yl-meter-fill"
         else paste("yl-meter-fill", "yl-meter-flagged")
  shiny::tags$div(
    class = "yl-tile-meter",
    title = sprintf("%.0f%% of the recorded population", 100 * row$attack_rate),
    shiny::tags$div(class = cls,
                    style = sprintf("width:%.1f%%", 100 * frac))
  )
}

# Badges for the flags a record carries. Shown on the detail card only \u2014 on a
# tile they would bury the sparkline, which is the thing worth looking at.
.explore_flags <- function(row) {
  badge <- function(cls, text, title) {
    shiny::tags$span(class = paste("yl-badge", cls), title = title, text)
  }
  out <- list()
  if (!isTRUE(row$fittable)) {
    out <- c(out, list(badge("yl-badge-mute", "not fittable",
                             row$unfit_reason %||% "")))
  }
  if (identical(row$attack_flag, "impossible")) {
    out <- c(out, list(badge(
      "yl-badge-bad", "impossible",
      paste("Recorded deaths exceed the recorded population \u2014 the",
            "series, the population, or both are wrong."))))
  } else if (identical(row$attack_flag, "high")) {
    out <- c(out, list(badge(
      "yl-badge-warn", "high attack rate",
      paste("Above 35% recorded mortality. A closed-population model",
            "struggles to reproduce this; in Stan fits it collapsed the",
            "sampler's step size."))))
  }
  if (identical(row$sourcetype, "graph")) {
    out <- c(out, list(badge(
      "yl-badge-mute", "from graph",
      "Digitised from a published figure rather than transcribed.")))
  }
  out
}

# One tile. Same markup as the cohort module's tiles; the only addition is
# `yl-tile-unfittable`, which greys the caption while leaving the sparkline
# legible — the record is real and its shape is still worth reading, it just
# can't reach a sampler.
.explore_tile <- function(ns, row, selected, deaths_series, ymax = NULL) {
  cls <- c("yl-tile",
           if (selected) "yl-tile-selected",
           if (!isTRUE(row$fittable)) "yl-tile-unfittable")
  shiny::tags$button(
    type = "button",
    class = paste(cls, collapse = " "),
    title = if (isTRUE(row$fittable)) row$label
            else paste0(row$label, " \u2014 ", row$unfit_reason),
    onclick = sprintf(
      "Shiny.setInputValue('%s', {id: '%s', t: Math.random()})",
      ns("card_click"), row$outbreak_id
    ),
    shiny::tags$div(class = "yl-tile-spark",
                    .sparkline_svg(deaths_series, width = 160, height = 40,
                                   ymax = ymax)),
    # Three channels on one line instead of two, so the extra data costs no
    # height: place and year on the left, total deaths on the right. The
    # meter below adds ~5px and carries attack rate.
    shiny::tags$div(class = "yl-tile-meta",
      shiny::tags$span(class = "yl-tile-name",
        shiny::tags$strong(row$location),
        shiny::tags$small(as.character(row$year))
      ),
      shiny::tags$span(class = "yl-tile-count",
                       title = "total recorded deaths",
                       .explore_num(row$total_deaths))
    ),
    .explore_meter(row)
  )
}

#' Outbreak explorer — UI.
#'
#' @param id Module namespace id.
#' @return A `bslib::layout_sidebar()` holding the filter rail, the card grid
#'   and the detail card.
#' @export
explore_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    fillable = TRUE,
    sidebar = bslib::sidebar(
      width = 280, class = "yl-explore-filters",
      shiny::textInput(ns("search"), "Search place or source", ""),
      # Bounds are filled in by the server from the data, so the rail can't
      # drift out of step with the catalogue it filters.
      shiny::sliderInput(ns("era"), "Years", min = 1300, max = 1900,
                         value = c(1300, 1900), sep = "", step = 1),
      shiny::selectizeInput(
        ns("country"), "Country", choices = NULL, multiple = TRUE,
        options = list(placeholder = "All countries")),
      shiny::checkboxGroupInput(
        ns("cadence"), "Reporting cadence",
        choices = c("daily", "weekly", "biweekly", "monthly"),
        selected = c("daily", "weekly", "biweekly", "monthly")),
      shiny::checkboxGroupInput(
        ns("type"), "Cause of death recorded",
        choices = c("plague only" = "plague mortality",
                    "all-cause"   = "all-cause mortality"),
        selected = c("plague mortality", "all-cause mortality")),
      # Transcription route is a data-quality axis, not trivia: a series read
      # off a printed figure carries the digitiser's eye as well as the
      # original's errors, and 48 of the 59 graph-derived records are
      # unfittable anyway.
      shiny::checkboxGroupInput(
        ns("sourcetype"), "Transcribed from",
        choices = c("a table" = "table", "a graph" = "graph"),
        selected = c("table", "graph")),
      shiny::hr(),
      shiny::checkboxInput(ns("only_fittable"), "Fittable only", FALSE),
      shiny::checkboxInput(ns("hide_flagged"),
                           "Hide attack-rate flagged", FALSE),
      shiny::radioButtons(
        ns("scale"), "Sparkline scale",
        choices = c("Each to its own peak" = "own",
                    "Shared across the grid" = "shared"),
        selected = "own"),
      shiny::selectInput(
        ns("sort"), "Sort by",
        choices = c("Year" = "year", "Place" = "label",
                    "Total deaths" = "total_deaths",
                    "Duration" = "duration_days",
                    "Attack rate" = "attack_rate",
                    "Population" = "population"),
        selected = "year"),
      shiny::actionLink(ns("reset"), "Reset filters", class = "yl-reset-link")
    ),
    shiny::div(
      class = "yl-explore-main",
      shiny::div(
        class = "yl-views",
        shiny::div(class = "yl-view yl-view-map",
                   shiny::div(class = "yl-view-head", "Where",
                              shiny::uiOutput(ns("place_chip"),
                                              inline = TRUE)),
                   shiny::uiOutput(ns("map"))),
        shiny::div(class = "yl-view yl-view-time",
                   shiny::div(class = "yl-view-head", "When"),
                   shiny::uiOutput(ns("timeline"))),
        shiny::div(class = "yl-view yl-view-phase",
                   shiny::div(class = "yl-view-head", "Seasonal phase",
                              shiny::tags$span(class = "yl-view-hint",
                                "peak day vs latitude \u2014 r = 0.64")),
                   shiny::uiOutput(ns("phase"))),
        shiny::div(class = "yl-view yl-view-src",
                   shiny::div(class = "yl-view-head", "From whom",
                              shiny::uiOutput(ns("source_chip"),
                                              inline = TRUE)),
                   shiny::uiOutput(ns("sources")))
      ),
      shiny::uiOutput(ns("count")),
      shiny::uiOutput(ns("grid")),
      shiny::uiOutput(ns("detail")),
      # A real table of the filtered set, and the same thing as a file.
      # Partly accessibility -- without it there is no non-visual path to
      # this data at all -- and partly because the filtered set is exactly
      # what you would want to paste into a methods section.
      shiny::tags$details(
        class = "yl-table-wrap",
        shiny::tags$summary("Table view"),
        shiny::downloadButton(ns("download"), "Download as CSV",
                              class = "yl-dl-btn"),
        shiny::uiOutput(ns("table"))
      )
    )
  )
}

#' Outbreak explorer — server.
#'
#' Tile selection writes through to `lab_session$cohort_ids`, and the grid is
#' rebuilt from that vector, so a cohort changed anywhere else (loading a
#' saved fit, removing a chip in the Lab sidebar) is reflected here without a
#' second source of truth.
#'
#' @param id Module namespace id.
#' @param lab_session A [LabSession] instance.
#' @param data Long outbreaks tibble. Defaults to the bundled [outbreaks_all].
#' @return Invisibly, the moduleServer result.
#' @export
explore_server <- function(id, lab_session, data = NULL) {
  if (is.null(data)) data <- get("outbreaks_all", envir = asNamespace("yersinia"))
  summary_tbl <- outbreak_summary(data)
  series_by_id <- split(data$deaths, data$outbreak_id)

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    focused <- shiny::reactiveVal(NULL)
    # Set by clicking a place on the map. Kept out of `input` because it is
    # toggled from a click rather than bound to a control, and because the
    # reset link has to be able to clear it.
    place_filter <- shiny::reactiveVal(NULL)
    source_filter <- shiny::reactiveVal(NULL)

    era_bounds <- range(summary_tbl$year)
    shiny::updateSelectizeInput(
      session, "country",
      choices = sort(unique(summary_tbl$country)), server = FALSE)
    shiny::updateSliderInput(session, "era",
                             min = era_bounds[1], max = era_bounds[2],
                             value = era_bounds)

    filtered <- shiny::reactive({
      s <- summary_tbl
      if (!is.null(input$era)) {
        s <- s[s$year >= input$era[1] & s$year <= input$era[2], ]
      }
      if (length(input$country)) s <- s[s$country %in% input$country, ]
      # An unchecked group means "none", not "all" — a box you have emptied
      # that silently showed all 130 would be lying about what you filtered.
      if (length(input$cadence)) {
        s <- s[s$interval %in% input$cadence, ]
      } else {
        s <- s[0, ]
      }
      if (length(input$type)) s <- s[s$type %in% input$type, ] else s <- s[0, ]
      if (length(input$sourcetype)) {
        s <- s[s$sourcetype %in% input$sourcetype, ]
      } else {
        s <- s[0, ]
      }
      if (!is.null(place_filter())) s <- s[s$location %in% place_filter(), ]
      if (!is.null(source_filter())) s <- s[s$source %in% source_filter(), ]
      if (isTRUE(input$only_fittable)) s <- s[s$fittable, ]
      if (isTRUE(input$hide_flagged)) s <- s[is.na(s$attack_flag), ]
      q <- trimws(input$search %||% "")
      if (nzchar(q)) {
        hay <- paste(s$label, s$country, s$source)
        s <- s[grepl(q, hay, ignore.case = TRUE), ]
      }
      key <- input$sort %||% "year"
      # Descending for the "how big / how long / how bad" sorts, where the
      # interesting end is the top; ascending for year and place, where it is
      # the natural reading order. NAs last either way.
      s <- if (key %in% c("year", "label")) {
        s[order(s[[key]], na.last = TRUE), ]
      } else {
        s[order(s[[key]], decreasing = TRUE, na.last = TRUE), ]
      }
      s
    })

    # Both views are drawn over the WHOLE catalogue, with the filtered-out
    # records dimmed rather than dropped. A map that redraws with only the
    # survivors on it stops being a map of anything — you lose the very
    # comparison (how lopsided is this catalogue?) the view exists to make.
    output$map <- shiny::renderUI({
      vis <- filtered()$outbreak_id
      sel <- lab_session$cohort_ids
      places <- summary_tbl |>
        dplyr::group_by(.data$location, .data$lat, .data$lon) |>
        dplyr::summarise(
          n = dplyr::n(),
          visible = any(.data$outbreak_id %in% vis),
          selected = any(.data$outbreak_id %in% sel),
          .groups = "drop")
      .explore_map_svg(places, ns, place_filter())
    })

    output$timeline <- shiny::renderUI({
      vis <- filtered()$outbreak_id
      sel <- lab_session$cohort_ids
      s <- summary_tbl
      s$visible <- s$outbreak_id %in% vis
      s$selected <- s$outbreak_id %in% sel
      .explore_timeline_svg(s, ns)
    })

    # Clicking a place filters to it; clicking the same place again clears.
    shiny::observeEvent(input$map_click, {
      pl <- input$map_click$place
      if (is.null(pl)) return()
      place_filter(if (identical(place_filter(), pl)) NULL else pl)
    })

    output$place_chip <- shiny::renderUI({
      pl <- place_filter()
      if (is.null(pl)) {
        return(shiny::tags$span(class = "yl-view-hint",
                                "click a place to filter"))
      }
      shiny::tags$span(
        class = "yl-chip yl-chip-sm", shiny::tags$span(pl),
        shiny::actionLink(ns("clear_place"), shiny::icon("xmark"),
                          class = "yl-chip-x", title = "Clear place filter"))
    })

    shiny::observeEvent(input$clear_place, place_filter(NULL))

    output$sources <- shiny::renderUI({
      vis <- filtered()$outbreak_id
      sel <- lab_session$cohort_ids
      s <- summary_tbl
      s$visible <- s$outbreak_id %in% vis
      s$selected <- s$outbreak_id %in% sel
      .explore_sources_ui(s, ns, source_filter())
    })

    shiny::observeEvent(input$source_click, {
      src <- input$source_click$source
      if (is.null(src)) return()
      source_filter(if (identical(source_filter(), src)) NULL else src)
    })

    output$source_chip <- shiny::renderUI({
      src <- source_filter()
      if (is.null(src)) {
        return(shiny::tags$span(class = "yl-view-hint",
                                "45 sources for 130 records"))
      }
      shiny::tags$span(
        class = "yl-chip yl-chip-sm", shiny::tags$span(src),
        shiny::actionLink(ns("clear_source"), shiny::icon("xmark"),
                          class = "yl-chip-x", title = "Clear source filter"))
    })

    shiny::observeEvent(input$clear_source, source_filter(NULL))

    output$count <- shiny::renderUI({
      s <- filtered()
      n_sel <- length(lab_session$cohort_ids)
      shiny::div(
        class = "yl-explore-count",
        shiny::tags$strong(nrow(s)), " of ", nrow(summary_tbl), " outbreaks",
        shiny::tags$span(class = "yl-dot", "·"),
        shiny::tags$span(sum(s$fittable), " fittable"),
        shiny::tags$span(class = "yl-dot", "·"),
        shiny::tags$span(class = if (n_sel > 0) "yl-sel-on" else "yl-sel-off",
                         n_sel, " selected"),
        .explore_provenance_note(
          summary_tbl[summary_tbl$outbreak_id %in% lab_session$cohort_ids, ])
      )
    })

    output$phase <- shiny::renderUI({
      vis <- filtered()$outbreak_id
      sel <- lab_session$cohort_ids
      s <- summary_tbl
      s$visible <- s$outbreak_id %in% vis
      s$selected <- s$outbreak_id %in% sel
      .explore_phase_svg(s, ns)
    })

    output$grid <- shiny::renderUI({
      s <- filtered()
      if (nrow(s) == 0) {
        return(shiny::div(class = "yl-grid-empty",
                          "No outbreaks match these filters."))
      }
      selected <- lab_session$cohort_ids
      # A shared ceiling is taken over what is *on screen*, not the whole
      # catalogue: scaling 130 tiles to Cairo's 33,532 when Cairo is filtered
      # out would flatten every visible series for no reason.
      ymax <- if (identical(input$scale, "shared")) {
        max(s$peak_deaths, na.rm = TRUE)
      } else NULL
      tiles <- lapply(seq_len(nrow(s)), function(i) {
        row <- s[i, ]
        .explore_tile(ns, row, row$outbreak_id %in% selected,
                      series_by_id[[row$outbreak_id]], ymax = ymax)
      })
      shiny::tags$div(class = "yl-tile-grid yl-explore-grid",
                      do.call(shiny::tagList, tiles))
    })

    # A click always focuses the detail card — that is the only thing left to
    # do with an unfittable record, so it must not be a dead click. Selection
    # is the part that gets refused, with the row's own reason.
    shiny::observeEvent(input$card_click, {
      id <- input$card_click$id
      if (is.null(id)) return()
      focused(id)
      row <- summary_tbl[match(id, summary_tbl$outbreak_id), ]
      if (!isTRUE(row$fittable)) {
        shiny::showNotification(
          shiny::span(shiny::icon("circle-info"), " ",
                      shiny::tags$strong(row$label), " cannot be fitted: ",
                      row$unfit_reason, "."),
          type = "warning", duration = 8)
        return()
      }
      current <- shiny::isolate(lab_session$cohort_ids)
      lab_session$cohort_ids <- if (id %in% current) setdiff(current, id)
                                else c(current, id)
    })

    output$detail <- shiny::renderUI({
      id <- focused()
      if (is.null(id)) {
        return(shiny::div(
          class = "yl-detail-empty",
          "Click a tile to see its full series, numbers and source."))
      }
      row <- summary_tbl[match(id, summary_tbl$outbreak_id), ]
      shiny::div(
        class = "yl-detail",
        shiny::div(class = "yl-detail-head",
                   shiny::tags$h5(row$label),
                   .explore_flags(row)),
        shiny::div(class = "yl-detail-spark",
                   .sparkline_svg(series_by_id[[row$outbreak_id]],
                                  width = 640, height = 90)),
        shiny::tags$dl(
          class = "yl-detail-meta",
          shiny::tags$dt("Records"),
          shiny::tags$dd(sprintf("%s, %s, %s calendar",
                                 .explore_type_short(row$type),
                                 row$interval, row$calendar)),
          shiny::tags$dt("Observations"),
          shiny::tags$dd(sprintf("%d over %d days%s", row$n_obs,
                                 row$duration_days,
                                 if (row$n_missing > 0)
                                   sprintf(" (%d missing)", row$n_missing)
                                 else "")),
          shiny::tags$dt("Deaths"),
          shiny::tags$dd(sprintf("%s total, peak %s on day %d",
                                 format(row$total_deaths, big.mark = ","),
                                 format(row$peak_deaths, big.mark = ","),
                                 row$peak_day)),
          shiny::tags$dt("Population"),
          shiny::tags$dd(if (is.na(row$population)) "not recorded"
                         else sprintf("%s (%s)",
                                      format(row$population, big.mark = ","),
                                      row$population_source)),
          shiny::tags$dt("Attack rate"),
          shiny::tags$dd(if (is.na(row$attack_rate)) "not computable"
                         else sprintf("%.0f%% of recorded population",
                                      100 * row$attack_rate)),
          shiny::tags$dt("Source"),
          shiny::tags$dd(
            row$source,
            shiny::tags$span(
              class = "yl-detail-sub",
              sprintf(", %s%s",
                      if (identical(row$sourcetype, "graph"))
                        "digitised from a figure" else "transcribed from a table",
                      switch(as.character(row$complete),
                             "yes" = "; Krauer records the series as complete",
                             "no"  = "; Krauer records the series as INCOMPLETE",
                             "; completeness not recorded")))),
          # How many other records ride on this same citation. One outbreak
          # from a source is an observation; twenty-three are one author's
          # dataset, and the distinction changes what a cohort fit means.
          shiny::tags$dt("Shared with"),
          shiny::tags$dd(local({
            n <- sum(summary_tbl$source == row$source) - 1L
            if (n == 0) "nothing \u2014 this source contributes one record"
            else sprintf("%d other outbreak%s from the same source", n,
                         if (n == 1) "" else "s")
          }))
        )
      )
    })

    # The table shows what the grid shows, in the order the grid shows it.
    table_df <- shiny::reactive({
      s <- filtered()
      data.frame(
        Outbreak = s$label, Country = s$country, Year = s$year,
        Cadence = s$interval, Records = .explore_type_short(s$type),
        Days = s$duration_days, Observations = s$n_obs, Missing = s$n_missing,
        Deaths = s$total_deaths, `Peak day` = s$peak_day,
        `Peak day-of-year` = s$peak_doy,
        Population = s$population, `Attack rate` = round(s$attack_rate, 4),
        Fittable = s$fittable, `Unfittable because` = s$unfit_reason,
        Flag = s$attack_flag, Source = s$source,
        `Transcribed from` = s$sourcetype, Complete = s$complete,
        `Krauer id` = s$krauer_id,
        check.names = FALSE, stringsAsFactors = FALSE)
    })

    output$table <- shiny::renderUI({
      df <- table_df()
      if (nrow(df) == 0) {
        return(shiny::tags$p(class = "yl-table-empty",
                             "No outbreaks match these filters."))
      }
      header <- shiny::tags$tr(lapply(names(df), function(n)
        shiny::tags$th(scope = "col", n)))
      rows <- lapply(seq_len(nrow(df)), function(i) {
        shiny::tags$tr(
          # The outbreak name is the row's header, so a screen reader can
          # announce which record a cell belongs to.
          shiny::tags$th(scope = "row", df[[1]][i]),
          lapply(df[i, -1, drop = FALSE], function(v) {
            shiny::tags$td(if (is.na(v)) "\u2014" else as.character(v))
          }))
      })
      shiny::tags$table(
        class = "yl-table",
        shiny::tags$caption(sprintf(
          "%d outbreak%s matching the current filters", nrow(df),
          if (nrow(df) == 1) "" else "s")),
        shiny::tags$thead(header), shiny::tags$tbody(rows))
    })

    output$download <- shiny::downloadHandler(
      filename = function() {
        sprintf("outbreaks-%s.csv", format(Sys.Date(), "%Y%m%d"))
      },
      content = function(file) {
        utils::write.csv(table_df(), file, row.names = FALSE, na = "")
      }
    )

    shiny::observeEvent(input$reset, {
      shiny::updateTextInput(session, "search", value = "")
      shiny::updateSliderInput(session, "era", value = era_bounds)
      shiny::updateSelectizeInput(session, "country", selected = character(0))
      shiny::updateCheckboxGroupInput(
        session, "cadence",
        selected = c("daily", "weekly", "biweekly", "monthly"))
      shiny::updateCheckboxGroupInput(
        session, "type",
        selected = c("plague mortality", "all-cause mortality"))
      shiny::updateCheckboxInput(session, "only_fittable", value = FALSE)
      shiny::updateCheckboxInput(session, "hide_flagged", value = FALSE)
      shiny::updateSelectInput(session, "sort", selected = "year")
      shiny::updateRadioButtons(session, "scale", selected = "own")
      shiny::updateCheckboxGroupInput(session, "sourcetype",
                                      selected = c("table", "graph"))
      place_filter(NULL)
      source_filter(NULL)
    })
  })
}
