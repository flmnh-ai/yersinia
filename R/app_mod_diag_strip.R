# ------------------------------------------------------------------------------
# app_mod_diag_strip.R — Shiny module: diagnostic chips below the hero.
#
# One chip per record returned by run_diagnostics(), colour-coded by
# severity (alert / warn / info). Empty state ("All clear") when no flags
# fire and a fit exists. Hidden entirely when no fit yet.
#
# Each chip is a Bootstrap popover — short label visible inline, message
# and suggested fix on hover/click.
# ------------------------------------------------------------------------------

#' Diagnostics strip module — UI.
#'
#' @param id Module namespace id.
#' @return A `shiny::div()` for the strip (initially empty).
#' @export
diag_strip_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "yl-diag-strip",
    shiny::tags$span(class = "yl-diag-label", "Diagnostics:"),
    shiny::uiOutput(ns("chips"), inline = TRUE),
    shiny::div(class = "yl-diag-spacer"),
    shiny::actionLink(ns("open_plots"), "Trace + density",
                      icon = shiny::icon("chart-line"),
                      class = "yl-diag-open")
  )
}

# Height (px) of trace/density plots, scaled by parameter count.
.diag_plot_height <- function(n_pars, per_row = 3, row_px = 110) {
  rows <- max(1L, ceiling(n_pars / per_row))
  rows * row_px
}

# Severity -> bootstrap-ish class fragment.
.diag_chip_class <- function(severity) {
  switch(severity,
    alert = "yl-diag-chip yl-diag-alert",
    warn  = "yl-diag-chip yl-diag-warn",
    info  = "yl-diag-chip yl-diag-info",
    "yl-diag-chip"
  )
}

# Render one diagnostic record as a popover chip.
.diag_chip <- function(rec) {
  bslib::popover(
    shiny::tags$span(
      class = .diag_chip_class(rec$severity),
      paste(toupper(rec$severity), rec$detector, sep = " · ")
    ),
    title = rec$message,
    shiny::p(shiny::tags$strong("Suggested fix:")),
    shiny::p(rec$suggested_fix)
  )
}

#' Diagnostics strip module — server.
#'
#' @param id Module namespace id.
#' @param lab_session A [LabSession] instance.
#' @return Invisibly, the moduleServer result.
#' @export
diag_strip_server <- function(id, lab_session) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    draws <- shiny::reactive({
      st <- lab_session$fit_state
      if (is.null(st$samples)) return(NULL)
      posterior::as_draws_array(st$samples)
    })

    diag_records <- shiny::reactive({
      d <- draws()
      if (is.null(d)) return(NULL)
      bounds <- priors_to_bounds(lab_session$priors, posterior::variables(d))
      run_diagnostics(d, bounds = bounds)
    })

    output$chips <- shiny::renderUI({
      recs <- diag_records()
      if (is.null(recs)) {
        return(shiny::tags$span(class = "yl-diag-empty",
                                "run a pilot to populate"))
      }
      if (length(recs) == 0) {
        return(shiny::tags$span(class = "yl-diag-chip yl-diag-ok",
                                "All diagnostics passed"))
      }
      do.call(shiny::tagList, lapply(recs, .diag_chip))
    })

    shiny::observeEvent(input$open_plots, {
      d <- draws()
      if (is.null(d)) {
        shiny::showModal(shiny::modalDialog(
          title = "Posterior diagnostics",
          shiny::p(shiny::em("Run a pilot to populate trace and density plots.")),
          easyClose = TRUE, size = "l"
        ))
        return()
      }
      n_pars <- length(posterior::variables(d))
      h <- paste0(.diag_plot_height(n_pars), "px")
      shiny::showModal(shiny::modalDialog(
        title = "Posterior diagnostics",
        shiny::tags$h6(class = "yl-diag-plots-subhead", "Trace"),
        shiny::plotOutput(ns("trace"), height = h),
        shiny::tags$hr(),
        shiny::tags$h6(class = "yl-diag-plots-subhead",
                       "Posterior density (per chain)"),
        shiny::plotOutput(ns("density"), height = h),
        easyClose = TRUE, size = "xl", footer = shiny::modalButton("Close")
      ))
    })

    output$trace <- shiny::renderPlot({
      d <- draws()
      shiny::req(d)
      bayesplot::mcmc_trace(d) +
        ggplot2::theme_minimal(base_size = 10) +
        ggplot2::theme(legend.position = "none")
    })

    output$density <- shiny::renderPlot({
      d <- draws()
      shiny::req(d)
      bayesplot::mcmc_dens_overlay(d) +
        ggplot2::theme_minimal(base_size = 10) +
        ggplot2::theme(legend.position = "none")
    })
  })
}
