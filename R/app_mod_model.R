# ------------------------------------------------------------------------------
# app_mod_model.R — Shiny module: model configuration as a rail accordion.
#
# Collapsed: header shows "<scenario> · <N> params". Expanded: scenario
# dropdown plus two compact selectize inputs (shared / local). Everything
# else stays fixed at the scenario's value.
#
# Replaces the old tab-based model_config module. Same state shape
# (`lab_session$model_config`), just a tighter, rail-friendly UI.
# ------------------------------------------------------------------------------

#' Model accordion module — UI.
#'
#' @param id Module namespace id.
#' @return A `bslib::accordion()` containing the model config controls.
#' @export
model_accordion_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::accordion(
    open = FALSE,
    bslib::accordion_panel(
      title = shiny::uiOutput(ns("title"), inline = TRUE),
      value = "model",
      icon = shiny::icon("sliders"),
      shiny::selectInput(ns("scenario"), "Scenario",
                         choices = available_scenarios()),
      shiny::selectizeInput(ns("shared"),
                            "Shared (one value across outbreaks)",
                            choices = configurable_param_names(),
                            multiple = TRUE),
      shiny::selectizeInput(ns("local"),
                            "Local (varies per outbreak)",
                            choices = configurable_param_names(),
                            multiple = TRUE)
    )
  )
}

#' Model accordion module — server.
#'
#' @param id Module namespace id.
#' @param lab_session A [LabSession] instance.
#' @return Invisibly, the moduleServer result.
#' @export
model_accordion_server <- function(id, lab_session) {
  shiny::moduleServer(id, function(input, output, session) {
    cfg_init <- shiny::isolate(lab_session$model_config)
    shiny::updateSelectInput(session, "scenario", selected = cfg_init$scenario)
    shiny::updateSelectizeInput(session, "shared", selected = cfg_init$shared)
    shiny::updateSelectizeInput(session, "local",  selected = cfg_init$local)

    output$title <- shiny::renderUI({
      cfg <- lab_session$model_config
      n <- length(unique(c(cfg$shared, cfg$local)))
      shiny::HTML(sprintf(
        "<strong>Model</strong> <span class='text-muted'>%s &middot; %d param%s</span>",
        cfg$scenario %||% "—", n, if (n == 1) "" else "s"
      ))
    })

    shiny::observe({
      if (is.null(input$scenario)) return()
      lab_session$model_config <- list(
        scenario = input$scenario,
        shared   = input$shared %||% character(0),
        local    = input$local  %||% character(0)
      )
    })
  })
}
