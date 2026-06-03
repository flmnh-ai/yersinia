# ------------------------------------------------------------------------------
# app_launcher.R — entrypoint for the Virtual Lab Shiny app.
#
# Single-screen layout:
#
#   +---------- sidebar ----------+--------- main ---------+
#   | COHORT  (chips + grid)      |     HERO plot          |
#   | MODEL   (accordion)         |  data + posterior fan  |
#   | PRIORS  (accordion)         +------------------------+
#   |                             |  diag chip strip       |
#   +-----------------------------+------------------------+
#   |    [ status text ]                  [ Run / Refit ]  |
#   +----------------------------------------------------- +
# ------------------------------------------------------------------------------

#' Launch the yersinia Virtual Lab Shiny app.
#'
#' Returns a [shiny::shinyApp()] object — call directly to launch
#' (`lab_app()`), or wrap with [shiny::runApp()] for explicit control over
#' host/port.
#'
#' Requires the optional packages `shiny`, `bslib`, `R6`, `ggplot2`,
#' `posterior`, and `bayesplot`. They live in `Suggests`; install with
#' `install.packages(c("shiny", "bslib", "R6", "ggplot2", "posterior", "bayesplot"))`.
#'
#' @param saved_session Optional [LabSession] instance. If `NULL` (default),
#'   a fresh empty session is created. Pass a previously saved session via
#'   [lab_session_from_list()] to resume a lab.
#' @return A [shiny::shinyApp()] object.
#' @export
lab_app <- function(saved_session = NULL) {
  for (pkg in c("shiny", "bslib", "R6", "ggplot2",
                "posterior", "bayesplot", "tidyr")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cli::cli_abort(c(
        "Missing optional dependency for the Virtual Lab app: {.pkg {pkg}}.",
        i = "Install with {.code install.packages(\"{pkg}\")}."
      ))
    }
  }

  www_path <- system.file("www", package = "yersinia")
  shiny::addResourcePath("yersinia-www", www_path)
  explainer_path <- system.file("explainer", package = "yersinia")
  shiny::addResourcePath("yersinia-explainer", explainer_path)

  ui <- bslib::page_sidebar(
    title = "yersinia Virtual Lab",
    theme = bslib::bs_theme(version = 5, preset = "shiny"),
    fillable = TRUE,
    sidebar = bslib::sidebar(
      width = 340,
      class = "yl-sidebar",
      cohort_ui("cohort"),
      model_accordion_ui("model"),
      priors_accordion_ui("priors")
    ),
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css",
                       href = "yersinia-www/lab.css")
    ),
    # Two tabs in the main area: Lab (hero + diag + status), and Help
    # (the standalone parameter explainer rendered in an iframe). Use
    # navset_card_tab (not navset_tab) — its panel content participates
    # in bslib's fillable flex layout, so the hero plot can actually
    # claim the available vertical space.
    bslib::navset_card_tab(
      full_screen = TRUE,
      bslib::nav_panel(
        "Lab", icon = shiny::icon("flask"),
        shiny::div(
          class = "yl-main",
          shiny::div(class = "yl-hero-wrap", hero_ui("hero")),
          diag_strip_ui("diag"),
          status_bar_ui("status")
        )
      ),
      bslib::nav_panel(
        "Help", icon = shiny::icon("circle-question"),
        shiny::tags$iframe(
          src = "yersinia-explainer/parameters.html",
          class = "yl-help-frame"
        )
      )
    )
  )

  server <- function(input, output, session) {
    lab <- saved_session %||% LabSession$new()
    cohort_server("cohort",  lab_session = lab)
    model_accordion_server("model",  lab_session = lab)
    priors_accordion_server("priors", lab_session = lab)
    hero_server("hero",   lab_session = lab)
    diag_strip_server("diag",   lab_session = lab)
    status_bar_server("status", lab_session = lab)
  }

  shiny::shinyApp(ui, server)
}
