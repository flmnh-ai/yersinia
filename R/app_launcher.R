# ------------------------------------------------------------------------------
# app_launcher.R — entrypoint for the Virtual Lab Shiny app.
#
# Three panels, opening on Explore:
#
#   EXPLORE (default)
#   +------ filters ------+------------- catalogue ----------------+
#   | search / era /      |  count line                            |
#   | country / cadence / |  sortable table, one row per outbreak  |
#   | cause / screens     |  detail card for the last row clicked  |
#   +---------------------+----------------------------------------+
#
#   LAB
#   +---------- sidebar ----------+--------- main ---------+
#   | COHORT  (chips)             |     HERO plot          |
#   | MODEL   (accordion)         |  data + posterior fan  |
#   | PRIORS  (accordion)         +------------------------+
#   |                             |  diag chip strip       |
#   +-----------------------------+------------------------+
#   |    [ status text ]                  [ Run / Refit ]  |
#   +----------------------------------------------------- +
#
#   HELP — the standalone parameter explainer, in an iframe.
#
# WHY EXPLORE OPENS FIRST (2026-09-22)
#
# The app used to open on the Lab with nine outbreaks in a sidebar grid, which
# made sense while nine was the whole world. It now offers Krauer's full
# catalogue: 130 records, 84 places, 1348-1878, of which 52 are fittable.
# Choosing from that is a task in itself, and it is the task that comes first,
# so it gets the opening screen and the full width rather than a 340px rail.
# The Lab sidebar keeps the selected-cohort chips so the choice stays visible
# once you have moved on from making it.
# ------------------------------------------------------------------------------

# -------------------------------------------------------------------------


#' Launch the yersinia Virtual Lab Shiny app.
#'
#' Returns a [shiny::shinyApp()] object — call directly to launch
#' (`lab_app()`), or wrap with [shiny::runApp()] for explicit control over
#' host/port.
#'
#' Requires the optional packages `shiny`, `bslib`, `R6`, `ggplot2`,
#' `posterior`, and `DT`. They live in `Suggests`; install with
#' `install.packages(c("shiny", "bslib", "R6", "ggplot2", "posterior", "DT"))`.
#'
#' @param library_dir Directory backing the saved-fit library. `NULL`
#'   (default) uses [lab_library_dir()] — a per-user data directory that
#'   persists across sessions. Point it at a project folder to keep a
#'   project's fits together.
#' @param restore_last Reopen the most recently saved fit (autosaves
#'   included) when the app starts and no `saved_session` was supplied.
#'   Default `TRUE`, so the lab picks up where you left off. Set `FALSE`
#'   to always open on an empty session. A failure to restore is never
#'   fatal — the app opens empty and says so.
#' @param saved_session Optional [LabSession] instance. If `NULL` (default),
#'   a fresh empty session is created. Pass a previously saved session via
#'   [lab_session_from_list()] to resume a lab.
#' @return A [shiny::shinyApp()] object.
#' @export
lab_app <- function(saved_session = NULL, library_dir = NULL,
                    restore_last = TRUE) {
  for (pkg in c("shiny", "bslib", "R6", "ggplot2",
                "posterior", "tidyr", "DT")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cli::cli_abort(c(
        "Missing optional dependency for the Virtual Lab app: {.pkg {pkg}}.",
        i = "Install with {.code install.packages(\"{pkg}\")}."
      ))
    }
  }

  www_path <- system.file("www", package = "yersinia")
  if (!nzchar(www_path)) {
    cli::cli_abort(c(
      "Cannot find the app's static assets ({.path inst/www}).",
      i = "Reinstall the package, or run {.code devtools::load_all()}."
    ))
  }
  shiny::addResourcePath("yersinia-www", www_path)
  explainer_path <- system.file("explainer", package = "yersinia")
  shiny::addResourcePath("yersinia-explainer", explainer_path)

  # Cache-buster for the stylesheet. Shiny serves static resources with
  # caching headers and the href is otherwise constant, so a browser that has
  # seen an older lab.css keeps using it across app restarts. That is not a
  # cosmetic problem: every `yl-` rule and the whole `:root` token block live
  # in this file, so a stale copy renders the app with square corners, black
  # sparklines and no layout -- indistinguishable from the CSS never loading.
  # Keying on the file's mtime means any edit invalidates it immediately.
  css_stamp <- tryCatch(
    as.integer(file.mtime(file.path(www_path, "lab.css"))),
    error = function(e) 0L)
  css_href <- sprintf("yersinia-www/lab.css?v=%d", css_stamp)

  ui <- bslib::page_navbar(
    title = "yersinia Virtual Lab",
    theme = bslib::bs_theme(version = 5, preset = "shiny"),
    fillable = TRUE,
    # Explore is listed first and named as `selected` explicitly: relying on
    # position alone silently stops being true the moment a panel is inserted
    # above it.
    selected = "Explore",
    header = shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css",
                       href = css_href)
    ),
    bslib::nav_panel(
      "Explore", icon = shiny::icon("table-list"),
      explore_ui("explore")
    ),
    bslib::nav_panel(
      "Lab", icon = shiny::icon("flask"),
      bslib::layout_sidebar(
        fillable = TRUE,
        sidebar = bslib::sidebar(
          width = 340,
          class = "yl-sidebar",
          cohort_ui("cohort"),
          model_accordion_ui("model"),
          priors_accordion_ui("priors")
        ),
        shiny::div(
          class = "yl-main",
          shiny::div(class = "yl-hero-wrap", hero_ui("hero")),
          diag_strip_ui("diag"),
          status_bar_ui("status")
        )
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

  server <- function(input, output, session) {
    lab <- saved_session %||% LabSession$new()

    # Reopen the last fit so the lab behaves like a library rather than a
    # blank slate each launch. Wrapped end-to-end: a corrupt or half-written
    # entry must never stop the app from starting, so any failure just
    # leaves the fresh empty session in place.
    #
    # isolate() is load-bearing, not decoration: LabSession's fields are
    # shiny::reactiveVal()s, and *reading* one outside a reactive context
    # throws "Operation not allowed without an active reactive context".
    # This block runs at server start, which is not a reactive context, so
    # without isolate() the restore threw, the tryCatch below swallowed it,
    # and the app opened empty with no notification and no clue why. The
    # library modal's own load worked all along because observeEvent() IS a
    # reactive context — which is exactly why this failed only on a fresh
    # open.
    if (is.null(saved_session) && isTRUE(restore_last)) {
      restored <- tryCatch(shiny::isolate({
        slug <- lab_library_last(library_dir)
        if (is.null(slug)) NULL else {
          lab_library_load(lab, slug, library_dir = library_dir)
          slug
        }
      }), error = function(e) {
        warning("Restoring the last fit failed: ", conditionMessage(e),
                call. = FALSE)
        NULL
      })
      if (!is.null(restored)) {
        st <- shiny::isolate(lab$fit_state)
        no_fan <- !is.null(st$samples) && is.null(st$setup)
        shiny::showNotification(
          shiny::span(shiny::icon("clock-rotate-left"), " Reopened last fit: ",
                      shiny::tags$strong(restored),
                      shiny::tags$br(),
                      shiny::tags$small(class = "text-muted",
                                        "Open the library to pick another.")),
          type = "default", duration = 6)
        # Say so rather than showing a fan-less hero and leaving the user to
        # wonder whether the fit or the plot is broken.
        if (no_fan) {
          shiny::showNotification(
            shiny::span(shiny::icon("triangle-exclamation"),
                        " Trajectories unavailable: this fit's model setup ",
                        "could not be rebuilt. Posterior diagnostics still work."),
            type = "warning", duration = 10)
        }
      }
    }

    explore_server("explore", lab_session = lab)
    cohort_server("cohort",  lab_session = lab)
    model_accordion_server("model",  lab_session = lab)
    priors_accordion_server("priors", lab_session = lab)
    hero_server("hero",   lab_session = lab)
    diag_strip_server("diag",   lab_session = lab)
    status_bar_server("status", lab_session = lab, library_dir = library_dir)
  }

  shiny::shinyApp(ui, server)
}
