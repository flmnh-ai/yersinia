# ------------------------------------------------------------------------------
# app_mod_status.R — Shiny module: sticky bottom status bar with Run/Refit.
#
# Spans the full app width below the rail + hero. Left side shows the
# current fit status (idle / running / done / error). Right side has the
# advanced-fit popover (n_chains, n_iter) and the primary Run/Refit button.
#
# Synchronous fit (lab_fit_run blocks). Progress comes from withProgress.
# v2 could swap in monty_runner_callr for parallel + cancellable chains.
# ------------------------------------------------------------------------------

#' Status bar module — UI.
#'
#' @param id Module namespace id.
#' @return A `shiny::div()` styled as a sticky bottom bar.
#' @export
status_bar_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "yl-statusbar",
    shiny::div(class = "yl-statusbar-msg",
               shiny::uiOutput(ns("msg"), inline = TRUE)),
    shiny::div(class = "yl-statusbar-actions",
      # Save current session to an RDS file the user can re-open later.
      shiny::downloadButton(ns("save"), label = NULL,
                            icon = shiny::icon("download"),
                            class = "btn btn-light btn-sm yl-iconbtn",
                            title = "Save session"),
      # Open a modal to upload a previously-saved RDS — overwrites state.
      shiny::actionButton(ns("load_open"), label = NULL,
                          icon = shiny::icon("upload"),
                          class = "btn btn-light btn-sm yl-iconbtn",
                          title = "Load session"),
      bslib::popover(
        shiny::actionButton(ns("settings"), label = NULL,
                            icon = shiny::icon("gear"),
                            class = "btn btn-light btn-sm yl-iconbtn",
                            title = "Fit settings"),
        shiny::numericInput(ns("n_chains"), "Chains",
                            value = 4L, min = 1L, max = 16L, step = 1L),
        shiny::numericInput(ns("n_iter"), "Pilot iter per chain",
                            value = 30000L, min = 100L, max = 200000L,
                            step = 1000L),
        shiny::tags$hr(style = "margin: 0.4rem 0;"),
        shiny::checkboxInput(
          ns("stochastic"),
          shiny::span(
            shiny::tags$strong("Run stochastic production after pilot"),
            shiny::tags$br(),
            shiny::tags$small(class = "text-muted",
              "Adds a short particle-filtered chain warm-started from ",
              "the pilot's tuned VCV. Slower but gives the canonical posterior.")
          ),
          value = FALSE
        ),
        shiny::numericInput(ns("prod_n_iter"), "Production iter per chain",
                            value = 5000L, min = 500L, max = 50000L,
                            step = 500L),
        shiny::numericInput(ns("prod_n_particles"), "Filter particles",
                            value = 500L, min = 50L, max = 5000L,
                            step = 50L),
        title = "Fit settings"
      ),
      # Conditionally rendered: appears when a deterministic pilot is
      # loaded/finished and could be promoted to a stochastic production
      # without re-running the pilot.
      shiny::uiOutput(ns("promote_btn"), inline = TRUE),
      shiny::actionButton(ns("run"), "Run pilot", icon = shiny::icon("play"),
                          class = "btn btn-primary")
    )
  )
}

# Friendly summary of a fit_state list for the bar's left side.
.status_msg <- function(st) {
  if (is.null(st) || is.null(st$status) || st$status == "idle") {
    return(shiny::tags$span(class = "text-muted", "Ready to run a pilot."))
  }
  if (st$status == "running") {
    return(shiny::tags$span(
      shiny::icon("spinner", class = "fa-spin"), " Running fit…"))
  }
  if (st$status == "error") {
    return(shiny::tags$span(class = "text-danger",
      shiny::icon("triangle-exclamation"),
      " Fit failed: ", st$error %||% "(no message)"))
  }
  if (st$status == "complete") {
    dur <- if (!is.null(st$duration)) sprintf(" in %.1fs", st$duration) else ""
    mode_lbl <- if (identical(st$mode, "stochastic")) {
      sprintf(" — pilot %d + stochastic %d iter", st$n_iter %||% NA,
              st$prod_n_iter %||% NA)
    } else {
      sprintf(" (%d chains × %d iter)", st$n_chains %||% NA, st$n_iter %||% NA)
    }
    return(shiny::tags$span(class = "text-success",
      shiny::icon("check"),
      " Fit complete", mode_lbl, dur, "."))
  }
  shiny::tags$span(class = "text-muted", st$status)
}

#' Status bar module — server.
#'
#' @param id Module namespace id.
#' @param lab_session A [LabSession] instance.
#' @return Invisibly, the moduleServer result.
#' @export
status_bar_server <- function(id, lab_session) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$msg <- shiny::renderUI(.status_msg(lab_session$fit_state))

    # Save: serialise to_list() and offer as a download.
    output$save <- shiny::downloadHandler(
      filename = function() {
        sprintf("yersinia-session-%s.rds",
                format(Sys.time(), "%Y%m%d-%H%M%S"))
      },
      content = function(file) {
        saveRDS(lab_session$to_list(), file)
      }
    )

    # Load: open a modal with a file input, then on upload restore state.
    shiny::observeEvent(input$load_open, {
      shiny::showModal(shiny::modalDialog(
        title = "Load session",
        shiny::p("Pick a previously saved ",
                 shiny::tags$code(".rds"),
                 " session file. This replaces your current cohort, model ",
                 "config, priors, and most recent fit."),
        shiny::fileInput(ns("load_file"), NULL, accept = ".rds",
                         buttonLabel = "Browse..."),
        footer = shiny::modalButton("Cancel"),
        easyClose = TRUE
      ))
    })
    shiny::observeEvent(input$load_file, {
      f <- input$load_file
      if (is.null(f)) return()
      state <- tryCatch(readRDS(f$datapath),
                        error = function(e) {
                          shiny::showNotification(
                            paste("Load failed:", conditionMessage(e)),
                            type = "error")
                          NULL
                        })
      if (is.null(state)) return()
      lab_session$apply_list(state)
      shiny::removeModal()
      shiny::showNotification("Session restored.", type = "default")
    })

    # Toggle button label between Run pilot / Refit.
    shiny::observe({
      st <- lab_session$fit_state
      lab <- if (!is.null(st$samples)) "Refit" else "Run pilot"
      shiny::updateActionButton(session, "run", label = lab)
    })

    # "Promote to stochastic" button — only when a deterministic fit exists
    # with a pilot setup we can rebuild the filter from.
    output$promote_btn <- shiny::renderUI({
      st <- lab_session$fit_state
      can_promote <- !is.null(st) &&
        identical(st$status, "complete") &&
        !identical(st$mode, "stochastic") &&
        !is.null(st$pilot_samples) &&
        !is.null(st$setup)
      if (!can_promote) return(NULL)
      shiny::actionButton(
        ns("run_production"), "Run production",
        icon = shiny::icon("microscope"),
        class = "btn btn-outline-primary btn-sm yl-iconbtn",
        title = "Run stochastic production warm-started from the existing pilot"
      )
    })

    shiny::observeEvent(input$run_production, {
      st <- lab_session$fit_state
      if (is.null(st$pilot_samples) || is.null(st$setup)) return()
      started <- Sys.time()
      lab_session$fit_state <- modifyList(st, list(
        status = "running", mode = "stochastic", started_at = started
      ))
      tryCatch({
        shiny::withProgress(message = "Stochastic production", value = 0.2, {
          shiny::incProgress(0.1, detail = "particle filter")
          prod_samples <- lab_fit_run_production(
            st$setup, st$pilot_samples,
            n_chains = st$n_chains %||% 4L,
            n_iter = input$prod_n_iter %||% 5000L,
            n_particles = input$prod_n_particles %||% 500L
          )
          shiny::incProgress(0.9, detail = "done")
          lab_session$fit_state <- list(
            status = "complete",
            samples = prod_samples,
            pilot_samples = st$pilot_samples,
            setup = st$setup,
            mode = "stochastic",
            n_chains = st$n_chains, n_iter = st$n_iter,
            prod_n_iter = input$prod_n_iter,
            prod_n_particles = input$prod_n_particles,
            duration = as.numeric(difftime(Sys.time(), started, units = "secs")),
            error = NULL,
            started_at = started, completed_at = Sys.time()
          )
        })
      }, error = function(e) {
        # Keep the pilot intact on failure.
        lab_session$fit_state <- modifyList(st, list(
          status = "error", error = conditionMessage(e),
          completed_at = Sys.time()
        ))
      })
    })

    shiny::observeEvent(input$run, {
      if (length(lab_session$cohort_ids) == 0) {
        lab_session$fit_state <- list(
          status = "error",
          error = "Select at least one outbreak before running a pilot.")
        return()
      }
      started <- Sys.time()
      stochastic <- isTRUE(input$stochastic)
      lab_session$fit_state <- list(status = "running", samples = NULL,
                                     setup = NULL, n_chains = input$n_chains,
                                     n_iter = input$n_iter,
                                     mode = if (stochastic) "stochastic" else "deterministic",
                                     started_at = started)
      tryCatch({
        shiny::withProgress(
          message = if (stochastic) "Fitting (pilot)" else "Fitting",
          value = 0, {
            shiny::incProgress(0.1, detail = "assembling")
            setup <- lab_fit_assemble(lab_session)
            shiny::incProgress(0.1, detail = "pilot sampling")
            pilot <- lab_fit_run(setup, n_chains = input$n_chains,
                                  n_iter = input$n_iter)
            prod_samples <- NULL
            if (stochastic) {
              shiny::setProgress(value = 0.5, message = "Fitting (production)",
                                  detail = "stochastic chain")
              prod_samples <- lab_fit_run_production(
                setup, pilot,
                n_chains = input$n_chains,
                n_iter = input$prod_n_iter %||% 5000L,
                n_particles = input$prod_n_particles %||% 500L
              )
            }
            shiny::incProgress(0.9, detail = "done")
            canonical <- prod_samples %||% pilot
            lab_session$fit_state <- list(
              status = "complete",
              samples = canonical,
              pilot_samples = pilot,
              setup = setup,
              mode = if (stochastic) "stochastic" else "deterministic",
              n_chains = input$n_chains, n_iter = input$n_iter,
              prod_n_iter = if (stochastic) input$prod_n_iter else NULL,
              prod_n_particles = if (stochastic) input$prod_n_particles else NULL,
              duration = as.numeric(difftime(Sys.time(), started, units = "secs")),
              error = NULL,
              started_at = started,
              completed_at = Sys.time()
            )
          })
      }, error = function(e) {
        lab_session$fit_state <- list(
          status = "error", error = conditionMessage(e),
          mode = if (stochastic) "stochastic" else "deterministic",
          started_at = started, completed_at = Sys.time())
      })
    })
  })
}
