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
      # Browse the on-disk library of saved fits (see R/app_library.R).
      shiny::actionButton(ns("library_open"), label = NULL,
                          icon = shiny::icon("book"),
                          class = "btn btn-light btn-sm yl-iconbtn",
                          title = "Fit library"),
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
        shiny::tags$hr(style = "margin: 0.4rem 0;"),
        shiny::checkboxInput(
          ns("autosave"),
          shiny::span(
            shiny::tags$strong("Autosave completed fits"),
            shiny::tags$br(),
            shiny::tags$small(class = "text-muted",
              "Every finished fit is written to the library as its own ",
              "versioned entry. Nothing is overwritten.")
          ),
          value = TRUE
        ),
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
status_bar_server <- function(id, lab_session, library_dir = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    lib_dir <- library_dir %||% lab_library_dir()
    # Bumped to force the library table to re-read after a save or delete.
    lib_tick <- shiny::reactiveVal(0L)
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

    # ---- Fit library ---------------------------------------------------
    # Same payload as the download/upload buttons above, written to a known
    # directory instead of through the browser. lab_library_list() reads only
    # the metadata sidecars, so this stays fast as the library grows.
    # All entries on disk, and the filtered view the table actually shows.
    # Keeping them separate matters: row selection indexes into the filtered
    # frame, so resolving a slug must use the same frame the user clicked.
    library_all <- shiny::reactive({
      lib_tick()
      lab_library_list(lib_dir)
    })

    library_rows <- shiny::reactive({
      rows <- library_all()
      if (isTRUE(input$library_show_auto)) return(rows)
      rows[!rows$autosaved, , drop = FALSE]
    })

    output$library_table <- DT::renderDT({
      rows <- library_rows()
      if (nrow(rows) == 0) {
        msg <- if (nrow(library_all()) > 0) {
          "No named fits — tick 'Show autosaves' to see automatic ones."
        } else {
          "No saved fits yet."
        }
        return(DT::datatable(
          data.frame(` ` = msg, check.names = FALSE),
          rownames = FALSE, options = list(dom = "t")))
      }
      shown <- data.frame(
        Name     = rows$name,
        Source   = ifelse(rows$autosaved, "auto", "saved"),
        Saved    = format(rows$saved_at, "%Y-%m-%d %H:%M"),
        Cohort   = rows$cohort_label,
        Outbreaks = rows$n_groups,
        Scenario = rows$scenario,
        Mode     = rows$mode,
        Chains   = rows$n_chains,
        Iter     = rows$n_iter,
        `Max Rhat` = ifelse(is.na(rows$max_rhat), "—",
                            sprintf("%.3f", rows$max_rhat)),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      DT::datatable(shown, rownames = FALSE, selection = "multiple",
                    options = list(pageLength = 8, dom = "tp", scrollX = TRUE))
    })

    .library_modal <- function() {
      shiny::showModal(shiny::modalDialog(
        title = "Fit library",
        shiny::p(class = "text-muted",
                 "Saved fits live in ", shiny::tags$code(lib_dir),
                 ". Select a row to load it — this replaces your current ",
                 "cohort, model config, priors, and fit."),
        shiny::checkboxInput(ns("library_show_auto"),
                             "Show autosaves", value = FALSE),
        shiny::checkboxInput(
          ns("library_split_stages"),
          shiny::span("Compare pilot and production separately",
                      shiny::tags$small(class = "text-muted",
                        " \u2014 a stochastic run keeps its pilot; tick to plot both")),
          value = FALSE),
        DT::DTOutput(ns("library_table")),
        shiny::tags$hr(),
        shiny::div(
          class = "d-flex gap-2 align-items-end",
          shiny::div(
            style = "flex: 1 1 auto;",
            shiny::textInput(ns("library_name"), "Name",
                             placeholder = "e.g. barcelona-seasonal")
          ),
          shiny::actionButton(ns("library_save"), "Save current",
                              icon = shiny::icon("floppy-disk"),
                              class = "btn btn-outline-primary btn-sm"),
          shiny::actionButton(ns("library_promote"), "Rename selected",
                              icon = shiny::icon("bookmark"),
                              class = "btn btn-outline-secondary btn-sm",
                              title = paste("Give the selected entry this",
                                            "name and clear its autosave flag"))
        ),
        footer = shiny::tagList(
          shiny::actionButton(ns("library_compare"), "Compare selected",
                              icon = shiny::icon("layer-group"),
                              class = "btn btn-outline-primary btn-sm",
                              title = paste("Overlay 2-", lab_compare_max(),
                                            "fits on shared axes")),
          shiny::actionButton(ns("library_delete"), "Delete selected",
                              icon = shiny::icon("trash"),
                              class = "btn btn-outline-danger btn-sm"),
          shiny::actionButton(ns("library_load"), "Load selected",
                              icon = shiny::icon("upload"),
                              class = "btn btn-primary btn-sm"),
          shiny::modalButton("Close")
        ),
        easyClose = TRUE, size = "l"
      ))
    }

    shiny::observeEvent(input$library_open, {
      lib_tick(lib_tick() + 1L)
      .library_modal()
    })

    shiny::observeEvent(input$library_save, {
      nm <- trimws(input$library_name %||% "")
      if (!nzchar(nm)) {
        shiny::showNotification("Give the fit a name first.", type = "warning")
        return()
      }
      ok <- tryCatch({
        lab_library_save(lab_session, nm, library_dir = lib_dir)
        TRUE
      }, error = function(e) {
        shiny::showNotification(paste("Save failed:", conditionMessage(e)),
                                type = "error")
        FALSE
      })
      if (ok) {
        lib_tick(lib_tick() + 1L)
        shiny::updateTextInput(session, "library_name", value = "")
        shiny::showNotification(sprintf("Saved '%s' to the library.", nm),
                                type = "default")
      }
    })

    # Resolve the DT selection to slugs. Load / rename / delete act on exactly
    # one row; compare wants several.
    .selected_slugs <- function() {
      rows <- library_rows()
      i <- input$library_table_rows_selected
      if (length(i) == 0 || nrow(rows) == 0) return(character(0))
      rows$slug[i]
    }

    .selected_slug <- function() {
      sl <- .selected_slugs()
      if (length(sl) == 0) {
        shiny::showNotification("Pick a row first.", type = "warning")
        return(NULL)
      }
      if (length(sl) > 1) {
        shiny::showNotification("Pick just one row for this action.",
                                type = "warning")
        return(NULL)
      }
      sl[[1]]
    }

    shiny::observeEvent(input$library_load, {
      slug <- .selected_slug()
      if (is.null(slug)) return()
      ok <- tryCatch({
        lab_library_load(lab_session, slug, library_dir = lib_dir)
        TRUE
      }, error = function(e) {
        shiny::showNotification(paste("Load failed:", conditionMessage(e)),
                                type = "error")
        FALSE
      })
      if (ok) {
        shiny::removeModal()
        st <- lab_session$fit_state
        # lab_library_load() rebuilds the dust2 setup; say so when it couldn't,
        # since that's the difference between "can promote" and "must refit".
        if (!is.null(st$samples) && is.null(st$setup)) {
          shiny::showNotification(
            paste("Fit restored, but its model setup couldn't be rebuilt —",
                  "refit before running production."),
            type = "warning", duration = 8)
        } else {
          shiny::showNotification("Fit restored from library.",
                                  type = "default")
        }
      }
    })

    # ---- Compare ---------------------------------------------------------
    # Selected entries, held so the compare modal keeps showing the same fits
    # even if the table's selection changes underneath it.
    # Held as a {slug, stage} spec so the modal keeps showing the same series
    # even if the table's selection changes underneath it.
    compare_spec <- shiny::reactiveVal(NULL)

    compare_draws <- shiny::reactive({
      sp <- compare_spec()
      shiny::req(length(sp$slug) > 0)
      lab_compare_draws(sp$slug, library_dir = lib_dir, stages = sp$stage)
    })

    compare_traj <- shiny::reactive({
      sp <- compare_spec()
      shiny::req(length(sp$slug) > 0)
      lab_compare_trajectories(sp$slug, library_dir = lib_dir,
                               stages = sp$stage)
    })

    output$compare_traj <- shiny::renderPlot({
      tr <- compare_traj()
      # Observed points come from the first fit's cohort. Overlaying fits of
      # different outbreaks is allowed (faceted by group), so this is only a
      # backdrop, not an assertion that they share data.
      obs <- tryCatch({
        s <- .compare_session(compare_spec()$slug[[1]], lib_dir)
        cohort_data(s$cohort_ids)
      }, error = function(e) NULL)
      .compare_traj_plot(tr, obs)
    })

    output$compare_dens <- shiny::renderPlot({
      .compare_dens_plot(compare_draws())
    })

    shiny::outputOptions(output, "compare_traj", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "compare_dens", suspendWhenHidden = FALSE)

    shiny::observeEvent(input$library_compare, {
      sl <- .selected_slugs()
      if (length(sl) < 2) {
        shiny::showNotification(
          "Select at least two rows to compare (ctrl/cmd-click).",
          type = "warning")
        return()
      }
      sp <- if (isTRUE(input$library_split_stages)) {
        lab_compare_expand_stages(sl, library_dir = lib_dir)
      } else {
        list(slug = sl, stage = rep("canonical", length(sl)))
      }
      # The cap is on series, not rows: splitting stages can double them.
      if (length(sp$slug) > lab_compare_max()) {
        shiny::showNotification(
          sprintf("That is %d series and the limit is %d — deselect a fit, or untick the pilot split.",
                  length(sp$slug), lab_compare_max()),
          type = "warning")
        return()
      }
      compare_spec(sp)

      n_facets <- length(unique(compare_draws()$variable))
      skipped <- attr(compare_traj(), "skipped")

      shiny::showModal(shiny::modalDialog(
        title = sprintf("Comparing %d series", length(sp$slug)),
        if (length(skipped) > 0) {
          shiny::div(
            class = "alert alert-warning py-2",
            shiny::icon("triangle-exclamation"),
            sprintf(" No trajectories for: %s. Their model setup could not be rebuilt; densities are still shown.",
                    paste(skipped, collapse = ", "))
          )
        },
        shiny::tags$h6(class = "yl-diag-plots-subhead",
                       "Posterior trajectories"),
        shiny::plotOutput(ns("compare_traj"), height = "380px"),
        shiny::tags$hr(),
        shiny::tags$h6(class = "yl-diag-plots-subhead",
                       "Marginal posteriors"),
        shiny::plotOutput(ns("compare_dens"),
                          height = paste0(.compare_height(n_facets), "px")),
        easyClose = TRUE, size = "xl",
        footer = shiny::modalButton("Close")
      ))
    })

    # Promote an autosave (or rename any entry) to a named keeper.
    shiny::observeEvent(input$library_promote, {
      slug <- .selected_slug()
      if (is.null(slug)) return()
      nm <- trimws(input$library_name %||% "")
      if (!nzchar(nm)) {
        shiny::showNotification("Type a name to rename it to.",
                                type = "warning")
        return()
      }
      ok <- tryCatch({
        lab_library_promote(slug, nm, library_dir = lib_dir)
        TRUE
      }, error = function(e) {
        shiny::showNotification(paste("Rename failed:", conditionMessage(e)),
                                type = "error")
        FALSE
      })
      if (ok) {
        lib_tick(lib_tick() + 1L)
        shiny::updateTextInput(session, "library_name", value = "")
        shiny::showNotification(sprintf("Kept as '%s'.", nm),
                                type = "default")
      }
    })

    shiny::observeEvent(input$library_delete, {
      slug <- .selected_slug()
      if (is.null(slug)) return()
      lab_library_delete(slug, library_dir = lib_dir)
      lib_tick(lib_tick() + 1L)
      shiny::showNotification("Deleted.", type = "default")
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
          # Promotion produces a genuinely different posterior from the
          # pilot, so it earns its own library entry too.
          if (isTRUE(input$autosave)) {
            lab_library_autosave(lab_session, library_dir = lib_dir)
            lib_tick(lib_tick() + 1L)
          }
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
            # Versioned autosave: a new library entry per run, never an
            # overwrite. lab_library_autosave() swallows its own errors so a
            # failed write can't turn a good fit into a visible error.
            if (isTRUE(input$autosave)) {
              lab_library_autosave(lab_session, library_dir = lib_dir)
              lib_tick(lib_tick() + 1L)
            }
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
