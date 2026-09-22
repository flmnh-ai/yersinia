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
.diag_plot_height <- function(n_pars, per_row = 3, row_px = 150) {
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

# Trace and density panels, built directly from the draws.
#
# These were bayesplot::mcmc_trace() / mcmc_dens_overlay() with a theme added
# on top, and the panels kept coming back with no tick values and no parameter
# names. bayesplot appends its own theme stack (bayesplot_theme_get() plus
# yaxis_text(FALSE), yaxis_title(FALSE), yaxis_ticks(FALSE) and
# xaxis_title(on = n_param == 1)) after building the plot, and layering a
# complete theme over that did not reliably restore the axes.
#
# Rather than keep guessing at another library's theme internals, these two
# plots are now plain ggplot2 over a long data frame. Same content, axes we
# control, one less dependency in the path.

# Draws (any posterior-compatible object) -> long data frame.
.diag_long <- function(d) {
  arr <- posterior::as_draws_array(d)
  vars <- posterior::variables(arr)
  df <- as.data.frame(posterior::as_draws_df(arr))
  out <- do.call(rbind, lapply(vars, function(v) {
    data.frame(variable = v,
               chain = df$.chain,
               iteration = df$.iteration,
               value = df[[v]],
               stringsAsFactors = FALSE)
  }))
  out$variable <- factor(out$variable, levels = vars)
  out$chain <- factor(out$chain)
  out
}

# Per-chain colours. The first four are the validated categorical hues used by
# the compare view, so a chain and a fit series never carry conflicting
# meanings for the same colour. Beyond four chains — unusual — fall back to a
# generated qualitative ramp rather than cycling the validated four, since
# cycling would make two chains indistinguishable.
.diag_chain_colours <- function(n) {
  base <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100")
  if (n <= length(base)) base[seq_len(n)]
  else grDevices::hcl.colors(n, "Dark 3")
}

.diag_plot_theme <- function(base_size = 10) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      legend.position = "top",
      legend.margin = ggplot2::margin(b = 0),
      axis.text  = ggplot2::element_text(size = base_size - 2,
                                         colour = "grey30"),
      axis.ticks = ggplot2::element_line(colour = "grey80"),
      axis.title = ggplot2::element_text(size = base_size - 1),
      strip.text = ggplot2::element_text(size = base_size, face = "bold",
                                         margin = ggplot2::margin(b = 4)),
      panel.spacing = ggplot2::unit(0.9, "lines"),
      plot.margin = ggplot2::margin(4, 8, 4, 4)
    )
}

# Trace: value against iteration, one line per chain, free y per parameter
# (parameters span orders of magnitude; a shared y would flatten most panels).
.diag_trace_plot <- function(long) {
  n_chain <- nlevels(long$chain)
  ggplot2::ggplot(long, ggplot2::aes(x = .data$iteration, y = .data$value,
                                     colour = .data$chain)) +
    ggplot2::geom_line(linewidth = 0.3, alpha = 0.8, na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = .diag_chain_colours(n_chain)) +
    ggplot2::facet_wrap(~ variable, scales = "free_y",
                        labeller = .param_labeller()) +
    ggplot2::labs(x = "Iteration", y = "Value", colour = "Chain") +
    .diag_plot_theme()
}

# Density: one curve per chain per parameter. Chains that disagree here are
# the visual form of a bad R-hat.
.diag_dens_plot <- function(long) {
  n_chain <- nlevels(long$chain)
  ggplot2::ggplot(long, ggplot2::aes(x = .data$value, colour = .data$chain)) +
    ggplot2::geom_density(linewidth = 0.6, na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = .diag_chain_colours(n_chain)) +
    ggplot2::facet_wrap(~ variable, scales = "free",
                        labeller = .param_labeller()) +
    ggplot2::labs(x = "Parameter value", y = "Density", colour = "Chain") +
    .diag_plot_theme()
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
      # Name the posterior in the title: these plots read fit_state$samples,
      # which is the production draws after a stochastic run and the pilot
      # otherwise, and the two can disagree.
      lbl <- .fit_mode_label(lab_session$fit_state)
      shiny::showModal(shiny::modalDialog(
        title = if (is.null(lbl)) "Posterior diagnostics" else {
          sprintf("Posterior diagnostics \u2014 %s", lbl)
        },
        shiny::tags$h6(class = "yl-diag-plots-subhead", "Trace"),
        shiny::plotOutput(ns("trace"), height = h),
        shiny::tags$hr(),
        shiny::tags$h6(class = "yl-diag-plots-subhead",
                       "Posterior density (per chain)"),
        shiny::plotOutput(ns("density"), height = h),
        easyClose = TRUE, size = "xl", footer = shiny::modalButton("Close")
      ))
    })

    long_draws <- shiny::reactive({
      d <- draws()
      if (is.null(d)) return(NULL)
      .diag_long(d)
    })

    output$trace <- shiny::renderPlot({
      l <- long_draws()
      shiny::req(l)
      .diag_trace_plot(l)
    })

    output$density <- shiny::renderPlot({
      l <- long_draws()
      shiny::req(l)
      .diag_dens_plot(l)
    })

    # These outputs only ever live inside a modal, so they are hidden when
    # the module first renders. Shiny suspends hidden outputs; depending on
    # the shiny/bslib version, outputs inserted later via showModal() are not
    # always resumed, which shows up as a correctly-sized but blank plot area
    # with no error. Opting out of suspension keeps them rendering.
    shiny::outputOptions(output, "trace", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "density", suspendWhenHidden = FALSE)
  })
}
