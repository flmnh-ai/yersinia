# ------------------------------------------------------------------------------
# app_mod_hero.R — Shiny module: the single hero plot.
#
# One ggplot anchors the entire app:
#   - Cohort death curves rendered as points (always, when any selected).
#   - Posterior-draw trajectories rendered as faint lines (when a fit exists).
# Multi-outbreak fits facet by group. Empty states: no cohort -> "Pick an
# outbreak"; cohort but no fit -> just the data with a hint.
#
# The hero is *passive* — it only reads from lab_session. Editing happens in
# the rail; running happens in the status bar.
# ------------------------------------------------------------------------------

#' Hero plot module — UI.
#'
#' @param id Module namespace id.
#' @return A `bslib::card()` with the hero plot.
#' @export
hero_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = TRUE,
    bslib::card_body(
      padding = 8L,
      shiny::plotOutput(ns("plot"), height = "100%")
    )
  )
}

# Build the hero ggplot. Pure function so it's easy to test.
.hero_plot <- function(data, posterior_long = NULL) {
  if (is.null(data) || nrow(data) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = "Pick an outbreak to begin",
                          size = 6, colour = "grey50") +
        ggplot2::theme_void()
    )
  }
  data <- data[!is.na(data$deaths), , drop = FALSE]
  has_post <- !is.null(posterior_long) && nrow(posterior_long) > 0
  p <- ggplot2::ggplot()
  if (has_post) {
    p <- p + ggplot2::geom_line(
      data = posterior_long,
      mapping = ggplot2::aes(x = .data$time, y = .data$mu,
                             group = .data$draw),
      colour = "#1f4e79", alpha = 0.08, linewidth = 0.35, na.rm = TRUE
    )
  }
  subtitle <- if (has_post) {
    sprintf("data + %d posterior draws", length(unique(posterior_long$draw)))
  } else {
    "data only — run a pilot to overlay the posterior trajectory fan"
  }
  p <- p +
    ggplot2::geom_point(
      data = data,
      mapping = ggplot2::aes(x = .data$time, y = .data$deaths),
      size = 1.2, colour = "#222", na.rm = TRUE
    ) +
    ggplot2::labs(x = "Day", y = "Deaths", subtitle = subtitle) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
  groups <- unique(data$group)
  n <- length(groups)
  if (n > 1L) {
    # Square-ish grid: 2 wide for 2-4 outbreaks, 3 wide for 5-9, 4 wide
    # for 10+. Keeps individual panels from getting squished as cohort
    # size grows.
    ncol <- if (n <= 4L) 2L else if (n <= 9L) 3L else 4L
    p <- p + ggplot2::facet_wrap(~ group, scales = "free", ncol = ncol)
  }
  p
}

#' Hero plot module — server.
#'
#' Renders cohort death curves and (when a fit exists) the posterior
#' predictive trajectory fan. Recomputes the fan lazily on `fit_state`
#' changes.
#'
#' @param id Module namespace id (must match [hero_ui()]).
#' @param lab_session A [LabSession] instance.
#' @param n_draws Number of posterior draws to render. Default 80.
#' @return Invisibly, the moduleServer result.
#' @export
hero_server <- function(id, lab_session, n_draws = 80L) {
  shiny::moduleServer(id, function(input, output, session) {

    cohort_long <- shiny::reactive({
      ids <- lab_session$cohort_ids
      if (length(ids) == 0) return(NULL)
      tryCatch(cohort_data(ids), error = function(e) NULL)
    })

    posterior_long <- shiny::reactive({
      st <- lab_session$fit_state
      if (is.null(st$samples) || is.null(st$setup)) return(NULL)
      tryCatch(
        lab_fit_forward_sim(st$setup, st$samples, n_draws = n_draws),
        error = function(e) {
          message("[hero] forward sim failed: ", conditionMessage(e))
          NULL
        }
      )
    })

    output$plot <- shiny::renderPlot({
      .hero_plot(cohort_long(), posterior_long())
    })
  })
}
