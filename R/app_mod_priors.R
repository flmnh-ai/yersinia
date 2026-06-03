# ------------------------------------------------------------------------------
# app_mod_priors.R — Shiny module: prior designer as a rail accordion.
#
# One accordion panel per fitted parameter. Header shows the param's name and
# its current prior in shorthand. Open a panel to edit:
#   - family selector (pill toggle)
#   - numeric inputs for the family's parameters (compact)
#   - live density plot
#
# Reconciles automatically when the fitted-parameter set in model_config
# changes — new params get default priors, removed params get dropped.
# ------------------------------------------------------------------------------

# Short-form prior label for the panel header (e.g. "Uniform(0.001, 0.15)").
.prior_shorthand <- function(prior) {
  if (is.null(prior)) return("—")
  fam <- prior_families()[[prior$family]]
  if (is.null(fam)) return(prior$family)
  args <- prior$params[names(fam$params)]
  fmt <- function(x) {
    if (is.null(x) || !is.finite(x)) return("?")
    if (abs(x) >= 1000 || (abs(x) < 0.01 && x != 0)) formatC(x, format = "g", digits = 3)
    else format(x, digits = 4, trim = TRUE)
  }
  paste0(prior$family, "(", paste(vapply(args, fmt, character(1)), collapse = ", "), ")")
}

# Header for one panel: name + shorthand on the right.
.priors_panel_header <- function(param, prior) {
  shiny::HTML(sprintf(
    "<strong>%s</strong> <span class='text-muted yl-prior-shorthand'>%s</span>",
    param, .prior_shorthand(prior)
  ))
}

# Body controls for one panel: pill family toggle + numeric inputs + density.
.priors_panel_body <- function(ns, param, prior) {
  fam_choices <- names(prior_families())
  family_id <- paste0("family_", param)
  shiny::tagList(
    shiny::div(
      class = "yl-pill-toggle",
      shiny::radioButtons(
        inputId = ns(family_id),
        label = NULL,
        choices = fam_choices,
        selected = prior$family,
        inline = TRUE
      )
    ),
    shiny::uiOutput(ns(paste0("paramui_", param))),
    shiny::plotOutput(ns(paste0("density_", param)), height = "100px")
  )
}

# Family-specific numeric inputs (compact horizontal row).
.priors_numeric_row <- function(ns, param, family, current_params) {
  fam <- prior_families()[[family]]
  if (is.null(fam)) return(NULL)
  inputs <- lapply(names(fam$params), function(pname) {
    spec <- fam$params[[pname]]
    val <- current_params[[pname]] %||% spec$default
    shiny::div(class = "yl-num",
      shiny::numericInput(
        ns(paste0("p_", param, "_", pname)),
        label = spec$label, value = val,
        min = if (is.finite(spec$min)) spec$min else NA,
        max = if (is.finite(spec$max)) spec$max else NA
      )
    )
  })
  shiny::div(class = "yl-num-row", do.call(shiny::tagList, inputs))
}

#' Priors accordion module — UI.
#'
#' @param id Module namespace id.
#' @return A `bslib::card()` whose body is the dynamic priors accordion.
#' @export
priors_accordion_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header(shiny::uiOutput(ns("header"), inline = TRUE)),
    bslib::card_body(
      shiny::uiOutput(ns("accordion"))
    )
  )
}

#' Priors accordion module — server.
#'
#' @param id Module namespace id.
#' @param lab_session A [LabSession] instance.
#' @return Invisibly, the moduleServer result.
#' @export
priors_accordion_server <- function(id, lab_session) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    fitted_params <- shiny::reactive({
      cfg <- lab_session$model_config
      sort(unique(c(cfg$shared, cfg$local)))
    })

    # Keep priors in sync with the fitted set.
    shiny::observe({
      fp <- fitted_params()
      current <- shiny::isolate(lab_session$priors) %||% list()
      keep <- intersect(names(current), fp)
      add  <- setdiff(fp, names(current))
      new_priors <- c(current[keep], priors_default(add))
      lab_session$priors <- new_priors[fp]
    })

    output$header <- shiny::renderUI({
      fp <- fitted_params()
      shiny::HTML(sprintf(
        "<strong>Priors</strong> <span class='text-muted'>%d set</span>",
        length(fp)))
    })

    output$accordion <- shiny::renderUI({
      fp <- fitted_params()
      priors <- shiny::isolate(lab_session$priors)
      if (length(fp) == 0) {
        return(shiny::p(shiny::em(
          "Add at least one shared or local parameter to design priors."
        )))
      }
      panels <- lapply(fp, function(p) {
        bslib::accordion_panel(
          title = .priors_panel_header(p, priors[[p]]),
          value = p,
          .priors_panel_body(ns, p, priors[[p]])
        )
      })
      do.call(bslib::accordion, c(list(open = FALSE), panels))
    })

    # Per-param observers — recreated whenever the fitted set changes.
    shiny::observe({
      fp <- fitted_params()
      for (param_ in fp) {
        local({
          param <- param_
          family_id  <- paste0("family_", param)
          paramui_id <- paste0("paramui_", param)
          density_id <- paste0("density_", param)

          output[[paramui_id]] <- shiny::renderUI({
            fam <- input[[family_id]] %||%
              shiny::isolate(lab_session$priors[[param]]$family)
            if (is.null(fam)) return(NULL)
            current <- shiny::isolate(lab_session$priors[[param]]$params)
            .priors_numeric_row(ns, param, fam, current)
          })

          current_prior <- shiny::reactive({
            fam <- input[[family_id]]
            if (is.null(fam)) return(NULL)
            spec <- prior_families()[[fam]]
            if (is.null(spec)) return(NULL)
            params <- lapply(names(spec$params), function(pname) {
              input[[paste0("p_", param, "_", pname)]]
            })
            names(params) <- names(spec$params)
            if (any(vapply(params, is.null, logical(1)))) return(NULL)
            list(family = fam, params = params)
          })

          output[[density_id]] <- shiny::renderPlot({
            pr <- current_prior()
            shiny::req(pr)
            d <- prior_density(pr)
            shiny::validate(shiny::need(nrow(d) > 0, "Invalid params."))
            ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$density)) +
              ggplot2::geom_area(fill = "#5b9bd5", alpha = 0.35) +
              ggplot2::geom_line(colour = "#1f4e79", linewidth = 0.6) +
              ggplot2::labs(x = NULL, y = NULL) +
              ggplot2::theme_minimal(base_size = 10) +
              ggplot2::theme(
                panel.grid.minor = ggplot2::element_blank(),
                axis.text.y = ggplot2::element_blank(),
                axis.ticks.y = ggplot2::element_blank()
              )
          })

          shiny::observe({
            pr <- current_prior()
            shiny::req(pr)
            existing <- shiny::isolate(lab_session$priors)
            if (!identical(existing[[param]], pr)) {
              existing[[param]] <- pr
              lab_session$priors <- existing
            }
          })
        })
      }
    })
  })
}
