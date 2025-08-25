# R/plague_gadget.R

#' Create a Shiny gadget for exploring the plague model
#' @param initial_params Optional list of initial parameters
#' @return Modified parameters if Apply is clicked, NULL if Cancel
#' @export
explore_plague_model <- function(initial_params = NULL) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("shiny package required for gadget functionality")
  }
  if (!requireNamespace("miniUI", quietly = TRUE)) {
    stop("miniUI package required for gadget functionality")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package required for gadget functionality")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package required for gadget functionality")
  }

  if (is.null(initial_params)) {
    initial_params <- load_parameters("defaults")
  }

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Plague Model Explorer"),
    miniUI::miniTabstripPanel(
      miniUI::miniTabPanel("Model", icon = shiny::icon("chart-line"),
                   miniUI::miniContentPanel(
                     shiny::fluidRow(
                       shiny::column(8,
                              shiny::plotOutput("plague_plot", height = "400px")
                       ),
                       shiny::column(4,
                              shiny::selectInput("plot_type", "Plot Type",
                                          choices = c("All Variables" = "all",
                                                      "Infected Only" = "infected",
                                                      "Phase Portrait" = "phase")),
                              shiny::checkboxInput("log_scale", "Log Scale", value = TRUE),
                              shiny::hr(),
                              shiny::actionButton("run_sim", "Run Simulation", class = "btn-primary")
                       )
                     )
                   )
      ),

      miniUI::miniTabPanel("Parameters", icon = shiny::icon("sliders"),
                   miniUI::miniContentPanel(
                     shiny::fluidRow(
                       shiny::column(6,
                              shiny::h4("Rat Parameters"),
                              shiny::sliderInput("K_r", "Carrying Capacity (K_r)",
                                          min = 1000, max = 5000, value = initial_params$K_r),
                              shiny::sliderInput("r_r", "Growth Rate (r_r)",
                                          min = 1, max = 10, value = initial_params$r_r),
                              shiny::sliderInput("p", "Inherited Resistance (p)",
                                          min = 0, max = 1, value = initial_params$p),
                              shiny::sliderInput("d_r", "Death Rate (d_r)",
                                          min = 0, max = 1, value = initial_params$d_r)
                       ),
                       shiny::column(6,
                              shiny::h4("Disease Parameters"),
                              shiny::sliderInput("beta_r", "Infection Rate (beta_r)",
                                          min = 0, max = 10, value = initial_params$beta_r),
                              shiny::sliderInput("m_r", "Recovery Rate (m_r)",
                                          min = 0, max = 40, value = initial_params$m_r),
                              shiny::sliderInput("g_r", "Survival Probability (g_r)",
                                          min = 0, max = 1, value = initial_params$g_r)
                       )
                     ),
                     shiny::fluidRow(
                       shiny::column(6,
                              shiny::h4("Flea Parameters"),
                              shiny::sliderInput("K_f", "Flea Capacity (K_f)",
                                          min = 0, max = 20, value = initial_params$K_f),
                              shiny::sliderInput("r_f", "Flea Growth (r_f)",
                                          min = 0, max = 40, value = initial_params$r_f),
                              shiny::sliderInput("d_f", "Flea Death (d_f)",
                                          min = 0, max = 20, value = initial_params$d_f),
                              shiny::sliderInput("a", "Search Efficiency (a)",
                                          min = 0, max = 0.01, value = initial_params$a)
                       ),
                       shiny::column(6,
                              shiny::h4("Simulation Settings"),
                              shiny::numericInput("sim_years", "Simulation Years",
                                           value = 40, min = 1, max = 100),
                              shiny::selectInput("time_step", "Time Step",
                                          choices = c("Daily" = 1/365,
                                                      "Weekly" = 1/52,
                                                      "Monthly" = 1/12),
                                          selected = 1/365),
                              shiny::checkboxInput("seasonal", "Include Seasonality", value = FALSE),
                              shiny::conditionalPanel(
                                condition = "input.seasonal == true",
                                shiny::sliderInput("seasonal_amp", "Seasonal Amplitude",
                                            min = 0, max = 1, value = 0.2)
                              )
                       )
                     )
                   )
      ),

      miniUI::miniTabPanel("Analysis", icon = shiny::icon("calculator"),
                   miniUI::miniContentPanel(
                     shiny::fluidRow(
                       shiny::column(6,
                              shiny::h4("Model Statistics"),
                              shiny::verbatimTextOutput("model_stats")
                       ),
                       shiny::column(6,
                              shiny::h4("Equilibrium Values"),
                              shiny::verbatimTextOutput("equilibrium")
                       )
                     ),
                     shiny::hr(),
                     shiny::fluidRow(
                       shiny::column(12,
                              shiny::h4("Basic Reproduction Number (R0)"),
                              shiny::verbatimTextOutput("r0")
                       )
                     )
                   )
      )
    )
  )

  server <- function(input, output, session) {
    # Reactive values for simulation results
    sim_results <- shiny::reactiveVal(NULL)

    # Get current parameter set
    get_current_params <- shiny::reactive({
      list(
        K_r = input$K_r,
        r_r = input$r_r,
        p = input$p,
        d_r = input$d_r,
        beta_r = input$beta_r,
        m_r = input$m_r,
        g_r = input$g_r,
        K_f = input$K_f,
        r_f = input$r_f,
        d_f = input$d_f,
        a = input$a
      )
    })

    # Run simulation when button is clicked
    shiny::observeEvent(input$run_sim, {
      params <- get_current_params()
      times <- seq(0, input$sim_years, by = as.numeric(input$time_step))

      if (input$seasonal) {
        model <- plague_model_seasonal$new(
          user = c(params, list(seasonal_amplitude = input$seasonal_amp))
        )
      } else {
        model <- plague_model$new(user = params)
      }

      results <- run_simulation(model, params, times)
      sim_results(results)
    })

    # Main plot
    output$plague_plot <- shiny::renderPlot({
      shiny::req(sim_results())

      results <- sim_results()

      if (input$plot_type == "all") {
        p <- plot_plague_simulation(results, input$log_scale)
      } else if (input$plot_type == "infected") {
        p <- results |>
          ggplot2::ggplot(ggplot2::aes(t, I_r)) +
          ggplot2::geom_line() +
          ggplot2::labs(x = "Time (years)", y = "Infected Rats")
      } else {
        p <- results |>
          ggplot2::ggplot(ggplot2::aes(S_r, I_r)) +
          ggplot2::geom_path() +
          ggplot2::labs(x = "Susceptible Rats", y = "Infected Rats")
      }

      if (input$log_scale && input$plot_type != "phase") {
        p <- p + ggplot2::scale_y_log10()
      }

      p + ggplot2::theme_minimal()
    })

    # Model statistics
    output$model_stats <- shiny::renderPrint({
      shiny::req(sim_results())
      results <- sim_results()

      cat("Peak infected:", max(results$I_r), "\n")
      cat("Final susceptible:", tail(results$S_r, 1), "\n")
      cat("Final resistant:", tail(results$R_r, 1), "\n")
    })

    # Equilibrium values
    output$equilibrium <- shiny::renderPrint({
      params <- get_current_params()
      eq <- calculate_equilibrium(params)
      print(eq)
    })

    # R0 calculation
    output$r0 <- shiny::renderPrint({
      params <- get_current_params()
      R0 <- calculate_R0(params)
      cat("R0 =", round(R0, 3))
    })

    # Handle the Done button
    shiny::observeEvent(input$done, {
      shiny::stopApp(get_current_params())
    })

    # Handle the Cancel button
    shiny::observeEvent(input$cancel, {
      shiny::stopApp(NULL)
    })
  }

  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("Plague Model Explorer"))
}
