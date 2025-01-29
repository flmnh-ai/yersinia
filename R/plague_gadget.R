# R/plague_gadget.R

#' Create a Shiny gadget for exploring the plague model
#' @param initial_params Optional list of initial parameters
#' @return Modified parameters if Apply is clicked, NULL if Cancel
#' @export
explore_plague_model <- function(initial_params = NULL) {
  library(shiny)
  library(miniUI)
  library(dplyr)
  library(ggplot2)

  if (is.null(initial_params)) {
    initial_params <- load_plague_parameters()
  }

  ui <- miniPage(
    gadgetTitleBar("Plague Model Explorer"),
    miniTabstripPanel(
      miniTabPanel("Model", icon = icon("chart-line"),
                   miniContentPanel(
                     fluidRow(
                       column(8,
                              plotOutput("plague_plot", height = "400px")
                       ),
                       column(4,
                              selectInput("plot_type", "Plot Type",
                                          choices = c("All Variables" = "all",
                                                      "Infected Only" = "infected",
                                                      "Phase Portrait" = "phase")),
                              checkboxInput("log_scale", "Log Scale", value = TRUE),
                              hr(),
                              actionButton("run_sim", "Run Simulation", class = "btn-primary")
                       )
                     )
                   )
      ),

      miniTabPanel("Parameters", icon = icon("sliders"),
                   miniContentPanel(
                     fluidRow(
                       column(6,
                              h4("Rat Parameters"),
                              sliderInput("K_r", "Carrying Capacity (K_r)",
                                          min = 1000, max = 5000, value = initial_params$K_r),
                              sliderInput("r_r", "Growth Rate (r_r)",
                                          min = 1, max = 10, value = initial_params$r_r),
                              sliderInput("p", "Inherited Resistance (p)",
                                          min = 0, max = 1, value = initial_params$p),
                              sliderInput("d_r", "Death Rate (d_r)",
                                          min = 0, max = 1, value = initial_params$d_r)
                       ),
                       column(6,
                              h4("Disease Parameters"),
                              sliderInput("beta_r", "Infection Rate (beta_r)",
                                          min = 0, max = 10, value = initial_params$beta_r),
                              sliderInput("m_r", "Recovery Rate (m_r)",
                                          min = 0, max = 40, value = initial_params$m_r),
                              sliderInput("g_r", "Survival Probability (g_r)",
                                          min = 0, max = 1, value = initial_params$g_r)
                       )
                     ),
                     fluidRow(
                       column(6,
                              h4("Flea Parameters"),
                              sliderInput("K_f", "Flea Capacity (K_f)",
                                          min = 0, max = 20, value = initial_params$K_f),
                              sliderInput("r_f", "Flea Growth (r_f)",
                                          min = 0, max = 40, value = initial_params$r_f),
                              sliderInput("d_f", "Flea Death (d_f)",
                                          min = 0, max = 20, value = initial_params$d_f),
                              sliderInput("a", "Search Efficiency (a)",
                                          min = 0, max = 0.01, value = initial_params$a)
                       ),
                       column(6,
                              h4("Simulation Settings"),
                              numericInput("sim_years", "Simulation Years",
                                           value = 40, min = 1, max = 100),
                              selectInput("time_step", "Time Step",
                                          choices = c("Daily" = 1/365,
                                                      "Weekly" = 1/52,
                                                      "Monthly" = 1/12),
                                          selected = 1/365),
                              checkboxInput("seasonal", "Include Seasonality", value = FALSE),
                              conditionalPanel(
                                condition = "input.seasonal == true",
                                sliderInput("seasonal_amp", "Seasonal Amplitude",
                                            min = 0, max = 1, value = 0.2)
                              )
                       )
                     )
                   )
      ),

      miniTabPanel("Analysis", icon = icon("calculator"),
                   miniContentPanel(
                     fluidRow(
                       column(6,
                              h4("Model Statistics"),
                              verbatimTextOutput("model_stats")
                       ),
                       column(6,
                              h4("Equilibrium Values"),
                              verbatimTextOutput("equilibrium")
                       )
                     ),
                     hr(),
                     fluidRow(
                       column(12,
                              h4("Basic Reproduction Number (R0)"),
                              verbatimTextOutput("r0")
                       )
                     )
                   )
      )
    )
  )

  server <- function(input, output, session) {
    # Reactive values for simulation results
    sim_results <- reactiveVal(NULL)

    # Get current parameter set
    get_current_params <- reactive({
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
    observeEvent(input$run_sim, {
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
    output$plague_plot <- renderPlot({
      req(sim_results())

      results <- sim_results()

      if (input$plot_type == "all") {
        p <- plot_plague_simulation(results, input$log_scale)
      } else if (input$plot_type == "infected") {
        p <- results |>
          ggplot(aes(t, I_r)) +
          geom_line() +
          labs(x = "Time (years)", y = "Infected Rats")
      } else {
        p <- results |>
          ggplot(aes(S_r, I_r)) +
          geom_path() +
          labs(x = "Susceptible Rats", y = "Infected Rats")
      }

      if (input$log_scale && input$plot_type != "phase") {
        p <- p + scale_y_log10()
      }

      p + theme_minimal()
    })

    # Model statistics
    output$model_stats <- renderPrint({
      req(sim_results())
      results <- sim_results()

      cat("Peak infected:", max(results$I_r), "\n")
      cat("Final susceptible:", tail(results$S_r, 1), "\n")
      cat("Final resistant:", tail(results$R_r, 1), "\n")
    })

    # Equilibrium values
    output$equilibrium <- renderPrint({
      params <- get_current_params()
      eq <- calculate_equilibrium(params)
      print(eq)
    })

    # R0 calculation
    output$r0 <- renderPrint({
      params <- get_current_params()
      R0 <- calculate_R0(params)
      cat("R0 =", round(R0, 3))
    })

    # Handle the Done button
    observeEvent(input$done, {
      stopApp(get_current_params())
    })

    # Handle the Cancel button
    observeEvent(input$cancel, {
      stopApp(NULL)
    })
  }

  runGadget(ui, server, viewer = dialogViewer("Plague Model Explorer"))
}
