# R/plague_utils.R

#' Load and validate plague model parameters
#' @param param_file Path to YAML parameter file
#' @param validate Logical, whether to validate parameters
#' @return List of validated parameters
load_plague_parameters <- function(param_file = NULL, validate = TRUE) {
  if (is.null(param_file)) {
    warning("No parameter file specified, using default parameters")
    params <- list(
      K_r = 2500,    # Rat carrying capacity
      r_r = 5.0,     # Rat population growth rate
      p = 0.975,     # Probability of inherited resistance
      d_r = 0.2,     # Natural death rate of rats
      beta_r = 4.7,  # Rat infection rate from fleas
      a = 0.004,     # Flea search efficiency
      m_r = 20.0,    # Infected rat mortality rate
      g_r = 0.02,    # Probability rat survives infection
      r_f = 20.0,    # Flea reproduction rate
      K_f = 6.57,    # Flea carrying capacity per rat
      d_f = 10.0     # Death rate of free fleas
    )
  } else {
    params <- yaml::read_yaml(param_file)
  }

  if (validate) {
    validate_parameters(params)
  }

  return(params)
}

#' Validate plague model parameters
#' @param params List of parameters
#' @return TRUE if valid, stops with error if invalid
validate_parameters <- function(params) {
  required_params <- c("K_r", "r_r", "p", "d_r", "beta_r", "a",
                       "m_r", "g_r", "r_f", "K_f", "d_f")

  # Check for missing parameters
  missing_params <- setdiff(required_params, names(params))
  if (length(missing_params) > 0) {
    stop("Missing required parameters: ", paste(missing_params, collapse = ", "))
  }

  # Validate parameter ranges
  if (params$p < 0 || params$p > 1) stop("p must be between 0 and 1")
  if (params$g_r < 0 || params$g_r > 1) stop("g_r must be between 0 and 1")
  if (any(unlist(params) < 0)) stop("All parameters must be non-negative")

  TRUE
}

#' Create simulation timepoints
#' @param years Number of years to simulate
#' @param timestep Time step in years
#' @return Vector of timepoints
create_timepoints <- function(years = 40, timestep = 1/365) {
  seq(0, years, by = timestep)
}

#' Run plague model simulation
#' @param params List of model parameters
#' @param times Vector of timepoints
#' @param seasonal Logical, whether to include seasonal forcing
#' @param seasonal_amplitude Amplitude of seasonal forcing if used
#' @return Tibble with simulation results
run_plague_simulation <- function(params, times = NULL,
                                  seasonal = FALSE, seasonal_amplitude = 0.2) {
  if (is.null(times)) {
    times <- create_timepoints()
  }

  if (seasonal) {
    model <- plague_model_seasonal$new(
      user = c(params, list(seasonal_amplitude = seasonal_amplitude))
    )
  } else {
    model <- plague_model$new(user = params)
  }

  output <- model$run(times) |>
    as_tibble()

  return(output)
}

#' Plot plague simulation results
#' @param output Simulation output tibble
#' @param log_scale Logical, whether to use log scale
#' @param plot_type One of "all", "infected", or "phase"
#' @return ggplot object
plot_plague_simulation <- function(output, log_scale = FALSE,
                                   plot_type = "all") {
  if (plot_type == "all") {
    p <- output |>
      pivot_longer(-t) |>
      ggplot(aes(t, value)) +
      geom_line() +
      facet_wrap(~name, scales = 'free_y') +
      labs(
        title = "Plague Model Simulation",
        x = "Time (years)",
        y = "Population"
      )
  } else if (plot_type == "infected") {
    p <- output |>
      ggplot(aes(t, I_r)) +
      geom_line() +
      labs(
        title = "Infected Rat Population",
        x = "Time (years)",
        y = "Number of Infected Rats"
      )
  } else if (plot_type == "phase") {
    p <- output |>
      ggplot(aes(S_r, I_r)) +
      geom_path() +
      labs(
        title = "Phase Portrait",
        x = "Susceptible Rats",
        y = "Infected Rats"
      )
  }

  if (log_scale && plot_type != "phase") {
    p <- p + scale_y_log10()
  }

  p + theme_minimal()
}

#' Run sensitivity analysis
#' @param base_params Base parameter list
#' @param param_name Parameter to vary
#' @param range Vector of multipliers
#' @param seasonal Include seasonal forcing
#' @return Tibble with sensitivity analysis results
run_sensitivity_analysis <- function(base_params, param_name,
                                     range = seq(0.5, 1.5, by = 0.1),
                                     seasonal = FALSE) {
  results <- map_dfr(range, function(mult) {
    params <- base_params
    params[[param_name]] <- params[[param_name]] * mult

    sim <- run_plague_simulation(params, seasonal = seasonal)
    sim |>
      mutate(
        multiplier = mult,
        parameter = param_name
      )
  })

  return(results)
}

#' Plot sensitivity analysis results
#' @param sensitivity_results Output from run_sensitivity_analysis
#' @param variable Variable to plot
#' @return ggplot object
plot_sensitivity <- function(sensitivity_results, variable = "I_r") {
  sensitivity_results |>
    ggplot(aes(t, .data[[variable]], group = multiplier, color = multiplier)) +
    geom_line(alpha = 0.7) +
    scale_color_viridis_c() +
    labs(
      title = paste("Sensitivity Analysis:", variable),
      x = "Time (years)",
      y = variable,
      color = "Parameter\nMultiplier"
    ) +
    theme_minimal()
}

#' Calculate model statistics
#' @param output Simulation output
#' @return List of statistics
calculate_model_stats <- function(output) {
  list(
    peak_infected = max(output$I_r),
    final_susceptible = tail(output$S_r, 1),
    final_resistant = tail(output$R_r, 1),
    average_flea_index = mean(output$N),
    min_total_rats = min(output$S_r + output$I_r + output$R_r)
  )
}

#' Calculate basic reproduction number (R0)
#' @param params Parameter list
#' @return Numeric R0 value
calculate_R0 <- function(params) {
  with(params, {
    beta_r * K_r * (1 - exp(-a * K_r)) / (d_r + m_r)
  })
}

#' Create seasonal forcing
#' @param times Vector of timepoints
#' @param amplitude Seasonal amplitude
#' @return Vector of seasonal multipliers
create_seasonal_forcing <- function(times, amplitude = 0.2) {
  sin(2 * pi * ((times * 365) %% 365) / 365) * amplitude
}

#' Save parameter set to YAML
#' @param params Parameter list
#' @param file Filename to save to
#' @return Invisibly returns the parameter list
save_parameters <- function(params, file) {
  yaml::write_yaml(params, file)
  invisible(params)
}

#' Format parameters for display
#' @param params Parameter list
#' @return Character string with formatted parameters
format_parameters <- function(params) {
  param_names <- c(
    K_r = "Rat carrying capacity",
    r_r = "Rat growth rate",
    p = "Inherited resistance prob.",
    d_r = "Rat death rate",
    beta_r = "Infection rate",
    a = "Flea search efficiency",
    m_r = "Recovery rate",
    g_r = "Survival probability",
    r_f = "Flea growth rate",
    K_f = "Flea carrying capacity",
    d_f = "Flea death rate"
  )

  paste(sprintf("%s (%s): %g",
                param_names[names(params)],
                names(params),
                unlist(params)),
        collapse = "\n")
}
