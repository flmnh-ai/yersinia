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

# Spatial utility functions

#' Create contact matrix for grid-based metapopulation
#' @param n_rows Number of rows in grid
#' @param n_cols Number of columns in grid
#' @return Normalized contact matrix
make_contact_matrix <- function(n_rows = 5, n_cols = 5) {
  n <- n_rows * n_cols
  m <- matrix(0, n, n)

  for(i in 1:n) {
    # Get row/col position
    row <- ceiling(i/n_cols)
    col <- ((i-1) %% n_cols) + 1

    # Add neighbors
    if(row > 1) m[i, i-n_cols] <- 1  # up
    if(row < n_rows) m[i, i+n_cols] <- 1  # down
    if(col > 1) m[i, i-1] <- 1  # left
    if(col < n_cols) m[i, i+1] <- 1  # right
  }

  # Normalize rows
  m <- t(apply(m, 1, function(x) x/sum(x)))
  return(m)
}

#' Plot connectivity matrix
#' @param contact_matrix Contact matrix
#' @param n_rows Number of rows
#' @param n_cols Number of columns
plot_connectivity <- function(contact_matrix, n_rows, n_cols) {
  positions <- expand.grid(x = 1:n_cols, y = 1:n_rows)
  n <- nrow(positions)

  # Create edge data
  edges <- data.frame()
  for(i in 1:n) {
    for(j in 1:n) {
      if(contact_matrix[i,j] > 0) {
        edges <- rbind(edges, data.frame(
          x = positions$x[i],
          y = positions$y[i],
          xend = positions$x[j],
          yend = positions$y[j]
        ))
      }
    }
  }

  ggplot() +
    geom_segment(data = edges, aes(x = x, y = y, xend = xend, yend = yend),
                 arrow = arrow(length = unit(0.2, "cm")), alpha = 0.5) +
    geom_point(data = positions, aes(x, y), size = 3) +
    scale_x_continuous(breaks = 1:n_cols) +
    scale_y_continuous(breaks = 1:n_rows) +
    coord_fixed() +
    theme_minimal() +
    labs(title = "Metapopulation Connectivity")
}

#' Convert population indices to grid coordinates
#' @param indices Population indices
#' @param n_cols Number of columns
#' @return Data frame with row and column positions
get_grid_positions <- function(indices, n_cols) {
  data.frame(
    row = ceiling(indices/n_cols),
    col = ((indices-1) %% n_cols) + 1
  )
}

#' Create animation of spatial spread
#' @param results Simulation results
#' @param timepoints Vector of timepoints to plot
#' @param n_rows Number of rows
#' @param n_cols Number of columns
animate_spatial_spread <- function(results, timepoints, n_rows, n_cols) {
  results %>%
    filter(time %in% timepoints, compartment == "I") %>%
    mutate(
      row = ceiling(subpop/n_cols),
      col = ((subpop-1) %% n_cols) + 1
    ) %>%
    ggplot(aes(col, row, fill = value)) +
    geom_tile() +
    scale_fill_viridis_c(option = "inferno") +
    facet_wrap(~time) +
    coord_fixed() +
    labs(
      title = "Spatial Spread of Infection",
      x = "Column",
      y = "Row",
      fill = "Infected\nRats"
    ) +
    theme_minimal()
}

#' Run stochastic simulation with given parameters
#' @param params List of parameters
#' @param timesteps Vector of timesteps
#' @param n_particles Number of particles (replicates)
#' @param n_threads Number of threads for parallel processing
#' @return Data frame with simulation results
run_stochastic_simulation <- function(params, timesteps, n_particles = 1, n_threads = 1) {
  # Initialize model
  plague_model_stochastic <- odin_dust('R/plague_stochastic.R')
  model <- plague_model_stochastic$new(
    pars = params,
    time = 1L,
    n_particles = n_particles,
    n_threads = n_threads,
    seed = sample.int(.Machine$integer.max, 1)
  )

  # Run simulation
  state <- model$simulate(timesteps) |>
    array(
      dim = c(params$npop, 5, n_particles, length(timesteps)),
      dimnames = list(
        subpop = 1:params$npop,
        compartment = c('S', 'I', 'R', 'N', 'F'),
        rep = 1:n_particles,
        time = timesteps * params$dt
      )
    ) |>
    cubelyr::as.tbl_cube(met_name = 'value') |>
    as_tibble()

  return(state)
}

# Stochastic simulation analysis functions

#' Generate seasonal forcing
#' @param timesteps Vector of timesteps
#' @param amplitude Amplitude of seasonal forcing
#' @return Vector of seasonal multipliers
get_seasonal_forcing <- function(timesteps, amplitude = 0.2) {
  sin(2 * pi * ((timesteps * 365) %% 365) / 365) * amplitude
}

#' Plot total infected over time with uncertainty
#' @param results Simulation results
#' @return ggplot object
plot_total_infected <- function(results) {
  results %>%
    filter(compartment == "I") %>%
    group_by(time, rep) %>%
    summarize(total = sum(value), .groups = "drop") %>%
    group_by(time) %>%
    summarize(
      median = median(total),
      lower = quantile(total, 0.025),
      upper = quantile(total, 0.975)
    ) %>%
    ggplot(aes(x = time, y = median)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    scale_y_log10() +
    labs(
      x = "Time (years)",
      y = "Total Infected Population"
    ) +
    theme_minimal()
}

#' Plot dynamics for specific patches
#' @param results Simulation results
#' @param patches Vector of patch indices to plot
#' @param compartments Vector of compartments to plot
#' @return ggplot object
plot_patch_dynamics <- function(results, patches, compartments) {
  results %>%
    filter(
      subpop %in% patches,
      compartment %in% compartments
    ) %>%
    group_by(time, subpop, compartment) %>%
    summarize(
      median = median(value),
      lower = quantile(value, 0.025),
      upper = quantile(value, 0.975),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = time, y = median, color = compartment)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = compartment), alpha = 0.2) +
    geom_line() +
    facet_wrap(~subpop) +
    labs(
      x = "Time (years)",
      y = "Population"
    ) +
    theme_minimal()
}

#' Analyze outbreak characteristics
#' @param results Simulation results
#' @return Data frame with outbreak statistics
analyze_outbreaks <- function(results) {
  results %>%
    filter(compartment == "I") %>%
    group_by(time, rep) %>%
    summarize(
      total_infected = sum(value),
      n_patches = sum(value > 0),
      .groups = "drop"
    ) %>%
    group_by(rep) %>%
    summarize(
      peak_infected = max(total_infected),
      peak_patches = max(n_patches),
      duration = sum(total_infected > 10) * unique(diff(time)[1]),
      .groups = "drop"
    )
}

#' Plot outbreak size distribution
#' @param outbreak_stats Output from analyze_outbreaks
#' @return ggplot object
plot_outbreak_distribution <- function(outbreak_stats) {
  ggplot(outbreak_stats, aes(x = peak_infected)) +
    geom_histogram(bins = 30) +
    scale_x_log10() +
    labs(
      x = "Peak Number of Infected",
      y = "Frequency",
      title = "Distribution of Outbreak Sizes"
    ) +
    theme_minimal()
}

#' Run sensitivity analysis (deterministic version)
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

#' Calculate intervention effects
#' @param scenario_results Results from multiple scenarios
#' @return Data frame with intervention statistics
calculate_intervention_effects <- function(scenario_results) {
  scenario_results %>%
    filter(compartment == "I") %>%
    group_by(scenario, rep) %>%
    summarize(
      peak_infected = max(sum(value)),
      outbreak_duration = sum(sum(value) > 10) * unique(diff(time)[1]),
      .groups = "drop"
    ) %>%
    group_by(scenario) %>%
    summarize(
      mean_peak = mean(peak_infected),
      mean_duration = mean(outbreak_duration),
      peak_reduction = 1 - mean_peak/first(mean_peak),
      duration_reduction = 1 - mean_duration/first(mean_duration),
      .groups = "drop"
    )
}
