# R/plague_utils.R

#' Load plague model parameters
#' @param param_set Name of parameter set or path to YAML file or list of parameters
#' @param validate Logical, whether to validate parameters
#' @param ... Additional parameters to override
#' @return List of validated parameters
#' @export
load_parameters <- function(param_set = "defaults", validate = TRUE, ...) {
  # Handle different input types
  if (is.list(param_set)) {
    params <- param_set
  } else if (is.character(param_set)) {
    if (file.exists(param_set)) {
      # It's a file path
      params <- yaml::read_yaml(param_set)
    } else {
      # It's a named parameter set
      params <- load_named_parameters(param_set)
    }
  } else {
    stop("param_set must be a character string, file path, or list")
  }
  
  # Override with any additional parameters
  override_params <- list(...)
  if (length(override_params) > 0) {
    params[names(override_params)] <- override_params
  }

  if (validate) {
    validate_parameters(params)
  }

  # Add metadata
  attr(params, "param_set") <- if(is.character(param_set)) param_set else "custom"
  attr(params, "modified") <- length(override_params) > 0
  
  class(params) <- c("plague_parameters", "list")
  return(params)
}

#' Load named parameter sets
#' @param name Name of parameter set
#' @return List of parameters
load_named_parameters <- function(name) {
  # Available parameter sets
  available_sets <- c("defaults", "keeling-gilligan", "modern-estimates", "historical")
  
  if (!name %in% available_sets) {
    stop("Parameter set '", name, "' not found. Available sets: ", 
         paste(available_sets, collapse = ", "))
  }
  
  # Try to load from YAML file first
  # Use system.file for installed packages, fallback to inst/ for development
  yaml_file <- system.file("parameters", paste0(name, ".yaml"), package = "yersinia")
  
  if (yaml_file == "" || !file.exists(yaml_file)) {
    # Fallback for development mode
    yaml_file <- file.path("inst", "parameters", paste0(name, ".yaml"))
  }
  
  if (file.exists(yaml_file)) {
    if (!requireNamespace("yaml", quietly = TRUE)) {
      stop("yaml package required for loading parameter files")
    }
    
    params_full <- yaml::read_yaml(yaml_file)
    
    # Extract just the parameter values (exclude metadata)
    metadata_keys <- c("name", "description", "source", "reference", "doi", 
                      "last_updated", "period", "notes")
    params <- params_full[!names(params_full) %in% metadata_keys]
    
    # Store metadata as attributes
    attr(params, "metadata") <- params_full[intersect(names(params_full), metadata_keys)]
    
    return(params)
  } else {
    # Fallback to hardcoded values if YAML files not found
    warning("YAML file not found for '", name, "', using fallback parameters")
    
    switch(name,
      "defaults" = ,
      "keeling-gilligan" = list(
        K_r = 2500, r_r = 5.0, p = 0.975, d_r = 0.2, beta_r = 4.7, a = 4e-3,
        m_r = 20.0, g_r = 0.02, r_f = 20.0, K_f = 6.57, d_f = 10.0,
        K_h = 5000, r_h = 0.045, d_h = 0.04, beta_h = 0.01, m_h = 26, g_h = 0.1,
        I_ini = 1, mu_r = 0.03, mu_f = 0.008
      ),
      "modern-estimates" = list(
        K_r = 3000, r_r = 4.5, p = 0.96, d_r = 0.25, beta_r = 5.0, a = 5e-3,
        m_r = 18.0, g_r = 0.03, r_f = 22.0, K_f = 7.0, d_f = 12.0,
        K_h = 10000, r_h = 0.05, d_h = 0.035, beta_h = 0.012, m_h = 24, g_h = 0.15,
        I_ini = 1, mu_r = 0.05, mu_f = 0.012
      ),
      "historical" = list(
        K_r = 2000, r_r = 6.0, p = 0.98, d_r = 0.15, beta_r = 6.0, a = 3e-3,
        m_r = 25.0, g_r = 0.01, r_f = 15.0, K_f = 5.0, d_f = 8.0,
        K_h = 3000, r_h = 0.03, d_h = 0.08, beta_h = 0.015, m_h = 35, g_h = 0.05,
        I_ini = 5, mu_r = 0.02, mu_f = 0.005
      )
    )
  }
}

#' Validate plague model parameters
#' @param params List of parameters
#' @return TRUE if valid, stops with error if invalid
validate_parameters <- function(params) {
  # Core rat-flea parameters (always required)
  required_params <- c("K_r", "r_r", "p", "d_r", "beta_r", "a",
                       "m_r", "g_r", "r_f", "K_f", "d_f")

  # Check for missing core parameters
  missing_params <- setdiff(required_params, names(params))
  if (length(missing_params) > 0) {
    stop("Missing required parameters: ", paste(missing_params, collapse = ", "))
  }

  # Validate parameter ranges
  if (params$p < 0 || params$p > 1) stop("p must be between 0 and 1")
  if (params$g_r < 0 || params$g_r > 1) stop("g_r must be between 0 and 1")
  
  # Check human parameters if present
  if ("g_h" %in% names(params)) {
    if (params$g_h < 0 || params$g_h > 1) stop("g_h must be between 0 and 1")
  }
  
  # Check all parameters are non-negative
  numeric_params <- params[sapply(params, is.numeric)]
  if (any(unlist(numeric_params) < 0)) {
    stop("All numeric parameters must be non-negative")
  }

  TRUE
}

#' Print method for plague_parameters
#' @param x plague_parameters object
#' @param ... Additional arguments (ignored)
#' @export
print.plague_parameters <- function(x, ...) {
  cat("Plague Model Parameters\n")
  cat("=======================\n")
  cat("Parameter set:", attr(x, "param_set"), "\n")
  if (attr(x, "modified")) cat("(with user modifications)\n")
  cat("\n")
  
  # Group parameters by category
  rat_params <- x[c("K_r", "r_r", "p", "d_r")]
  disease_params <- x[c("beta_r", "m_r", "g_r")]
  flea_params <- x[c("r_f", "K_f", "d_f", "a")]
  
  cat("Rat population:\n")
  for (name in names(rat_params)) {
    cat(sprintf("  %s: %g\n", name, rat_params[[name]]))
  }
  
  cat("\nDisease dynamics:\n")
  for (name in names(disease_params)) {
    cat(sprintf("  %s: %g\n", name, disease_params[[name]]))
  }
  
  cat("\nFlea dynamics:\n")
  for (name in names(flea_params)) {
    cat(sprintf("  %s: %g\n", name, flea_params[[name]]))
  }
  
  # Show human parameters if present
  human_params <- x[intersect(names(x), c("K_h", "r_h", "d_h", "beta_h", "m_h", "g_h"))]
  if (length(human_params) > 0) {
    cat("\nHuman dynamics:\n")
    for (name in names(human_params)) {
      cat(sprintf("  %s: %g\n", name, human_params[[name]]))
    }
  }
  
  invisible(x)
}

# Plague Results Class --------------------------------------------------------

#' Create a plague_results object
#' @param data Tibble with simulation results
#' @param model_type Type of model used
#' @param params Parameters used in simulation
#' @param run_info Runtime information
#' @return plague_results object
#' @export
new_plague_results <- function(data, model_type, params, run_info = list()) {
  # Validate data structure
  required_cols <- c("time", "compartment", "population", "replicate", "value")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  structure(
    data,
    class = c("plague_results", "tbl_df", "tbl", "data.frame"),
    model_type = model_type,
    params = params,
    run_info = run_info
  )
}

#' Print method for plague_results
#' @param x plague_results object
#' @param ... Additional arguments passed to tibble print
#' @export
print.plague_results <- function(x, ...) {
  cat("Plague Model Results\n")
  cat("====================\n")
  cat("Model type:", attr(x, "model_type"), "\n")
  cat("Parameter set:", attr(attr(x, "params"), "param_set"), "\n")
  
  # Summary statistics
  n_time <- length(unique(as.vector(x$time)))
  n_reps <- length(unique(as.vector(x$replicate)))
  n_pops <- length(unique(as.vector(x$population)))
  compartments <- unique(as.vector(x$compartment))
  
  cat("Time points:", n_time, "\n")
  cat("Replicates:", n_reps, "\n")
  cat("Populations:", n_pops, "\n")
  cat("Compartments:", paste(compartments, collapse = ", "), "\n")
  cat("\n")
  
  # Print data preview
  cat("Data preview:\n")
  NextMethod("print")
  
  invisible(x)
}

#' Summary method for plague_results
#' @param object plague_results object
#' @param ... Additional arguments (ignored)
#' @export
summary.plague_results <- function(object, ...) {
  cat("Plague Model Results Summary\n")
  cat("============================\n")
  
  # Model info
  cat("Model type:", attr(object, "model_type"), "\n")
  cat("Parameter set:", attr(attr(object, "params"), "param_set"), "\n")
  
  # Calculate summary statistics by compartment
  summary_stats <- object |>
    group_by(compartment) |>
    summarise(
      min_value = min(value),
      max_value = max(value),
      mean_value = mean(value),
      final_value = value[which.max(time)],
      .groups = "drop"
    )
  
  cat("\nCompartment summaries:\n")
  print(summary_stats)
  
  # Basic reproduction number if parameters available
  params <- attr(object, "params")
  if (!is.null(params)) {
    R0 <- calculate_R0(params)
    cat("\nBasic reproduction number (R0):", round(R0, 3), "\n")
  }
  
  invisible(object)
}

#' Plot method for plague_results
#' @param x plague_results object
#' @param compartments Vector of compartments to plot (NULL for all)
#' @param log_scale Logical, use log scale for y-axis
#' @param ... Additional arguments (ignored)
#' @export
plot.plague_results <- function(x, compartments = NULL, log_scale = FALSE, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for plotting")
  }
  
  # Filter compartments if specified
  plot_data <- x
  if (!is.null(compartments)) {
    plot_data <- plot_data |> filter(compartment %in% compartments)
  }
  
  # Determine if spatial
  is_spatial <- length(unique(x$population)) > 1
  
  if (is_spatial) {
    # Spatial plot - show by population
    p <- plot_data |>
      ggplot(aes(time, value, color = compartment)) +
      geom_line(alpha = 0.7) +
      facet_wrap(~population, labeller = label_both) +
      labs(
        title = paste("Plague Model Results:", attr(x, "model_type")),
        x = "Time (years)",
        y = "Population",
        color = "Compartment"
      ) +
      theme_minimal()
  } else {
    # Non-spatial plot
    if (length(unique(x$replicate)) > 1) {
      # Multiple replicates - show uncertainty
      summary_data <- plot_data |>
        group_by(time, compartment) |>
        summarise(
          median = median(value),
          lower = quantile(value, 0.025),
          upper = quantile(value, 0.975),
          .groups = "drop"
        )
      
      p <- summary_data |>
        ggplot(aes(time, median, color = compartment)) +
        geom_ribbon(aes(ymin = lower, ymax = upper, fill = compartment), alpha = 0.2) +
        geom_line() +
        labs(
          title = paste("Plague Model Results:", attr(x, "model_type")),
          x = "Time (years)",
          y = "Population",
          color = "Compartment"
        ) +
        theme_minimal()
    } else {
      # Single replicate
      p <- plot_data |>
        ggplot(aes(time, value, color = compartment)) +
        geom_line() +
        labs(
          title = paste("Plague Model Results:", attr(x, "model_type")),
          x = "Time (years)",
          y = "Population",
          color = "Compartment"
        ) +
        theme_minimal()
    }
  }
  
  if (log_scale) {
    p <- p + scale_y_log10()
  }
  
  return(p)
}

# Main Simulation Interface ---------------------------------------------------

#' Run plague model simulation
#' @param params Parameter set name, file path, or list of parameters
#' @param times Vector of time points or NULL for default (0 to 10 years)
#' @param include_humans Logical, include human dynamics (only npop = 1 supported)
#' @param npop Number of populations (1 for single population, >1 for spatial)
#' @param contact_matrix Contact matrix for multi-population models (auto-generated if NULL and npop = 25)
#' @param n_particles Number of particles for stochastic models
#' @param n_threads Number of threads for parallel processing
#' @param seasonal Logical, include seasonal forcing
#' @param ... Additional parameters to override
#' @return plague_results object
#' @export
run_plague_model <- function(params = "defaults",
                             times = NULL,
                             include_humans = FALSE,
                             npop = 1,
                             contact_matrix = NULL,
                             n_particles = 100,
                             n_threads = 1,
                             seasonal = FALSE,
                             ...) {
  
  # Load and validate parameters
  if (inherits(params, "plague_parameters")) {
    model_params <- params
  } else {
    model_params <- load_parameters(params, ...)
  }
  
  # Set default times if not provided
  if (is.null(times)) {
    times <- seq(0, 10, by = 0.1)
  }
  
  # All models are now stochastic - no validation needed
  
  # Multi-population human models not yet implemented
  if (npop > 1 && include_humans) {
    stop("Multi-population human models not yet implemented - use npop = 1 with include_humans = TRUE")
  }
  
  # Generate contact matrix for multi-population models
  if (npop > 1 && is.null(contact_matrix)) {
    # Default to 5x5 grid if npop = 25, otherwise error
    if (npop == 25) {
      contact_matrix <- make_contact_matrix(5, 5)
    } else {
      stop("contact_matrix required when npop > 1 (except npop = 25 defaults to 5x5 grid)")
    }
  } else if (npop > 1) {
    # Validate contact matrix dimensions
    if (nrow(contact_matrix) != npop || ncol(contact_matrix) != npop) {
      stop("contact_matrix must be ", npop, "x", npop, " matrix")
    }
  } else {
    # Single population case
    contact_matrix <- matrix(1, 1, 1)
  }
  
  # Prepare model-specific parameters
  sim_params <- as.list(model_params)
  
  # Always set up parameters for spatial framework (works for npop = 1 too)
  sim_params$npop <- npop
  sim_params$contact <- contact_matrix
  
  # Handle I_ini (needs to be a vector for odin model)
  if ("I_ini" %in% names(sim_params) && length(sim_params$I_ini) == 1) {
    sim_params$I_ini <- c(sim_params$I_ini, rep(0, npop - 1))  # Start infection in population 1
  } else if (!"I_ini" %in% names(sim_params)) {
    sim_params$I_ini <- c(10, rep(0, npop - 1))  # Default initial infection
  }
  
  sim_params$S_ini <- 1
  
  # Distribute carrying capacity across populations
  if (npop > 1) {
    sim_params$K_r <- sim_params$K_r / npop
  }
  
  # Add temporal parameters
  dt <- 1/52  # Weekly timesteps for stochastic models
  sim_params$dt <- dt
  timesteps <- 1:(dt^-1 * max(times))
  
  # Always provide season parameter for stochastic models (even if flat)
  if (seasonal) {
    sim_params$season <- get_seasonal_forcing(timesteps)
  } else {
    # Provide flat seasonal forcing (no variation)
    sim_params$season <- rep(0, length(timesteps))
  }
  
  # Run the appropriate stochastic model
  if (include_humans) {
    # Single-population human model (npop = 1 enforced above)
    results <- run_human_stochastic_model(sim_params, timesteps, n_particles, n_threads)
    model_type <- "stochastic_humans"
  } else {
    # Always use spatial model (works for npop = 1 or npop > 1)
    results <- run_spatial_stochastic_model(sim_params, timesteps, n_particles, n_threads)
    model_type <- if (npop > 1) "stochastic_spatial" else "stochastic_single"
  }
  
  # Create run info
  run_info <- list(
    timestamp = Sys.time(),
    model = "stochastic",
    include_humans = include_humans,
    npop = npop,
    n_particles = n_particles,
    seasonal = seasonal
  )
  
  # Return plague_results object
  new_plague_results(results, model_type, model_params, run_info)
}

# Model-specific runner functions ---------------------------------------------

#' Run spatial stochastic model
#' @param params List of parameters
#' @param timesteps Vector of timesteps
#' @param n_particles Number of particles
#' @param n_threads Number of threads
#' @return Tidy tibble with results
run_spatial_stochastic_model <- function(params, timesteps, n_particles, n_threads) {
  # Clean pipeline - no redundant processing needed
  results <- run_stochastic_simulation(params, timesteps, n_particles, n_threads)
  return(results)
}

#' Run human stochastic model
#' @param params List of parameters
#' @param timesteps Vector of timesteps
#' @param n_particles Number of particles
#' @param n_threads Number of threads
#' @return Tidy tibble with results
run_human_stochastic_model <- function(params, timesteps, n_particles, n_threads) {
  # Initialize model using plague_stochastic_humans.R
  model_path <- system.file("odin", "plague_stochastic_humans.R", package = "yersinia")
  if (model_path == "") {
    # Fallback for development mode
    model_path <- file.path("inst", "odin", "plague_stochastic_humans.R")
  }
  plague_model_humans <- odin.dust::odin_dust(model_path)
  model <- plague_model_humans$new(
    pars = params,
    time = 1L,
    n_particles = n_particles,
    n_threads = n_threads,
    seed = sample.int(.Machine$integer.max, 1)
  )
  
  # Run simulation
  state <- model$simulate(timesteps) |>
    array(
      dim = c(10, n_particles, length(timesteps)),
      dimnames = list(
        compartment = c('S', 'I', 'R', 'D', 'N', 'F', 'Sh', 'Ih', 'Rh', 'Dh'),
        replicate = 1:n_particles,
        time = timesteps * params$dt
      )
    ) |>
    cubelyr::as.tbl_cube(met_name = 'value') |>
    tibble::as_tibble() |>
    dplyr::mutate(population = 1L)  # Single population only
  
  return(state)
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
    tibble::as_tibble()

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
      tidyr::pivot_longer(-t) |>
      ggplot2::ggplot(ggplot2::aes(t, value)) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~name, scales = 'free_y') +
      ggplot2::labs(
        title = "Plague Model Simulation",
        x = "Time (years)",
        y = "Population"
      )
  } else if (plot_type == "infected") {
    p <- output |>
      ggplot2::ggplot(ggplot2::aes(t, I_r)) +
      ggplot2::geom_line() +
      ggplot2::labs(
        title = "Infected Rat Population",
        x = "Time (years)",
        y = "Number of Infected Rats"
      )
  } else if (plot_type == "phase") {
    p <- output |>
      ggplot2::ggplot(ggplot2::aes(S_r, I_r)) +
      ggplot2::geom_path() +
      ggplot2::labs(
        title = "Phase Portrait",
        x = "Susceptible Rats",
        y = "Infected Rats"
      )
  }

  if (log_scale && plot_type != "phase") {
    p <- p + ggplot2::scale_y_log10()
  }

  p + ggplot2::theme_minimal()
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
  results <- purrr::map_dfr(range, function(mult) {
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
    ggplot2::ggplot(ggplot2::aes(t, .data[[variable]], group = multiplier, color = multiplier)) +
    ggplot2::geom_line(alpha = 0.7) +
    ggplot2::scale_color_viridis_c() +
    ggplot2::labs(
      title = paste("Sensitivity Analysis:", variable),
      x = "Time (years)",
      y = variable,
      color = "Parameter\nMultiplier"
    ) +
    ggplot2::theme_minimal()
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

  ggplot2::ggplot() +
    ggplot2::geom_segment(data = edges, ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                 arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")), alpha = 0.5) +
    ggplot2::geom_point(data = positions, ggplot2::aes(x, y), size = 3) +
    ggplot2::scale_x_continuous(breaks = 1:n_cols) +
    ggplot2::scale_y_continuous(breaks = 1:n_rows) +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Metapopulation Connectivity")
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
  results |>
    dplyr::filter(time %in% timepoints, compartment == "I") |>
    dplyr::mutate(
      row = ceiling(subpop/n_cols),
      col = ((subpop-1) %% n_cols) + 1
    ) |>
    ggplot2::ggplot(ggplot2::aes(col, row, fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c(option = "inferno") +
    ggplot2::facet_wrap(~time) +
    ggplot2::coord_fixed() +
    ggplot2::labs(
      title = "Spatial Spread of Infection",
      x = "Column",
      y = "Row",
      fill = "Infected\nRats"
    ) +
    ggplot2::theme_minimal()
}

#' Run stochastic simulation with given parameters
#' @param params List of parameters
#' @param timesteps Vector of timesteps
#' @param n_particles Number of particles (replicates)
#' @param n_threads Number of threads for parallel processing
#' @return Data frame with simulation results
run_stochastic_simulation <- function(params, timesteps, n_particles = 1, n_threads = 1) {
  # Initialize model
  model_path <- system.file("odin", "plague_stochastic.R", package = "yersinia")
  if (model_path == "") {
    # Fallback for development mode
    model_path <- file.path("inst", "odin", "plague_stochastic.R")
  }
  plague_model_stochastic <- odin.dust::odin_dust(model_path)
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
        population = 1:params$npop,
        compartment = c('S', 'I', 'R', 'N', 'F'),
        replicate = 1:n_particles,
        time = timesteps * params$dt
      )
    ) |>
    cubelyr::as.tbl_cube(met_name = 'value') |>
    tibble::as_tibble()

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
  results |>
    dplyr::filter(compartment == "I") |>
    dplyr::group_by(time, rep) |>
    dplyr::summarize(total = sum(value), .groups = "drop") |>
    dplyr::group_by(time) |>
    dplyr::summarize(
      median = median(total),
      lower = quantile(total, 0.025),
      upper = quantile(total, 0.975)
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = time, y = median)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.2) +
    ggplot2::geom_line() +
    ggplot2::scale_y_log10() +
    ggplot2::labs(
      x = "Time (years)",
      y = "Total Infected Population"
    ) +
    ggplot2::theme_minimal()
}

#' Plot dynamics for specific patches
#' @param results Simulation results
#' @param patches Vector of patch indices to plot
#' @param compartments Vector of compartments to plot
#' @return ggplot object
plot_patch_dynamics <- function(results, patches, compartments) {
  results |>
    dplyr::filter(
      subpop %in% patches,
      compartment %in% compartments
    ) |>
    dplyr::group_by(time, subpop, compartment) |>
    dplyr::summarize(
      median = median(value),
      lower = quantile(value, 0.025),
      upper = quantile(value, 0.975),
      .groups = "drop"
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = time, y = median, color = compartment)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper, fill = compartment), alpha = 0.2) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~subpop) +
    ggplot2::labs(
      x = "Time (years)",
      y = "Population"
    ) +
    ggplot2::theme_minimal()
}

#' Analyze outbreak characteristics
#' @param results Simulation results
#' @return Data frame with outbreak statistics
analyze_outbreaks <- function(results) {
  results |>
    dplyr::filter(compartment == "I") |>
    dplyr::group_by(time, rep) |>
    dplyr::summarize(
      total_infected = sum(value),
      n_patches = sum(value > 0),
      .groups = "drop"
    ) |>
    dplyr::group_by(rep) |>
    dplyr::summarize(
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
  ggplot2::ggplot(outbreak_stats, ggplot2::aes(x = peak_infected)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(
      x = "Peak Number of Infected",
      y = "Frequency",
      title = "Distribution of Outbreak Sizes"
    ) +
    ggplot2::theme_minimal()
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
  results <- purrr::map_dfr(range, function(mult) {
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
    ggplot2::ggplot(ggplot2::aes(t, .data[[variable]], group = multiplier, color = multiplier)) +
    ggplot2::geom_line(alpha = 0.7) +
    ggplot2::scale_color_viridis_c() +
    ggplot2::labs(
      title = paste("Sensitivity Analysis:", variable),
      x = "Time (years)",
      y = variable,
      color = "Parameter\nMultiplier"
    ) +
    ggplot2::theme_minimal()
}

#' Calculate intervention effects
#' @param scenario_results Results from multiple scenarios
#' @return Data frame with intervention statistics
calculate_intervention_effects <- function(scenario_results) {
  scenario_results |>
    dplyr::filter(compartment == "I") |>
    dplyr::group_by(scenario, rep) |>
    dplyr::summarize(
      peak_infected = max(sum(value)),
      outbreak_duration = sum(sum(value) > 10) * unique(diff(time)[1]),
      .groups = "drop"
    ) |>
    dplyr::group_by(scenario) |>
    dplyr::summarize(
      mean_peak = mean(peak_infected),
      mean_duration = mean(outbreak_duration),
      peak_reduction = 1 - mean_peak/dplyr::first(mean_peak),
      duration_reduction = 1 - mean_duration/dplyr::first(mean_duration),
      .groups = "drop"
    )
}
