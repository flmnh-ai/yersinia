# R/plague_utils.R

`%||%` <- function(x, y) if (is.null(x)) y else x


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
  param_set <- attr(x, "param_set") %||% "custom"
  cat("🦠 Plague Parameters (", param_set, ")\n", sep = "")

  # Add description if available from metadata
  metadata <- attr(x, "metadata")
  if (!is.null(metadata) && !is.null(metadata$description)) {
    cat("📄 ", metadata$description, "\n")
  }
  if (!is.null(metadata) && !is.null(metadata$source)) {
    cat("📚 Source: ", metadata$source, "\n")
  }

  cat("\n")

  # Group parameters by biological meaning
  cat("🐀 Rat Population Parameters:\n")
  rat_params <- c("K_r", "r_r", "d_r", "p")
  rat_descriptions <- list(
    K_r = "Rat carrying capacity",
    r_r = "Rat population growth rate (per year)",
    d_r = "Natural death rate of rats (per year)",
    p = "Probability of inherited resistance"
  )

  for (param in rat_params) {
    if (param %in% names(x)) {
      desc <- rat_descriptions[[param]] %||% ""
      cat(sprintf("  %-6s = %8.3f  # %s\n", param, x[[param]], desc))
    }
  }

  cat("\n🦟 Flea Parameters:\n")
  flea_params <- c("K_f", "r_f", "d_f", "a")
  flea_descriptions <- list(
    K_f = "Flea carrying capacity per rat",
    r_f = "Flea reproduction rate (per year)",
    d_f = "Death rate of free fleas (per year)",
    a = "Flea search efficiency"
  )

  for (param in flea_params) {
    if (param %in% names(x)) {
      desc <- flea_descriptions[[param]] %||% ""
      cat(sprintf("  %-6s = %8.3f  # %s\n", param, x[[param]], desc))
    }
  }

  cat("\n🔬 Disease Parameters:\n")
  disease_params <- c("beta_r", "m_r", "g_r")
  disease_descriptions <- list(
    beta_r = "Rat infection rate from fleas (per year)",
    m_r = "Infected rat mortality rate (per year)",
    g_r = "Probability rat survives infection"
  )

  for (param in disease_params) {
    if (param %in% names(x)) {
      desc <- disease_descriptions[[param]] %||% ""
      cat(sprintf("  %-6s = %8.3f  # %s\n", param, x[[param]], desc))
    }
  }

  # Human parameters if present
  human_params <- c("K_h", "r_h", "d_h", "beta_h", "m_h", "g_h")
  has_human_params <- any(human_params %in% names(x))

  if (has_human_params) {
    cat("\n👤 Human Parameters:\n")
    human_descriptions <- list(
      K_h = "Human carrying capacity",
      r_h = "Human population growth rate (per year)",
      d_h = "Natural death rate of humans (per year)",
      beta_h = "Human infection rate from fleas",
      m_h = "Human recovery rate (per year)",
      g_h = "Probability human survives infection"
    )

    for (param in human_params) {
      if (param %in% names(x)) {
        desc <- human_descriptions[[param]] %||% ""
        cat(sprintf("  %-6s = %8.3f  # %s\n", param, x[[param]], desc))
      }
    }
  }

  # Initial conditions and other parameters
  other_params <- setdiff(names(x), c(rat_params, flea_params, disease_params, human_params))
  if (length(other_params) > 0) {
    cat("\n⚙️  Other Parameters:\n")
    other_descriptions <- list(
      I_ini = "Initial number of infected rats",
      mu_r = "Rat movement rate (per year)",
      mu_f = "Flea movement rate (per year)"
    )

    for (param in other_params) {
      desc <- other_descriptions[[param]] %||% ""
      cat(sprintf("  %-6s = %8.3f  # %s\n", param, x[[param]], desc))
    }
  }

  # Calculate and display R0
  R0 <- tryCatch(calculate_R0(x), error = function(e) NA)
  if (!is.na(R0)) {
    cat("\n📈 Basic Reproduction Number (R₀): ", round(R0, 3))
    if (R0 > 1) {
      cat(" ✅ (Disease can spread)")
    } else {
      cat(" ⚠️  (Disease may not persist)")
    }
    cat("\n")
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
  cat("📊 Plague Model Results Summary\n")
  cat("================================\n")

  # Model info
  model_type <- attr(object, "model_type")
  param_set <- attr(attr(object, "params"), "param_set") %||% "custom"
  run_info <- attr(object, "run_info")

  cat("🔬 Model type: ", model_type, "\n")
  cat("📋 Parameter set: ", param_set, "\n")

  if (!is.null(run_info)) {
    if (!is.null(run_info$npop)) cat("🏘️  Populations: ", run_info$npop, "\n")
    if (!is.null(run_info$n_particles)) cat("🎲 Particles: ", run_info$n_particles, "\n")
    if (!is.null(run_info$include_humans) && run_info$include_humans) {
      cat("👤 Includes humans: Yes\n")
    }
  }

  # Time span
  time_range <- range(object$time, na.rm = TRUE)
  n_time <- length(unique(object$time))
  cat("⏱️  Time span: ", round(time_range[1], 2), " to ", round(time_range[2], 2),
      " years (", n_time, " points)\n")

  cat("\n")

  # Epidemic summary for infected compartments
  infected_data <- object |> filter(compartment %in% c("I", "Ih"))

  if (nrow(infected_data) > 0) {
    cat("📈 Epidemic Summary:\n")

    # Calculate key epidemic metrics
    epidemic_stats <- infected_data |>
      group_by(compartment, replicate) |>
      summarise(
        peak_infected = max(value, na.rm = TRUE),
        peak_time = time[which.max(value)],
        final_infected = last(value),
        duration = sum(value > 1) * (max(time) - min(time)) / (n() - 1),
        .groups = "drop"
      ) |>
      group_by(compartment) |>
      summarise(
        avg_peak = mean(peak_infected, na.rm = TRUE),
        median_peak = median(peak_infected, na.rm = TRUE),
        avg_peak_time = mean(peak_time, na.rm = TRUE),
        avg_duration = mean(duration, na.rm = TRUE),
        extinction_rate = mean(final_infected < 1, na.rm = TRUE) * 100,
        .groups = "drop"
      )

    for (i in seq_len(nrow(epidemic_stats))) {
      comp <- epidemic_stats$compartment[i]
      comp_name <- if (comp == "I") "Rats" else "Humans"

      cat("  ", comp_name, ":\n")
      cat("    Peak infections: ", round(epidemic_stats$median_peak[i], 1),
          " (median), ", round(epidemic_stats$avg_peak[i], 1), " (mean)\n")
      cat("    Time to peak: ", round(epidemic_stats$avg_peak_time[i], 2), " years\n")
      cat("    Epidemic duration: ", round(epidemic_stats$avg_duration[i], 2), " years\n")

      if (epidemic_stats$extinction_rate[i] > 0) {
        cat("    Extinction rate: ", round(epidemic_stats$extinction_rate[i], 1), "%\n")
      }
    }
  }

  cat("\n")

  # Population impact summary
  initial_pops <- object |>
    filter(time == min(time)) |>
    group_by(compartment) |>
    summarise(initial = sum(value, na.rm = TRUE), .groups = "drop")

  final_pops <- object |>
    filter(time == max(time)) |>
    group_by(compartment) |>
    summarise(final = sum(value, na.rm = TRUE), .groups = "drop")

  pop_change <- merge(initial_pops, final_pops, by = "compartment") |>
    mutate(change = final - initial, pct_change = (final - initial) / initial * 100) |>
    filter(compartment %in% c("S", "R", "Sh", "Rh"))

  if (nrow(pop_change) > 0) {
    cat("🏘️  Population Changes:\n")
    for (i in seq_len(nrow(pop_change))) {
      comp <- pop_change$compartment[i]
      comp_name <- switch(comp,
        "S" = "Susceptible rats",
        "R" = "Recovered rats",
        "Sh" = "Susceptible humans",
        "Rh" = "Recovered humans",
        comp  # default case
      )

      change_icon <- if (pop_change$pct_change[i] > 0) "📈" else "📉"
      cat("  ", change_icon, " ", comp_name, ": ",
          sprintf("%+.1f%% (%+.0f)", pop_change$pct_change[i], pop_change$change[i]), "\n")
    }
  }

  cat("\n")

  # Basic reproduction number if parameters available
  params <- attr(object, "params")
  if (!is.null(params)) {
    R0 <- tryCatch(calculate_R0(params), error = function(e) NA)
    if (!is.na(R0)) {
      cat("📊 Basic Reproduction Number (R₀): ", round(R0, 3))
      if (R0 > 1) {
        cat(" ✅ (Epidemic potential)")
      } else {
        cat(" ⚠️  (Below epidemic threshold)")
      }
      cat("\n")
    }
  }

  # Spatial summary if multiple populations
  n_pops <- length(unique(object$population))
  if (n_pops > 1) {
    cat("\n🗺️  Spatial Distribution:\n")

    spatial_summary <- object |>
      filter(compartment == "I", time == max(time)) |>
      group_by(population) |>
      summarise(final_infected = mean(value, na.rm = TRUE), .groups = "drop") |>
      summarise(
        affected_pops = sum(final_infected > 1),
        max_infected = max(final_infected),
        total_infected = sum(final_infected),
        .groups = "drop"
      )

    cat("  Populations affected: ", spatial_summary$affected_pops, " of ", n_pops, "\n")
    cat("  Peak population infection: ", round(spatial_summary$max_infected, 1), "\n")
    cat("  Total infected: ", round(spatial_summary$total_infected, 1), "\n")
  }

  invisible(object)
}

#' Plot method for plague_results
#' @param x plague_results object
#' @param compartments Vector of compartments to plot (NULL for all)
#' @export
plot.plague_results <- function(x, compartments = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for plotting")
  }

  if (!is.null(compartments)) {
    x <- x |> dplyr::filter(compartment %in% compartments)
  }

  # Determine if spatial and if multiple replicates
  is_spatial <- length(unique(x$population)) > 1
  multiple_reps <- length(unique(x$replicate)) > 1

  if (is_spatial) {
    # Spatial plot - show by population with facets
    p <- x |>
      ggplot2::ggplot(ggplot2::aes(time, value, color = compartment, group = interaction(compartment, replicate))) +
      ggplot2::geom_line(alpha = 0.7) +
      ggplot2::facet_grid(compartment~population, labeller = ggplot2::label_both) +
      ggplot2::labs(
        title = paste("Plague Model Results:", attr(x, "model_type")),
        x = "Time (years)",
        y = "Population",
        color = "Compartment"
      ) +
      ggplot2::theme_minimal()
  } else  {
    # Multiple replicates - show individual replicate lines
    p <- x |>
      ggplot2::ggplot(ggplot2::aes(time, value, color = compartment, group = interaction(compartment, replicate))) +
      ggplot2::geom_line(alpha = 0.6) +
      ggplot2::labs(
        title = paste("Plague Model Results:", attr(x, "model_type")),
        x = "Time (years)",
        y = "Population",
        color = "Compartment"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::facet_wrap(~compartment, scales = "free_y")
  }

  return(p)
}

# Main Simulation Interface ---------------------------------------------------

#' Run plague model simulation
#' @param params Parameter set name, file path, or list of parameters
#' @param years Number of years to simulate (default 10)
#' @param timestep Time step resolution: "weekly" or "daily" (default "weekly")
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
                             years = 10,
                             timestep = c("weekly", "daily"),
                             include_humans = FALSE,
                             npop = 1,
                             contact_matrix = NULL,
                             n_particles = 100,
                             n_threads = 1,
                             seasonal = FALSE,
                             ...) {
  
  # Validate arguments
  timestep <- match.arg(timestep)
  stopifnot(is.numeric(years), years > 0)

  # Load and validate parameters
  if (inherits(params, "plague_parameters")) {
    model_params <- params
  } else {
    model_params <- load_parameters(params, ...)
  }

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

  # Prepare model-specific parameters - filter to avoid warnings
  sim_params <- as.list(model_params)

  # Define expected parameters for each model type
  if (include_humans) {
    # Human model parameters (no movement parameters)
    expected_params <- c("K_r", "r_r", "p", "d_r", "beta_r", "a", "m_r", "g_r", "r_f", "K_f", "d_f",
                        "K_h", "r_h", "d_h", "beta_h", "m_h", "g_h", "I_ini", "seasonal_amplitude")
  } else {
    # Spatial rat model parameters (includes movement parameters)
    expected_params <- c("K_r", "r_r", "p", "d_r", "beta_r", "a", "m_r", "g_r", "r_f", "K_f", "d_f",
                        "mu_r", "mu_f", "I_ini", "seasonal_amplitude")
  }

  # Filter to only expected parameters to avoid warnings
  sim_params <- sim_params[intersect(names(sim_params), expected_params)]

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
  # K_r in parameter files represents total system capacity
  if (npop > 1) {
    sim_params$K_r <- sim_params$K_r / npop
  }

  # Add temporal parameters
  dt <- switch(timestep,
    "weekly" = 1/52,
    "daily" = 1/365
  )
  sim_params$dt <- dt
  timesteps <- seq_len(as.integer(dt^-1 * years))

  time_years <- timesteps * dt
  if (seasonal) {
    sim_params$season <- sin(2 * pi * time_years)
  } else {
    sim_params$season <- rep(0, length(timesteps))
  }

  # Show simulation info
  start_time <- Sys.time() # use tic/toc instead?
  message("🚀 Running ", n_particles, " particles over ", length(timesteps), " time steps...")

  # Run the appropriate stochastic model
  if (include_humans) {
    # Single-population human model (npop = 1 enforced above)
    results <- run_human_stochastic_model(sim_params, timesteps, n_particles, n_threads)
    model_type <- "stochastic_humans"
  } else {
    # Always use spatial model (works for npop = 1 or npop > 1)
    results <- run_stochastic_simulation(sim_params, timesteps, n_particles, n_threads)
    model_type <- if (npop > 1) "stochastic_spatial" else "stochastic_single"
  }

  # Completion message
  elapsed <- round(as.numeric(Sys.time() - start_time, units = "secs"), 1)
  message("✅ Simulation completed in ", elapsed, "s")

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


#' Run human stochastic model
#' @param params List of parameters
#' @param timesteps Vector of timesteps
#' @param n_particles Number of particles
#' @param n_threads Number of threads
#' @return Tidy tibble with results
run_human_stochastic_model <- function(params, timesteps, n_particles, n_threads) {
  model <- plague_stochastic_humans$new(
    pars = params,
    time = 1L,
    n_particles = n_particles,
    n_threads = n_threads,
    seed = sample.int(.Machine$integer.max, 1)
  )

  # Run simulation
  state <- model$simulate(timesteps)

  state <- state |>
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

#' Plot plague simulation results
#' @param output Simulation output tibble
#' @param log_scale Logical, whether to use log scale
#' @param plot_type One of "all", "infected", or "phase"
#' @return ggplot object
# plot_plague_simulation <- function(output, log_scale = FALSE,
#                                    plot_type = "all") {
#   if (plot_type == "all") {
#     p <- output |>
#       tidyr::pivot_longer(-t) |>
#       ggplot2::ggplot(ggplot2::aes(t, value)) +
#       ggplot2::geom_line() +
#       ggplot2::facet_wrap(~name, scales = 'free_y') +
#       ggplot2::labs(
#         title = "Plague Model Simulation",
#         x = "Time (years)",
#         y = "Population"
#       )
#   } else if (plot_type == "infected") {
#     p <- output |>
#       ggplot2::ggplot(ggplot2::aes(t, I_r)) +
#       ggplot2::geom_line() +
#       ggplot2::labs(
#         title = "Infected Rat Population",
#         x = "Time (years)",
#         y = "Number of Infected Rats"
#       )
#   } else if (plot_type == "phase") {
#     p <- output |>
#       ggplot2::ggplot(ggplot2::aes(S_r, I_r)) +
#       ggplot2::geom_path() +
#       ggplot2::labs(
#         title = "Phase Portrait",
#         x = "Susceptible Rats",
#         y = "Infected Rats"
#       )
#   }
#
#   if (log_scale && plot_type != "phase") {
#     p <- p + ggplot2::scale_y_log10()
#   }
#
#   p + ggplot2::theme_minimal()
# }

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

    # Use current API instead of broken run_plague_simulation
    sim <- run_plague_model(
      params = params,
      npop = 1,  # Single population for sensitivity analysis
      n_particles = 5,  # Few particles for speed
      years = 10,
      seasonal = seasonal
    )
    sim |>
      dplyr::mutate(
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
#calculate_model_stats <- function(output) {
#  list(
#    peak_infected = max(output$I_r),
#    final_susceptible = tail(output$S_r, 1),
#    final_resistant = tail(output$R_r, 1),
#    average_flea_index = mean(output$N),
#    min_total_rats = min(output$S_r + output$I_r + output$R_r)
#  )
#}


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


#' Run stochastic simulation with given parameters
#' @param params List of parameters
#' @param timesteps Vector of timesteps
#' @param n_particles Number of particles (replicates)
#' @param n_threads Number of threads for parallel processing
#' @return Data frame with simulation results
run_stochastic_simulation <- function(params, timesteps, n_particles = 1, n_threads = 1) {
  stopifnot(is.list(params))
  stopifnot(is.numeric(timesteps))
  stopifnot(n_particles >= 1)
  stopifnot(n_threads >= 1)

  model <- plague_stochastic$new(
    pars = params,
    time = 1L,
    n_particles = n_particles,
    n_threads = n_threads,
    seed = sample.int(.Machine$integer.max, 1)
  )

  # Run simulation
  state <- model$simulate(timesteps)

  state <- state |>
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

#' Plot total infected over time with uncertainty
#' @param results Simulation results
#' @return ggplot object
plot_total_infected <- function(results) {
  results |>
    dplyr::filter(compartment == "I") |>
    dplyr::group_by(time, replicate) |>
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


#' Analyze outbreak characteristics
#' @param results Simulation results
#' @return Data frame with outbreak statistics
analyze_outbreaks <- function(results) {
  results |>
    dplyr::filter(compartment == "I") |>
    dplyr::group_by(time, replicate) |>
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

validate_inputs <- function(params, times, npop, n_particles) {
  assert_that(is.list(params) || is.character(params))
  assert_that(is.numeric(times))
  assert_that(length(times) >= 2)
  assert_that(is_scalar_integerish(npop))
  assert_that(npop >= 1)
  assert_that(is_scalar_integerish(n_particles))
  assert_that(n_particles >= 1)
}
