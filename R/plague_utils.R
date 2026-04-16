# R/plague_utils.R

`%||%` <- function(x, y) if (is.null(x)) y else x

# Helper functions for validation (not exported)
check_plague_results <- function(results, context = "analysis") {
  checkmate::assert_class(results, "plague_results",
                         .var.name = glue::glue("results (for {context})"))
}

check_ggplot2 <- function(context = "plotting") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort("ggplot2 package required for {context}")
  }
}

# check_spatial_model() and check_gganimate() moved to
# archive/spatial_helpers.R in April 2026 alongside the retirement of the
# spatial plague model.


#' Load plague model scenario parameters
#' @param scenario Name of scenario or path to YAML file or list of parameters
#' @param ... Additional model parameters to override
#' @return List of model parameters
#' @export
load_scenario <- function(scenario = "defaults", ...) {
  # Handle different input types
  if (is.list(scenario)) {
    params <- scenario
  } else if (is.character(scenario)) {
    if (file.exists(scenario)) {
      # It's a file path
      params <- yaml::read_yaml(scenario)
    } else {
      # It's a named scenario
      params <- load_named_scenario(scenario)
    }
  } else {
    cli::cli_abort("scenario must be a character string, file path, or list")
  }

  # Override with any additional parameters
  override_params <- list(...)
  if (length(override_params) > 0) {
    params[names(override_params)] <- override_params
  }

  # Lightweight probability bounds check
  if (!is.null(params$p) && (params$p < 0 || params$p > 1))
    cli::cli_abort("p (resistance probability) must be between 0 and 1")
  if (!is.null(params$g_r) && (params$g_r < 0 || params$g_r > 1))
    cli::cli_abort("g_r (rat survival probability) must be between 0 and 1")
  if (!is.null(params$g_h) && (params$g_h < 0 || params$g_h > 1))
    cli::cli_abort("g_h (human survival probability) must be between 0 and 1")

  # Add metadata
  attr(params, "scenario") <- if(is.character(scenario)) scenario else "custom"
  attr(params, "modified") <- length(override_params) > 0

  class(params) <- c("scenario_parameters", "list")
  return(params)
}

#' Load named scenario parameters
#' @param name Name of scenario
#' @return List of model parameters
load_named_scenario <- function(name) {
  # Available scenarios
  available_scenarios <- c("defaults", "keeling-gilligan", "modern-estimates", "historical", "didelot")

  checkmate::assert_choice(name, available_scenarios,
                          .var.name = glue::glue("scenario name '{name}'"))

  # Try to load from YAML file first
  # Use system.file for installed packages, fallback to inst/ for development
  yaml_file <- system.file("scenarios", paste0(name, ".yaml"), package = "yersinia")

  if (yaml_file == "" || !file.exists(yaml_file)) {
    # Fallback for development mode
    yaml_file <- file.path("inst", "scenarios", paste0(name, ".yaml"))
  }

  checkmate::assert_file_exists(yaml_file,
    .var.name = glue::glue("YAML file for scenario '{name}' (package installation issue)"))

  if (!requireNamespace("yaml", quietly = TRUE)) {
    cli::cli_abort("yaml package required for loading parameter files")
  }

  params_full <- yaml::read_yaml(yaml_file)

  # Extract just the parameter values (exclude metadata)
  metadata_keys <- c("name", "description", "source", "reference", "doi",
                    "last_updated", "period", "notes", "time_unit")
  params <- params_full[!names(params_full) %in% metadata_keys]

  # Store metadata as attributes
  attr(params, "metadata") <- params_full[intersect(names(params_full), metadata_keys)]

  return(params)
}

#' Print method for plague_parameters
#' @param x plague_parameters object
#' @param ... Additional arguments (ignored)
#' @export
print.scenario_parameters <- function(x, ...) {
  scenario <- attr(x, "scenario") %||% "custom"
  cat("🦠 Plague Scenario (", scenario, ")\n", sep = "")

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
    r_r = "Rat population growth rate (per day)",
    d_r = "Natural death rate of rats (per day)",
    p = "Probability of inherited resistance"
  )

  for (param in rat_params) {
    if (param %in% names(x)) {
      desc <- rat_descriptions[[param]] %||% ""
      cat(sprintf("  %-6s = %8.3f  # %s\n", param, x[[param]], desc))
    }
  }

  cat("\n💀 Carcass/Transmission Parameters:\n")
  carcass_params <- c("rho", "delta_R")
  carcass_descriptions <- list(
    rho = "Rat carcass infectivity range",
    delta_R = "Carcass decay rate"
  )

  for (param in carcass_params) {
    if (param %in% names(x)) {
      desc <- carcass_descriptions[[param]] %||% ""
      cat(sprintf("  %-8s = %8.3f  # %s\n", param, x[[param]], desc))
    }
  }

  cat("\n🔬 Disease Parameters:\n")
  disease_params <- c("beta_r", "m_r", "g_r")
  disease_descriptions <- list(
    beta_r = "Transmission rate from carcasses to rats (per day)",
    m_r = "Plague resolution rate in rats (per day)",
    g_r = "Probability rat survives infection"
  )

  for (param in disease_params) {
    if (param %in% names(x)) {
      desc <- disease_descriptions[[param]] %||% ""
      cat(sprintf("  %-6s = %8.3f  # %s\n", param, x[[param]], desc))
    }
  }

  # Human parameters if present
  human_params <- c("K_h", "r_h", "d_h", "beta_h", "beta_I", "m_h", "g_h")
  has_human_params <- any(human_params %in% names(x))

  if (has_human_params) {
    cat("\n👤 Human Parameters:\n")
    human_descriptions <- list(
      K_h = "Human carrying capacity",
      r_h = "Human population growth rate (per day)",
      d_h = "Natural death rate of humans (per day)",
      beta_h = "Transmission rate from carcasses to humans (per day)",
      beta_I = "Human-to-human transmission rate (per day)",
      m_h = "Plague resolution rate in humans (per day)",
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
  other_params <- setdiff(names(x), c(rat_params, carcass_params, disease_params, human_params))
  if (length(other_params) > 0) {
    cat("\n⚙️  Other Parameters:\n")
    other_descriptions <- list(
      I_ini = "Initial number of infected rats",
      mu_r = "Rat movement rate (per day)"
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
  checkmate::assert_names(names(data), must.include = required_cols,
                         .var.name = "simulation results data")

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
  infected_data <- object |> dplyr::filter(.data$compartment %in% c("I", "Ih"))

  if (nrow(infected_data) > 0) {
    cat("📈 Epidemic Summary:\n")

    # Calculate key epidemic metrics
    epidemic_stats <- infected_data |>
      dplyr::group_by(.data$compartment, .data$replicate) |>
      dplyr::summarise(
        peak_infected = max(value, na.rm = TRUE),
        peak_time = time[which.max(value)],
        final_infected = dplyr::last(value),
        duration = sum(value > 1) * (max(time) - min(time)) / (dplyr::n() - 1),
        .groups = "drop"
      ) |>
      dplyr::group_by(.data$compartment) |>
      dplyr::summarise(
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
    dplyr::filter(time == min(time)) |>
    dplyr::group_by(.data$compartment) |>
    dplyr::summarise(initial = sum(value, na.rm = TRUE), .groups = "drop")

  final_pops <- object |>
    dplyr::filter(time == max(time)) |>
    dplyr::group_by(.data$compartment) |>
    dplyr::summarise(final = sum(value, na.rm = TRUE), .groups = "drop")

  pop_change <- merge(initial_pops, final_pops, by = "compartment") |>
    dplyr::mutate(change = final - initial, pct_change = (final - initial) / initial * 100) |>
    dplyr::filter(.data$compartment %in% c("S", "R", "Sh", "Rh"))

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
      dplyr::filter(.data$compartment == "I", time == max(time)) |>
      dplyr::group_by(.data$population) |>
      dplyr::summarise(final_infected = mean(value, na.rm = TRUE), .groups = "drop") |>
      dplyr::summarise(
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
  check_ggplot2("results plotting")

  if (!is.null(compartments)) {
    x <- x |> dplyr::filter(.data$compartment %in% compartments)
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
#'
#' Runs the stochastic rats + humans plague model
#' (`inst/odin/plague_stochastic_humans.R`) via dust2. The spatial rats-only
#' model was retired in April 2026 along with the dust v1 backend (see
#' `archive/plague_stochastic.R`). Re-adding spatial support means writing
#' a spatial+humans odin2 model first; this function will then grow a
#' `npop` argument again.
#'
#' @param scenario Parameter set name, file path, or list of parameters.
#' @param years Number of years to simulate (default 10).
#' @param timestep Time step resolution: "weekly" or "daily" (default "weekly").
#' @param n_particles Number of particles for the stochastic run.
#' @param n_threads Number of threads for parallel processing.
#' @param K_r Rat carrying capacity (used only when scenario does not set it).
#' @param K_h Human carrying capacity (used only when scenario does not set it).
#' @param I_ini Initial infected rats.
#' @param ... Additional parameters to override on top of the scenario.
#' @return plague_results object.
#' @export
run_plague_model <- function(scenario = "defaults",
                             years = 10,
                             timestep = c("weekly", "daily"),
                             n_particles = 100,
                             n_threads = 1,
                             K_r = 2500,
                             K_h = 5000,
                             I_ini = 1,
                             ...) {

  # Validate arguments
  timestep <- match.arg(timestep)
  stopifnot(is.numeric(years), years > 0)

  # Load and validate scenario parameters
  if (inherits(scenario, "scenario_parameters")) {
    model_params <- scenario
  } else {
    model_params <- load_scenario(scenario, ...)
  }

  # Prepare odin parameters from scenario
  sim_params <- as.list(model_params)

  # Use carrying capacities from function arguments only if not already set by scenario
  if (is.null(sim_params$K_r)) sim_params$K_r <- K_r
  if (is.null(sim_params$K_h)) sim_params$K_h <- K_h

  # Humans model is single-population; I_ini is scalar.
  sim_params$I_ini <- I_ini[[1]]
  sim_params$S_ini <- 1

  # Add temporal parameters — dt in days, all rates per day
  dt <- switch(timestep,
    "weekly" = 7,
    "daily" = 1
  )
  sim_params$dt <- dt
  n_days <- as.integer(365 * years)
  timesteps <- seq_len(as.integer(n_days / dt))

  # Show simulation info
  start_time <- Sys.time()
  cli::cli_progress_step("🚀 Running {n_particles} particles over {length(timesteps)} time steps")

  results <- run_human_stochastic_model(sim_params, timesteps, n_particles, n_threads)
  model_type <- "stochastic_humans"

  # Completion message
  elapsed <- round(as.numeric(Sys.time() - start_time, units = "secs"), 1)
  cli::cli_progress_done()
  cli::cli_inform("✅ Simulation completed in {elapsed}s")

  # Create run info
  run_info <- list(
    timestamp = Sys.time(),
    model = "stochastic_humans",
    n_particles = n_particles
  )

  # Return plague_results object
  new_plague_results(results, model_type, model_params, run_info)
}

# Model-specific runner functions ---------------------------------------------

#' Stochastic humans plague model (carcass formulation)
#'
#' An odin2/dust2 system generator compiled at package build time from
#' `inst/odin/plague_stochastic_humans.R`. Pass it directly to
#' [dust2::dust_system_create()] or [dust2::dust_filter_create()].
#'
#' @name plague_stochastic_humans
#' @export
NULL

#' Run human stochastic model (dust2/odin2 backend)
#' @param params List of parameters
#' @param timesteps Vector of timesteps
#' @param n_particles Number of particles
#' @param n_threads Number of threads
#' @return Tidy tibble with results
run_human_stochastic_model <- function(params, timesteps, n_particles, n_threads) {
  # Translate legacy `dt` -> odin2's `tau` (odin2 reserves `dt`).
  dt_days <- params$dt %||% params$tau %||% 1
  params$tau <- dt_days
  params$dt <- NULL

  # Restrict to parameters the humans odin2 model accepts; drop spatial-only
  # keys like npop, contact, mu_r so dust2 doesn't reject the call.
  model_param_names <- c("tau", "I_ini", "S_ini", "K_r", "K_h", "r_r", "r_h",
                         "p", "d_r", "d_h", "beta_r", "beta_h", "beta_I", "rho",
                         "m_r", "m_h", "g_r", "g_h", "delta_R", "kappa")
  model_params <- params[intersect(names(params), model_param_names)]
  # I_ini may arrive as length-1 vector from run_plague_model's spatial prep;
  # odin2 wants scalar in the single-population case.
  if (length(model_params$I_ini) > 1) {
    model_params$I_ini <- model_params$I_ini[[1]]
  }

  sys <- dust2::dust_system_create(
    plague_stochastic_humans,
    pars = model_params,
    n_particles = n_particles,
    n_threads = n_threads,
    seed = sample.int(.Machine$integer.max, 1)
  )
  dust2::dust_system_set_state_initial(sys)

  y <- dust2::dust_system_simulate(sys, times = timesteps)
  state_list <- dust2::dust_unpack_state(sys, y)

  # Rename odin2 compartment names to the legacy convention used by downstream
  # plot/summary methods (S_h -> Sh, etc.)
  name_map <- c(S = "S", I = "I", R = "R", Q = "Q",
                S_h = "Sh", I_h = "Ih", R_h = "Rh", D_h = "Dh")
  time_years <- timesteps * dt_days / 365

  state <- do.call(rbind, lapply(names(name_map), function(src) {
    mat <- state_list[[src]]  # dims: n_particles x n_times (scalar compartment)
    tibble::tibble(
      compartment = name_map[[src]],
      replicate   = rep(seq_len(n_particles), times = length(timesteps)),
      time        = rep(time_years, each = n_particles),
      value       = as.vector(mat)
    )
  }))
  state$population <- 1L

  state
}



