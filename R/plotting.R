# R/plotting.R
# Enhanced plotting functions for plague model results

#' Plot phase portrait (S vs I)
#' @param results plague_results object
#' @param compartments Vector of two compartments to plot (default c("S_r", "I_r"))
#' @param population Which population to plot (for spatial models, default 1)
#' @param replicate Which replicate to plot (for stochastic models, default 1)
#' @param ... Additional ggplot2 arguments
#' @return ggplot2 object
#' @export
plot_phase_portrait <- function(results, compartments = c("S_r", "I_r"), 
                               population = 1, replicate = 1, ...) {
  if (!inherits(results, "plague_results")) {
    stop("Input must be a plague_results object")
  }
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for plotting")
  }
  
  # Filter and prepare data
  phase_data <- results |>
    filter(
      compartment %in% compartments,
      population == !!population,
      replicate == !!replicate
    ) |>
    select(time, compartment, value) |>
    pivot_wider(names_from = compartment, values_from = value)
  
  if (ncol(phase_data) != 3) {
    stop("Need exactly two compartments for phase portrait")
  }
  
  # Create plot
  x_var <- names(phase_data)[2]
  y_var <- names(phase_data)[3]
  
  p <- phase_data |>
    ggplot(aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_path(size = 0.8, alpha = 0.8) +
    geom_point(data = phase_data[1, ], size = 3, color = "green", alpha = 0.8) +  # Start
    geom_point(data = phase_data[nrow(phase_data), ], size = 3, color = "red", alpha = 0.8) +  # End
    labs(
      title = paste("Phase Portrait:", attr(results, "model_type")),
      subtitle = paste("Population", population, "- Replicate", replicate),
      x = x_var,
      y = y_var
    ) +
    theme_minimal()
  
  return(p)
}

#' Plot spatial heatmap for a single time point
#' @param results plague_results object (must be spatial)
#' @param time_point Time point to plot
#' @param compartment Compartment to plot (default "I")
#' @param replicate Which replicate to plot (default 1)
#' @param n_rows Number of rows in spatial grid (default 5)
#' @param n_cols Number of columns in spatial grid (default 5)
#' @return ggplot2 object
#' @export
plot_spatial_heatmap <- function(results, time_point, compartment = "I", 
                                replicate = 1, n_rows = 5, n_cols = 5) {
  if (!inherits(results, "plague_results")) {
    stop("Input must be a plague_results object")
  }
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for plotting")
  }
  
  # Check if spatial
  if (length(unique(results$population)) <= 1) {
    stop("Results must be from a spatial model")
  }
  
  # Find closest time point
  available_times <- unique(results$time)
  closest_time <- available_times[which.min(abs(available_times - time_point))]
  
  # Prepare spatial data
  spatial_data <- results |>
    filter(
      time == closest_time,
      compartment == !!compartment,
      replicate == !!replicate
    ) |>
    mutate(
      row = ceiling(population / n_cols),
      col = ((population - 1) %% n_cols) + 1
    )
  
  # Create heatmap
  p <- spatial_data |>
    ggplot(aes(col, row, fill = value)) +
    geom_tile() +
    scale_fill_viridis_c(option = "plasma", name = compartment) +
    scale_y_reverse() +  # Flip y-axis to match grid layout
    coord_fixed() +
    labs(
      title = paste("Spatial Distribution:", compartment),
      subtitle = paste("Time =", round(closest_time, 2), "- Replicate", replicate),
      x = "Column",
      y = "Row"
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.ticks = element_blank()
    )
  
  return(p)
}

#' Create animated spatial plot over time
#' @param results plague_results object (must be spatial)  
#' @param compartment Compartment to animate (default "I")
#' @param replicate Which replicate to plot (default 1)
#' @param n_rows Number of rows in spatial grid (default 5)
#' @param n_cols Number of columns in spatial grid (default 5)
#' @param time_points Vector of time points to include (NULL for all)
#' @return gganimate object
#' @export
animate_spatial_spread <- function(results, compartment = "I", replicate = 1,
                                  n_rows = 5, n_cols = 5, time_points = NULL) {
  if (!inherits(results, "plague_results")) {
    stop("Input must be a plague_results object")
  }
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for plotting")
  }
  
  if (!requireNamespace("gganimate", quietly = TRUE)) {
    stop("gganimate required for animation")
  }
  
  # Check if spatial
  if (length(unique(results$population)) <= 1) {
    stop("Results must be from a spatial model")
  }
  
  # Select time points
  if (is.null(time_points)) {
    # Use every 5th time point to keep animation manageable
    all_times <- sort(unique(results$time))
    time_points <- all_times[seq(1, length(all_times), by = 5)]
  }
  
  # Prepare animation data
  anim_data <- results |>
    filter(
      time %in% time_points,
      compartment == !!compartment,
      replicate == !!replicate
    ) |>
    mutate(
      row = ceiling(population / n_cols),
      col = ((population - 1) %% n_cols) + 1
    )
  
  # Create animated plot
  p <- anim_data |>
    ggplot(aes(col, row, fill = value)) +
    geom_tile() +
    scale_fill_viridis_c(option = "plasma", name = compartment) +
    scale_y_reverse() +
    coord_fixed() +
    labs(
      title = paste("Spatial Spread of", compartment),
      subtitle = "Time: {closest_state}",
      x = "Column", 
      y = "Row"
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.ticks = element_blank()
    ) +
    gganimate::transition_states(time) +
    gganimate::ease_aes('linear')
  
  return(p)
}

#' Plot compartment dynamics with uncertainty bands
#' @param results plague_results object
#' @param compartments Vector of compartments to plot (NULL for all)
#' @param population Which population to plot (for spatial models, default 1)
#' @param show_uncertainty Show uncertainty bands for stochastic models (default TRUE)
#' @param log_scale Use log scale for y-axis (default FALSE)
#' @return ggplot2 object
#' @export
plot_dynamics <- function(results, compartments = NULL, population = 1, 
                         show_uncertainty = TRUE, log_scale = FALSE) {
  if (!inherits(results, "plague_results")) {
    stop("Input must be a plague_results object")
  }
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for plotting")
  }
  
  # Filter data
  plot_data <- results |>
    filter(population == !!population)
  
  if (!is.null(compartments)) {
    plot_data <- plot_data |> filter(compartment %in% compartments)
  }
  
  # Check if multiple replicates
  multiple_reps <- length(unique(plot_data$replicate)) > 1
  
  if (multiple_reps && show_uncertainty) {
    # Plot with uncertainty bands
    summary_data <- plot_data |>
      group_by(time, compartment) |>
      summarise(
        median = median(value),
        lower = quantile(value, 0.025),
        upper = quantile(value, 0.975),
        .groups = "drop"
      )
    
    p <- summary_data |>
      ggplot(aes(time, median, color = compartment, fill = compartment)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
      geom_line(size = 1) +
      labs(
        title = paste("Population Dynamics:", attr(results, "model_type")),
        subtitle = paste("Population", population, "with 95% uncertainty bands"),
        x = "Time (years)",
        y = "Population",
        color = "Compartment",
        fill = "Compartment"
      )
  } else {
    # Simple line plot
    if (multiple_reps) {
      # Show individual replicates
      p <- plot_data |>
        ggplot(aes(time, value, color = compartment, group = interaction(compartment, replicate))) +
        geom_line(alpha = 0.6) +
        labs(
          title = paste("Population Dynamics:", attr(results, "model_type")),
          subtitle = paste("Population", population, "- All replicates"),
          x = "Time (years)",
          y = "Population",
          color = "Compartment"
        )
    } else {
      # Single replicate
      p <- plot_data |>
        ggplot(aes(time, value, color = compartment)) +
        geom_line(size = 1) +
        labs(
          title = paste("Population Dynamics:", attr(results, "model_type")),
          subtitle = paste("Population", population),
          x = "Time (years)",
          y = "Population", 
          color = "Compartment"
        )
    }
  }
  
  p <- p + theme_minimal()
  
  if (log_scale) {
    p <- p + scale_y_log10()
  }
  
  return(p)
}

#' Plot parameter sensitivity results
#' @param sensitivity_results Output from run_sensitivity_analysis
#' @param compartment Compartment to plot (default "I_r")
#' @param metric Summary metric to plot ("peak", "final", "auc")
#' @return ggplot2 object  
#' @export
plot_sensitivity <- function(sensitivity_results, compartment = "I_r", metric = "peak") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for plotting")
  }
  
  # Calculate summary metric
  metric_data <- sensitivity_results |>
    filter(compartment == !!compartment) |>
    group_by(parameter, multiplier) |>
    summarise(
      peak = max(value),
      final = last(value),
      auc = sum(value) * unique(diff(time))[1],
      .groups = "drop"
    )
  
  # Select metric to plot
  y_var <- switch(metric,
    "peak" = "peak",
    "final" = "final", 
    "auc" = "auc",
    stop("metric must be 'peak', 'final', or 'auc'")
  )
  
  p <- metric_data |>
    ggplot(aes(multiplier, .data[[y_var]])) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(
      title = paste("Sensitivity Analysis:", compartment),
      subtitle = paste("Parameter:", unique(metric_data$parameter)),
      x = "Parameter Multiplier",
      y = paste(str_to_title(metric), compartment),
      caption = "Relative to baseline parameter value"
    ) +
    theme_minimal()
  
  return(p)
}

#' Create multi-panel comparison plot
#' @param results_list Named list of plague_results objects
#' @param compartment Compartment to compare (default "I")
#' @param population Which population to plot (default 1)
#' @return ggplot2 object
#' @export
plot_comparison <- function(results_list, compartment = "I", population = 1) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for plotting")
  }
  
  # Combine results
  combined_data <- map_dfr(names(results_list), function(name) {
    results_list[[name]] |>
      filter(compartment == !!compartment, population == !!population) |>
      mutate(scenario = name)
  })
  
  # Plot comparison
  p <- combined_data |>
    ggplot(aes(time, value, color = scenario)) +
    geom_line(size = 1) +
    labs(
      title = paste("Model Comparison:", compartment),
      subtitle = paste("Population", population),
      x = "Time (years)",
      y = "Population",
      color = "Scenario"
    ) +
    theme_minimal()
  
  return(p)
}