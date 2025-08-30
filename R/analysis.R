# R/analysis.R
# Analysis functions for plague model results

#' Calculate outbreak summary statistics
#' @param results plague_results object
#' @param compartment Compartment to analyze (default "I")
#' @param threshold Minimum value to consider as "outbreak" (default 1)
#' @return Data frame with outbreak statistics
#' @export
calculate_outbreak_metrics <- function(results, compartment = "I", threshold = 1) {
  if (!inherits(results, "plague_results")) {
    stop("Input must be a plague_results object")
  }
  
  # Filter to specified compartment
  outbreak_data <- results |>
    dplyr::filter(compartment == !!compartment)
  
  # Calculate metrics by replicate and population
  metrics <- outbreak_data |>
    dplyr::group_by(population, replicate) |>
    dplyr::summarise(
      peak_value = max(value),
      peak_time = time[which.max(value)],
      duration = sum(value >= threshold) * unique(diff(time))[1],
      total_cumulative = sum(value) * unique(diff(time))[1],
      final_size = dplyr::last(value),
      time_to_peak = peak_time - dplyr::first(time),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      outbreak_occurred = peak_value >= threshold
    )
  
  # Add model metadata
  attr(metrics, "compartment") <- compartment
  attr(metrics, "threshold") <- threshold
  attr(metrics, "model_type") <- attr(results, "model_type")
  
  class(metrics) <- c("plague_outbreak_metrics", class(metrics))
  return(metrics)
}

#' Summarize outbreak metrics across replicates
#' @param outbreak_metrics Output from calculate_outbreak_metrics
#' @return Summary statistics
#' @export
summarize_outbreak_metrics <- function(outbreak_metrics) {
  if (!inherits(outbreak_metrics, "plague_outbreak_metrics")) {
    stop("Input must be output from calculate_outbreak_metrics()")
  }
  
  # Overall summary
  summary_stats <- outbreak_metrics |>
    dplyr::group_by(population) |>
    dplyr::summarise(
      n_replicates = dplyr::n(),
      outbreak_probability = mean(outbreak_occurred),
      mean_peak = mean(peak_value),
      median_peak = median(peak_value),
      sd_peak = sd(peak_value),
      mean_duration = mean(duration),
      median_duration = median(duration),
      mean_time_to_peak = mean(time_to_peak),
      .groups = "drop"
    )
  
  # Add confidence intervals if multiple replicates
  if (nrow(outbreak_metrics) > 1) {
    summary_stats <- summary_stats |>
      dplyr::mutate(
        peak_ci_lower = purrr::map_dbl(population, ~ quantile(outbreak_metrics$peak_value[outbreak_metrics$population == .x], 0.025, na.rm = TRUE)),
        peak_ci_upper = purrr::map_dbl(population, ~ quantile(outbreak_metrics$peak_value[outbreak_metrics$population == .x], 0.975, na.rm = TRUE))
      )
  }
  
  return(summary_stats)
}

#' Calculate basic reproduction number (R0) for different model types
#' @param params plague_parameters object or list
#' @param model_type Type of model ("rats_only", "with_humans", etc.)
#' @param K_r Rat carrying capacity (defaults to 2500 if not in params)
#' @param K_h Human carrying capacity (defaults to 5000 if not in params, only used for "with_humans")
#' @return Numeric R0 value or list of R0 values
#' @export
calculate_R0 <- function(params, model_type = "rats_only", K_r = 2500, K_h = 5000) {
  if (inherits(params, "plague_parameters")) {
    p <- as.list(params)
  } else {
    p <- params
  }
  
  # Use K_r/K_h from params if available, otherwise use provided defaults
  K_r_val <- p$K_r %||% K_r
  K_h_val <- p$K_h %||% K_h
  
  switch(model_type,
    "rats_only" = {
      # R0 for rat-flea cycle
      # Transmission rate * flea searching success * rat density / (death + recovery)
      p$beta_r * K_r_val * (1 - exp(-p$a * K_r_val)) / (p$d_r + p$m_r)
    },
    "with_humans" = {
      # Calculate both rat and human R0
      R0_rats <- p$beta_r * K_r_val * (1 - exp(-p$a * K_r_val)) / (p$d_r + p$m_r)
      
      R0_humans <- p$beta_h * p$K_f / (p$d_h + p$m_h)
      
      list(rats = R0_rats, humans = R0_humans, combined = max(R0_rats, R0_humans))
    },
    stop("Unknown model_type: ", model_type)
  )
}

#' Calculate equilibrium values for deterministic models
#' 
#' @note This function is for reference/educational purposes.
#'   For operational models, use the stochastic simulations in run_plague_model().
#' @param params plague_parameters object or list
#' @param model_type Type of model
#' @return List of equilibrium values
#' @export
calculate_equilibrium <- function(params, model_type = "rats_only") {
  if (inherits(params, "plague_parameters")) {
    p <- as.list(params)
  } else {
    p <- params
  }
  
  R0 <- calculate_R0(params, model_type)
  
  if (is.list(R0)) {
    R0_rats <- R0$rats
  } else {
    R0_rats <- R0
  }
  
  if (R0_rats <= 1) {
    # Disease-free equilibrium
    return(list(
      S_r = p$K_r,
      I_r = 0,
      R_r = 0,
      N = p$K_f,
      F = 0,
      R0 = R0_rats,
      equilibrium_type = "disease_free"
    ))
  } else {
    # Endemic equilibrium (approximate)
    # This is a simplified calculation - full equilibrium would require solving the system
    with(p, {
      # Approximate endemic values
      I_r_endemic <- K_r * (R0_rats - 1) / R0_rats * 0.1  # rough approximation
      S_r_endemic <- K_r / R0_rats
      R_r_endemic <- K_r - S_r_endemic - I_r_endemic
      
      list(
        S_r = S_r_endemic,
        I_r = I_r_endemic, 
        R_r = max(0, R_r_endemic),
        N = K_f,  # approximate
        F = I_r_endemic * K_f * 0.1,  # rough approximation
        R0 = R0_rats,
        equilibrium_type = "endemic"
      )
    })
  }
}

#' Calculate force of infection over time
#' @param results plague_results object
#' @return Tibble with force of infection by time
#' @export
calculate_force_of_infection <- function(results) {
  if (!inherits(results, "plague_results")) {
    stop("Input must be a plague_results object")
  }
  
  # Extract relevant compartments
  force_data <- results |>
    dplyr::filter(compartment %in% c("F", "S_r", "I_r", "R_r")) |>
    tidyr::pivot_wider(names_from = compartment, values_from = value, values_fill = 0) |>
    dplyr::mutate(
      T_r = S_r + I_r + R_r,
      # Force of infection in humans (lambda_h)
      lambda_h = F * exp(-4e-3 * T_r),  # Using default 'a' parameter
      # Force of infection in rats  
      lambda_r = ifelse(T_r > 0, F * (1 - exp(-4e-3 * T_r)) / T_r, 0)
    ) |>
    dplyr::select(time, population, replicate, lambda_h, lambda_r)
  
  return(force_data)
}

#' Calculate spatial correlation in infection levels
#' @param results plague_results object (must be spatial)
#' @param compartment Compartment to analyze (default "I")
#' @param method Correlation method ("pearson", "spearman")
#' @return Correlation matrix
#' @export
calculate_spatial_correlation <- function(results, compartment = "I", method = "pearson") {
  if (!inherits(results, "plague_results")) {
    stop("Input must be a plague_results object")
  }
  
  # Check if spatial
  if (length(unique(results$population)) <= 1) {
    stop("Results must be from a spatial model")
  }
  
  # Prepare data for correlation analysis
  spatial_data <- results |>
    dplyr::filter(compartment == !!compartment) |>
    dplyr::select(time, population, replicate, value) |>
    # Average across replicates if multiple
    dplyr::group_by(time, population) |>
    dplyr::summarise(mean_value = mean(value), .groups = "drop") |>
    tidyr::pivot_wider(names_from = population, values_from = mean_value, names_prefix = "pop_")
  
  # Calculate correlation matrix
  cor_matrix <- spatial_data |>
    dplyr::select(-time) |>
    cor(method = method, use = "complete.obs")
  
  return(cor_matrix)
}

#' Print method for outbreak metrics
#' @param x plague_outbreak_metrics object
#' @param ... Additional arguments (ignored)
#' @export
print.plague_outbreak_metrics <- function(x, ...) {
  cat("Plague Outbreak Metrics\n")
  cat("=======================\n")
  cat("Compartment:", attr(x, "compartment"), "\n")
  cat("Threshold:", attr(x, "threshold"), "\n")
  cat("Model type:", attr(x, "model_type"), "\n")
  cat("Populations:", length(unique(x$population)), "\n")
  cat("Replicates:", length(unique(x$replicate)), "\n")
  cat("\n")
  
  # Summary statistics
  cat("Overall outbreak probability:", mean(x$outbreak_occurred), "\n")
  cat("Mean peak value:", round(mean(x$peak_value), 2), "\n")
  cat("Mean duration:", round(mean(x$duration), 2), "\n")
  cat("\n")
  
  # Print data preview
  NextMethod("print")
  
  invisible(x)
}