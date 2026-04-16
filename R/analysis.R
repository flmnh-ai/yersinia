# R/analysis.R
# Analysis functions for plague model results

#' Calculate outbreak summary statistics
#' @param results plague_results object
#' @param compartment Compartment to analyze (default "I")
#' @param threshold Minimum value to consider as "outbreak" (default 1)
#' @return Data frame with outbreak statistics
#' @export
calculate_outbreak_metrics <- function(results, compartment = "I", threshold = 1) {
  check_plague_results(results, "outbreak analysis")
  
  # Filter to specified compartment
  outbreak_data <- results |>
    dplyr::filter(.data$compartment == .env$compartment)
  
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
  checkmate::assert_class(outbreak_metrics, "plague_outbreak_metrics")
  
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
      # R0 for carcass-based rat-plague cycle (next-generation for I-Q system).
      # At disease-free equilibrium S = T_r = K_r, so the per-carcass new-infection
      # rate simplifies: lambda_r * S = beta_r * Q * (1 - exp(-rho*T_r/K_r)) = beta_r * Q * (1 - exp(-rho)).
      #
      # Per newly infected rat, expected carcasses produced = (1 - g_r) * m_r / m_r = (1 - g_r).
      # Per carcass, expected new infections = beta_r * (1 - exp(-rho)) / delta_R.
      # Cycle R0 = (1 - g_r) * beta_r * (1 - exp(-rho)) / delta_R.
      #
      # Note: m_r cancels out (appears in both carcass-production rate and I lifetime).
      # Earlier versions of this code erroneously kept m_r in the denominator, which
      # inflated R0 by ~1/m_r (factor of ~18 for Didelot's m_r = 0.056/day).
      p$beta_r * (1 - p$g_r) * (1 - exp(-p$rho)) / p$delta_R
    },
    "with_humans" = {
      # Rat R0 via carcass transmission (see "rats_only" branch for derivation)
      R0_rats <- p$beta_r * (1 - p$g_r) * (1 - exp(-p$rho)) / p$delta_R

      # Human R0 from human-to-human transmission only
      beta_I <- p$beta_I %||% 0
      R0_humans <- beta_I / p$m_h

      list(rats = R0_rats, humans = R0_humans, combined = max(R0_rats, R0_humans))
    },
    cli::cli_abort("Unknown model_type: {model_type}")
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
  
  K_r_val <- p$K_r %||% 2500

  if (R0_rats <= 1) {
    # Disease-free equilibrium
    return(list(
      S_r = K_r_val,
      I_r = 0,
      R_r = 0,
      Q = 0,
      R0 = R0_rats,
      equilibrium_type = "disease_free"
    ))
  } else {
    # Endemic equilibrium (approximate)
    # Simplified - full equilibrium requires solving the nonlinear system
    S_r_endemic <- K_r_val / R0_rats
    I_r_endemic <- K_r_val * (R0_rats - 1) / R0_rats * 0.1
    R_r_endemic <- K_r_val - S_r_endemic - I_r_endemic
    Q_endemic <- (1 - p$g_r) * p$m_r * I_r_endemic / p$delta_R

    list(
      S_r = S_r_endemic,
      I_r = I_r_endemic,
      R_r = max(0, R_r_endemic),
      Q = Q_endemic,
      R0 = R0_rats,
      equilibrium_type = "endemic"
    )
  }
}

#' Calculate force of infection over time
#' @param results plague_results object
#' @return Tibble with force of infection by time
#' @export
calculate_force_of_infection <- function(results) {
  check_plague_results(results, "outbreak analysis")
  
  # Extract relevant compartments for carcass-based force of infection
  # Get params for rho and K_r
  p <- attr(results, "params")
  rho <- p$rho %||% 2.63
  K_r_val <- p$K_r %||% 2500

  force_data <- results |>
    dplyr::filter(.data$compartment %in% c("Q", "S", "I", "R")) |>
    tidyr::pivot_wider(names_from = compartment, values_from = value, values_fill = 0) |>
    dplyr::mutate(
      T_r = S + I + R,
      # Force of infection in rats from carcasses
      lambda_r = ifelse(T_r > 0, (p$beta_r %||% 0.77) * Q * (1 - exp(-rho * T_r / K_r_val)) / T_r, 0),
      # Force of infection in humans from carcasses (fleas that don't find rats)
      lambda_h = (p$beta_h %||% 0.0145) * Q * exp(-rho * T_r / K_r_val) / K_r_val
    ) |>
    dplyr::select(time, population, replicate, lambda_h, lambda_r)
  
  return(force_data)
}

# calculate_spatial_correlation moved to archive/spatial_helpers.R in April
# 2026 alongside the retirement of the spatial plague model.

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