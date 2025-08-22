# Comprehensive test script for plague modeling API
# Run this after sourcing all the new files

library(tidyverse)
library(odin)
library(odin.dust)
library(yaml)

# Source all the updated files
source("R/plague_utils.R")
source("R/analysis.R")
source("R/models_deterministic.R")

cat("=== COMPREHENSIVE API TESTING ===\n\n")

# Test 1: Parameter loading from YAML files
cat("=== Testing YAML Parameter Loading ===\n")
for (param_set in c("defaults", "keeling-gilligan", "modern-estimates", "historical")) {
  cat("Loading", param_set, "...\n")
  tryCatch({
    params <- load_parameters(param_set)
    cat("  Success! R0 =", round(calculate_R0(params), 2), "\n")
  }, error = function(e) {
    cat("  Error:", e$message, "\n")
  })
}

# Test 2: Deterministic models
cat("\n=== Testing Deterministic Models ===\n")
tryCatch({
  # Basic deterministic model
  results_det <- run_plague_model(
    model = "deterministic",
    params = "keeling-gilligan",
    times = seq(0, 5, by = 0.1)
  )
  
  cat("Success! Deterministic model completed\n")
  cat("Model type:", attr(results_det, "model_type"), "\n")
  cat("Compartments:", paste(unique(results_det$compartment), collapse = ", "), "\n")
  
  # Test with humans
  results_det_human <- run_plague_model(
    model = "deterministic",
    params = "modern-estimates",
    include_humans = TRUE,
    times = seq(0, 3, by = 0.1)
  )
  
  cat("Success! Deterministic human model completed\n")
  
}, error = function(e) {
  cat("Error in deterministic models:", e$message, "\n")
})

# Test 3: Analysis functions
cat("\n=== Testing Analysis Functions ===\n")
if (exists("results_det")) {
  tryCatch({
    # Outbreak metrics
    metrics <- calculate_outbreak_metrics(results_det, compartment = "I_r")
    cat("Outbreak metrics calculated successfully\n")
    print(metrics)
    
    # R0 calculation
    params <- load_parameters("historical")
    R0 <- calculate_R0(params)
    cat("R0 for historical parameters:", round(R0, 2), "\n")
    
    # Equilibrium calculation
    eq <- calculate_equilibrium(params)
    cat("Equilibrium type:", eq$equilibrium_type, "\n")
    
  }, error = function(e) {
    cat("Error in analysis functions:", e$message, "\n")
  })
}

# Test 4: Stochastic models (if they work)
cat("\n=== Testing Stochastic Models ===\n")
tryCatch({
  # Spatial model
  results_spatial <- run_plague_model(
    model = "stochastic",
    params = "defaults",
    spatial = TRUE,
    n_particles = 2,
    times = seq(0, 1, by = 0.1)
  )
  
  cat("Success! Spatial stochastic model completed\n")
  
  # Human model
  results_human <- run_plague_model(
    model = "stochastic", 
    params = "modern-estimates",
    include_humans = TRUE,
    n_particles = 2,
    times = seq(0, 1, by = 0.1)
  )
  
  cat("Success! Human stochastic model completed\n")
  
}, error = function(e) {
  cat("Error in stochastic models:", e$message, "\n")
})

# Test 5: Parameter comparison
cat("\n=== Parameter Set Comparison ===\n")
param_sets <- c("defaults", "keeling-gilligan", "modern-estimates", "historical")
R0_values <- sapply(param_sets, function(name) {
  params <- load_parameters(name)
  calculate_R0(params)
})

cat("R0 values by parameter set:\n")
for (i in seq_along(R0_values)) {
  cat(sprintf("  %s: %.2f\n", names(R0_values)[i], R0_values[i]))
}

# Test 6: Custom parameters
cat("\n=== Testing Custom Parameters ===\n")
tryCatch({
  # Modify parameters on the fly
  results_custom <- run_plague_model(
    model = "deterministic",
    params = "defaults",
    beta_r = 8.0,  # Higher transmission
    g_r = 0.05,    # Higher survival
    times = seq(0, 3, by = 0.1)
  )
  
  cat("Success! Custom parameter simulation completed\n")
  
  # Check the modified parameters
  custom_params <- attr(results_custom, "params")
  cat("Modified beta_r:", custom_params$beta_r, "\n")
  cat("Modified g_r:", custom_params$g_r, "\n")
  
}, error = function(e) {
  cat("Error with custom parameters:", e$message, "\n")
})

cat("\n=== TESTING COMPLETE ===\n")
cat("All major API components have been tested!\n")