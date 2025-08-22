# Demonstration of the complete plague modeling package
# This script showcases all the major features of the cleaned-up API

library(tidyverse)
library(odin)
library(odin.dust)
library(yaml)

# Source all package files
source("R/plague_utils.R")
source("R/analysis.R") 
source("R/models_deterministic.R")
source("R/plotting.R")

cat("🦠 PLAGUE MODELING PACKAGE DEMONSTRATION 🦠\n")
cat(paste(rep("=", 50), collapse = ""), "\n\n")

# ═══════════════════════════════════════════════════════════════════════════════
# 1. PARAMETER MANAGEMENT
# ═══════════════════════════════════════════════════════════════════════════════

cat("📊 PARAMETER MANAGEMENT\n")
cat(paste(rep("-", 30), collapse = ""), "\n")

# Load different parameter sets
params_default <- load_parameters("defaults")
params_historical <- load_parameters("historical") 
params_modern <- load_parameters("modern-estimates")

cat("Available parameter sets loaded:\n")
cat("• Defaults - R0 =", round(calculate_R0(params_default), 2), "\n")
cat("• Historical - R0 =", round(calculate_R0(params_historical), 2), "\n") 
cat("• Modern - R0 =", round(calculate_R0(params_modern), 2), "\n\n")

# Parameter modification on-the-fly
cat("Parameter modification example:\n")
print(load_parameters("defaults", beta_r = 8.0, K_r = 5000))

# ═══════════════════════════════════════════════════════════════════════════════
# 2. DETERMINISTIC MODELS
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n🔬 DETERMINISTIC MODELS\n")
cat(paste(rep("-", 30), collapse = ""), "\n")

# Basic rat-flea model
cat("Running basic deterministic model...\n")
results_basic <- run_plague_model(
  model = "deterministic",
  params = "keeling-gilligan", 
  times = seq(0, 10, by = 0.1)
)

cat("✓ Basic model completed - compartments:", 
    paste(unique(results_basic$compartment), collapse = ", "), "\n")

# Model with humans
cat("Running deterministic model with humans...\n")
results_humans <- run_plague_model(
  model = "deterministic",
  params = "modern-estimates",
  include_humans = TRUE,
  times = seq(0, 8, by = 0.1)
)

cat("✓ Human model completed - compartments:", 
    paste(unique(results_humans$compartment), collapse = ", "), "\n")

# ═══════════════════════════════════════════════════════════════════════════════
# 3. ANALYSIS FUNCTIONS
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n📈 ANALYSIS FUNCTIONS\n")
cat(paste(rep("-", 30), collapse = ""), "\n")

# Outbreak metrics
metrics <- calculate_outbreak_metrics(results_basic, compartment = "I_r")
cat("Outbreak analysis:\n")
cat("• Peak infections:", round(max(metrics$peak_value), 1), "\n")
cat("• Outbreak duration:", round(max(metrics$duration), 1), "years\n")

# R0 comparison
cat("\nR0 comparison across parameter sets:\n")
R0_comparison <- tibble(
  Parameter_Set = c("defaults", "keeling-gilligan", "modern-estimates", "historical"),
  R0 = map_dbl(Parameter_Set, ~ calculate_R0(load_parameters(.x)))
)
print(R0_comparison)

# Equilibrium analysis
eq_historical <- calculate_equilibrium(params_historical)
cat("\nHistorical parameters equilibrium:", eq_historical$equilibrium_type, "\n")

# ═══════════════════════════════════════════════════════════════════════════════
# 4. ENHANCED PLOTTING
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n📊 PLOTTING CAPABILITIES\n")
cat(paste(rep("-", 30), collapse = ""), "\n")

# Basic time series plot
cat("Creating time series plot...\n")
p1 <- plot_dynamics(results_basic, compartments = c("S_r", "I_r", "R_r"))
print(p1)

# Phase portrait
cat("Creating phase portrait...\n") 
p2 <- plot_phase_portrait(results_basic, compartments = c("S_r", "I_r"))
print(p2)

# Model comparison
cat("Creating model comparison...\n")
comparison_results <- list(
  "Basic" = results_basic,
  "With_Humans" = results_humans
)
p3 <- plot_comparison(comparison_results, compartment = "I_r")
print(p3)

# ═══════════════════════════════════════════════════════════════════════════════
# 5. STOCHASTIC MODELS (if working)
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n🎲 STOCHASTIC MODELS\n")
cat(paste(rep("-", 30), collapse = ""), "\n")

tryCatch({
  # Spatial stochastic model
  cat("Attempting spatial stochastic model...\n")
  results_spatial <- run_plague_model(
    model = "stochastic",
    params = "defaults",
    spatial = TRUE,
    n_particles = 5,
    times = seq(0, 2, by = 0.1)
  )
  
  cat("✓ Spatial model completed\n")
  
  # Spatial visualization
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    cat("Creating spatial heatmap...\n")
    p4 <- plot_spatial_heatmap(results_spatial, time_point = 1.0, compartment = "I")
    print(p4)
  }
  
}, error = function(e) {
  cat("⚠️  Stochastic models need debugging:", e$message, "\n")
})

# ═══════════════════════════════════════════════════════════════════════════════
# 6. PARAMETER SENSITIVITY
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n🔍 SENSITIVITY ANALYSIS\n")
cat(paste(rep("-", 30), collapse = ""), "\n")

tryCatch({
  cat("Running sensitivity analysis on beta_r...\n")
  sensitivity_results <- run_sensitivity_analysis(
    params_default,
    param_name = "beta_r",
    range = seq(0.5, 1.5, by = 0.2)
  )
  
  cat("✓ Sensitivity analysis completed\n")
  
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    p5 <- plot_sensitivity(sensitivity_results, compartment = "I_r")
    print(p5)
  }
  
}, error = function(e) {
  cat("⚠️  Sensitivity analysis needs the deterministic models working\n")
})

# ═══════════════════════════════════════════════════════════════════════════════
# 7. PACKAGE SUMMARY
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n🎯 PACKAGE SUMMARY\n")
cat(paste(rep("=", 30), collapse = ""), "\n")

cat("✅ Features implemented:\n")
cat("  • Parameter management with YAML loading\n")
cat("  • Deterministic model implementations\n") 
cat("  • Custom plague_results and plague_parameters classes\n")
cat("  • Comprehensive analysis functions\n")
cat("  • Enhanced plotting capabilities\n")
cat("  • Clean, unified API interface\n\n")

cat("📁 File structure:\n")
cat("  • R/plague_utils.R - Core API and classes\n")
cat("  • R/models_deterministic.R - Deterministic model definitions\n")
cat("  • R/analysis.R - Analysis and metrics functions\n")
cat("  • R/plotting.R - Enhanced plotting functions\n")
cat("  • inst/parameters/*.yaml - Parameter sets from literature\n\n")

cat("🚀 Ready for R package development!\n")
cat("Next steps: Add DESCRIPTION, NAMESPACE, documentation, and tests.\n")

cat("\n", paste(rep("=", 50), collapse = ""), "\n")
cat("DEMONSTRATION COMPLETE! 🎉\n")