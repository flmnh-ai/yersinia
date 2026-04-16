
<!-- README.md is generated from README.Rmd. Please edit that file -->

# yersinia <img src="man/figures/logo.png" align="right" height="138" alt="" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/flmnh-ai/plague-model/workflows/R-CMD-check/badge.svg)](https://github.com/flmnh-ai/plague-model/actions)
<!-- badges: end -->

> **Stochastic plague transmission modeling for epidemiological research
> and public health applications**

The `yersinia` package provides a comprehensive toolkit for modeling
plague transmission dynamics using realistic stochastic simulation.
Built on the `odin.dust` framework, it uses a carcass-based transmission
formulation (Didelot et al. 2017) to capture demographic stochasticity,
spatial spread, and multi-host dynamics.

## Why Stochastic Models?

Traditional deterministic plague models fail to capture: - **Small
population effects** where random events drive extinction/persistence -
**Spatial heterogeneity** in transmission and population structure -
**Uncertainty quantification** essential for risk assessment -
**Realistic outbreak variability** observed in natural systems

## Key Features

- 🎲 **Stochastic simulation** with demographic noise and realistic
  population dynamics
- 🗺️ **Spatial metapopulations** with migration and local adaptation
- 📚 **Evidence-based parameters** from historical and contemporary
  plague research\
- 🏥 **Multi-host dynamics** with carcass-based rat-to-human
  transmission
- 📊 **Professional analysis** tools for R₀, outbreak metrics, and
  spatial patterns
- 📈 **Publication-ready plotting** with uncertainty quantification

## Installation

Install the development version from GitHub:

``` r
# install.packages("pak")
pak::pak("flmnh-ai/plague-model")
```

## Quick Start

``` r
library(yersinia)
set.seed(42)

# Run a basic plague model
results <- run_plague_model(
  scenario = "keeling-gilligan",  # Keeling & Gilligan (2000) parameters
  years = 3,
  n_particles = 50              # 50 stochastic replicates
)

# Built-in plotting with uncertainty bands  
plot(results)
```

<img src="man/figures/README-quickstart-1.png" alt="" width="100%" />

## Core Capabilities

### Evidence-Based Parameter Sets

``` r
# Load curated parameter sets from the literature
params <- load_scenario("historical")  # Medieval Black Death parameters
print(params)
#> 🦠 Plague Scenario (historical)
#> 📄  Biological parameters for historical plague outbreaks (14th-17th centuries) 
#> 📚 Source:  Historical analysis and paleoepidemiology 
#> 
#> 🐀 Rat Population Parameters:
#>   r_r    =    0.016  # Rat population growth rate (per day)
#>   d_r    =    0.000  # Natural death rate of rats (per day)
#>   p      =    0.980  # Probability of inherited resistance
#> 
#> 💀 Carcass/Transmission Parameters:
#>   rho      =    2.000  # Rat carcass infectivity range
#>   delta_R  =    0.200  # Carcass decay rate
#> 
#> 🔬 Disease Parameters:
#>   beta_r =    1.000  # Transmission rate from carcasses to rats (per day)
#>   m_r    =    0.040  # Plague resolution rate in rats (per day)
#>   g_r    =    0.010  # Probability rat survives infection
#> 
#> 👤 Human Parameters:
#>   r_h    =    0.000  # Human population growth rate (per day)
#>   d_h    =    0.000  # Natural death rate of humans (per day)
#>   beta_h =    0.020  # Transmission rate from carcasses to humans (per day)
#>   beta_I =    0.030  # Human-to-human transmission rate (per day)
#>   m_h    =    0.080  # Plague resolution rate in humans (per day)
#>   g_h    =    0.050  # Probability human survives infection
#> 
#> ⚙️  Other Parameters:
#>   mu_r   =    0.000  # Rat movement rate (per day)
#> 
#> 📈 Basic Reproduction Number (R₀):  4.28 ✅ (Disease can spread)

# Calculate basic reproduction number
R0 <- calculate_R0(params)
cat("Historical R₀:", round(R0, 2))
#> Historical R₀: 4.28
```

### Professional Analysis Tools

``` r
# Comprehensive outbreak analysis
outbreak_stats <- results |>
  calculate_outbreak_metrics(compartment = "I") |>
  summarize_outbreak_metrics()

print(outbreak_stats[c("outbreak_probability", "mean_peak", "mean_duration")])
#> # A tibble: 1 × 3
#>   outbreak_probability mean_peak mean_duration
#>                  <dbl>     <dbl>         <dbl>
#> 1                 0.74      0.76        0.0733
```

### Spatial Modeling

``` r
# Multi-population spatial model
spatial_results <- run_plague_model(
  scenario = "modern-estimates",
  npop = 16,                    # 4x4 spatial grid
  K_r = 8000,                   # Total rat carrying capacity
  years = 8,
  n_particles = 30
)

# Compare single vs spatial dynamics  
plot_comparison(
  list("Single Population" = results, "Spatial Model" = spatial_results),
  compartment = "I"
)
```

<img src="man/figures/README-spatial-1.png" alt="" width="100%" />

### Historical Applications

``` r
# Model Black Death scenario
black_death <- run_plague_model(
  scenario = "historical",
  include_humans = TRUE,        # Include human transmission
  years = 5,
  n_particles = 40
)

# Focus on human epidemic dynamics
plot_dynamics(black_death, compartments = c("Ih", "Rh"))
```

<img src="man/figures/README-historical-1.png" alt="" width="100%" />

## Model Types

| Model | Description | Use Case |
|----|----|----|
| **Single Population** | Basic carcass-based dynamics | Parameter exploration, R₀ analysis |
| **Spatial** | Multi-population with migration | Landscape epidemiology, spatial spread |
| **Multi-host** | Carcass-based rat-to-human transmission | Epidemiological studies, intervention planning |

## Parameter Sets

| Scenario | Source | Description | R₀ |
|----|----|----|----|
| `"defaults"` | Package defaults | Baseline parameters | 2.62 |
| `"didelot"` | Didelot et al. (2017) | Cairo 1801 posterior estimates | 2.68 |
| `"keeling-gilligan"` | Keeling & Gilligan (2000) | Adapted to carcass formulation | 0.46 |
| `"modern-estimates"` | Contemporary research | Current parameter estimates | 2.77 |
| `"historical"` | Medieval records | Black Death era parameters | 4.28 |

## Getting Help

- 📖 **Comprehensive tutorial**: `vignette("yersinia-intro")`
- 🔍 **Function reference**: `help(package = "yersinia")`
- 🎯 **Main modeling function**: `?run_plague_model`
- 📐 **Deterministic comparisons**:
  `vignette("reference-deterministic-models")`

## Citation

If you use `yersinia` in your research, please cite:

``` r
citation("yersinia")
```

## Related Work

- **Didelot et al. (2017)**: Carcass-based plague transmission model
  ([J. R. Soc. Interface](https://doi.org/10.1098/rsif.2017.0160))
- **Keeling & Gilligan (2000)**: Foundational plague metapopulation
  model ([Nature](https://doi.org/10.1038/35038073))
- **odin.dust framework**: Stochastic compartmental modeling
  ([CRAN](https://cran.r-project.org/package=odin.dust))

------------------------------------------------------------------------

**License**: MIT \| **Bugs**: [GitHub
Issues](https://github.com/flmnh-ai/plague-model/issues)
