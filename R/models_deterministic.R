# R/models_deterministic.R
# Deterministic plague models using odin

#' Create base deterministic plague model (rats and fleas only)
#' @return odin model generator
#' @export
create_deterministic_model <- function() {
  # Load model from inst/odin/ directory
  model_path <- system.file("odin", "deterministic_basic.R", package = "yersinia")
  if (model_path == "") {
    # Fallback for development mode
    model_path <- file.path("inst", "odin", "deterministic_basic.R")
  }
  odin::odin(model_path)
}

#' Create deterministic plague model with seasonal forcing
#' @return odin model generator
#' @export
create_seasonal_deterministic_model <- function() {
  # Load model from inst/odin/ directory
  model_path <- system.file("odin", "deterministic_seasonal.R", package = "yersinia")
  if (model_path == "") {
    # Fallback for development mode
    model_path <- file.path("inst", "odin", "deterministic_seasonal.R")
  }
  odin::odin(model_path)
}

#' Create deterministic plague model with human dynamics
#' @return odin model generator  
#' @export
create_deterministic_human_model <- function() {
  # Load model from inst/odin/ directory
  model_path <- system.file("odin", "deterministic_humans.R", package = "yersinia")
  if (model_path == "") {
    # Fallback for development mode
    model_path <- file.path("inst", "odin", "deterministic_humans.R")
  }
  odin::odin(model_path)
}