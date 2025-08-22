# R/models_stochastic.R
# Stochastic plague models using odin.dust

#' Create spatial stochastic plague model 
#' @return odin.dust model generator
#' @export
create_spatial_stochastic_model <- function() {
  # Load model from inst/odin/ directory
  model_path <- system.file("odin", "plague_stochastic.R", package = "yersinia")
  if (model_path == "") {
    # Fallback for development mode
    model_path <- file.path("inst", "odin", "plague_stochastic.R")
  }
  odin.dust::odin_dust(model_path)
}

#' Create human stochastic plague model
#' @return odin.dust model generator
#' @export  
create_human_stochastic_model <- function() {
  # Load model from inst/odin/ directory
  model_path <- system.file("odin", "plague_stochastic_humans.R", package = "yersinia")
  if (model_path == "") {
    # Fallback for development mode
    model_path <- file.path("inst", "odin", "plague_stochastic_humans.R")
  }
  odin.dust::odin_dust(model_path)
}