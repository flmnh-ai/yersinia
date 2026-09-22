# ------------------------------------------------------------------------------
# app_param_labels.R -- human-readable names for model parameters.
#
# Posterior plots faceted by `beta_h`, `delta_R`, `kappa` are readable only if
# you already hold the model in your head. These labels pair a plain-language
# name with the symbol, so a panel reads "Carcass-to-human transmission (beta_h)"
# and stays findable in the code.
#
# Descriptions are taken from the parameter block of
# inst/odin/plague_stochastic_humans.R, not paraphrased, so the app and the
# model cannot drift apart silently.
#
# Greek letters are written as \uXXXX escapes rather than literal UTF-8: R
# package sources should stay ASCII, and a literal beta here would trip
# R CMD check on some platforms.
# ------------------------------------------------------------------------------

# Symbols. Greek where the literature uses Greek, plain otherwise.
.param_symbols <- function() {
  c(
    tau             = "\u03c4",
    beta_r          = "\u03b2_r",
    beta_h          = "\u03b2_h",
    beta_I          = "\u03b2_I",
    rho             = "\u03c1",
    kappa           = "\u03ba",
    delta_R         = "\u03b4_R",
    iota            = "\u03b9",
    lambda_baseline = "\u03bb_base",
    R0              = "R\u2080",
    seasonal_beta   = "w_\u03b2"
  )
}

# name -> plain-language description. Anything absent falls back to the raw
# name, so a parameter added to the model still plots, just unprettified.
.param_descriptions <- function() {
  c(
    tau             = "Time step",
    I_ini           = "Initial infected rats",
    R_ini           = "Initial resistant rats",
    K_r             = "Rat carrying capacity",
    K_h             = "Human population",
    r_r             = "Rat growth rate",
    r_h             = "Human growth rate",
    p               = "Inherited resistance probability",
    d_r             = "Rat natural death rate",
    d_h             = "Human natural death rate",
    beta_r          = "Carcass-to-rat transmission",
    beta_h          = "Carcass-to-human transmission",
    beta_I          = "Human-to-human transmission",
    rho             = "Carcass infectivity range",
    m_r             = "Rat plague resolution rate",
    m_h             = "Human plague resolution rate",
    g_r             = "Rat survival probability",
    g_h             = "Human survival probability",
    delta_R         = "Carcass decay rate",
    kappa           = "Observation overdispersion",
    p_obs           = "Reporting probability",
    iota            = "Resistant-rat fecundity",
    I_h_ini         = "Initial infected humans",
    R_h_ini         = "Initial immune humans",
    lambda_baseline = "Baseline non-plague deaths",
    obs_period      = "Observation window",
    seasonal_beta   = "Thermal forcing on transmission",
    # Thermal-curve parameters (fitted only in the hierarchical cohort work).
    R0              = "Basic reproduction number",
    T_opt           = "Thermal optimum",
    hw_cold         = "Cold-side width (to zero)",
    hw_hot          = "Hot-side width (to zero)"
  )
}

#' Human-readable label for a model parameter.
#'
#' Renders `beta_h` as `"Carcass-to-human transmission (beta_h)"`, with the
#' symbol in Greek where the literature uses Greek. Grouped parameter names
#' from [monty::monty_packer_grouped()] (`param<group>`) keep their group,
#' appended after a separator, so per-outbreak panels stay distinguishable.
#'
#' Unknown names pass through unchanged -- a parameter added to the model still
#' plots, it just isn't prettified yet.
#'
#' @param x Character vector of parameter names.
#' @param with_symbol Include the symbol in parentheses. Default `TRUE`.
#' @return Character vector of labels, same length as `x`.
#' @export
#' @examples
#' param_label(c("beta_h", "kappa", "beta_h<Eyam_1665>"))
param_label <- function(x, with_symbol = TRUE) {
  desc <- .param_descriptions()
  syms <- .param_symbols()
  vapply(as.character(x), function(nm) {
    if (is.na(nm) || !nzchar(nm)) return(nm)
    # Split monty's grouped form: "beta_h<Eyam_1665>".
    grp <- NA_character_
    m <- regmatches(nm, regexec("^(.*)<(.+)>$", nm))[[1]]
    base <- if (length(m) == 3) { grp <- m[[3]]; m[[2]] } else nm
    # `[[` on a named atomic vector throws "subscript out of bounds" for a
    # name that isn't there -- %||% never gets a chance to run -- so test
    # membership first.
    if (!base %in% names(desc)) return(nm)
    d <- desc[[base]]
    sym <- if (base %in% names(syms)) syms[[base]] else base
    out <- if (isTRUE(with_symbol)) sprintf("%s (%s)", d, sym) else d
    if (!is.na(grp)) paste0(out, " \u00b7 ", grp) else out
  }, character(1), USE.NAMES = FALSE)
}

# Facet labeller: pretty names, wrapped so long ones don't overflow a strip.
.param_labeller <- function(width = 30) {
  function(labels) {
    labels[] <- lapply(labels, function(v) param_label(v))
    ggplot2::label_wrap_gen(width = width)(labels)
  }
}
