# ------------------------------------------------------------------------------
# app_forward_sim.R — posterior predictive forward simulation for the lab app.
#
# Given a `setup` (from lab_fit_assemble) and a `samples` object from
# monty_sample, draws N posterior samples, runs the deterministic model
# forward for each one, and returns a long tibble of predicted observed
# deaths suitable for ggplot overlay on cohort data.
#
# Predicted "deaths" is the model's expected observation:
#   mu_t = p_obs * D_h_t + lambda_baseline * obs_period
# matching the NegBinomial likelihood mean used at fit time. Showing mu_t
# (not raw D_h) keeps the comparison apples-to-apples with the data.
# ------------------------------------------------------------------------------

#' Posterior predictive forward simulation from a Lab fit.
#'
#' Subsamples `n_draws` posterior draws (evenly spaced by `posterior::thin_draws`),
#' runs the deterministic plague humans model forward for each, and returns a
#' long-format tibble of per-draw predicted observation series. Used by the
#' hero plot to render the posterior fan over cohort data.
#'
#' Handles both the single-outbreak (flat packer) and multi-outbreak
#' (grouped packer) cases.
#'
#' @param setup Output of [lab_fit_assemble()].
#' @param samples A `monty_samples` object from [lab_fit_run()].
#' @param n_draws Target number of posterior draws to simulate. Default 100.
#' @param t_grid Optional integer vector of simulation times. Defaults to
#'   `seq(0, max(setup$data$time))`.
#' @return Tibble with columns `draw`, `group`, `time`, `mu` (predicted
#'   observation expectation per posterior draw).
#' @export
lab_fit_forward_sim <- function(setup, samples, n_draws = 100, t_grid = NULL) {
  if (!requireNamespace("posterior", quietly = TRUE)) {
    cli::cli_abort("Forward sim needs {.pkg posterior}.")
  }
  draws_df <- posterior::as_draws_df(samples)
  pn <- setup$packer$names()
  draws_df <- posterior::subset_draws(draws_df,
                                       variable = intersect(pn,
                                                            posterior::variables(draws_df)))
  thin <- max(1L, posterior::ndraws(draws_df) %/% n_draws)
  thinned <- posterior::thin_draws(draws_df, thin = thin)
  keep_ix <- seq_len(min(n_draws, posterior::ndraws(thinned)))
  thinned <- posterior::subset_draws(thinned, draw = keep_ix)

  if (is.null(t_grid)) {
    t_grid <- seq(0L, as.integer(max(setup$data$time, na.rm = TRUE)))
  }

  groups <- setup$cohort_ids
  is_grouped <- length(groups) > 1L

  one_draw <- function(theta, draw_id) {
    unpacked <- setup$packer$unpack(theta)
    if (!is_grouped) {
      unpacked <- stats::setNames(list(unpacked), groups)
    }
    do.call(rbind, lapply(groups, function(g) {
      pars <- unpacked[[g]]
      sys <- dust2::dust_system_create(plague_stochastic_humans,
                                       pars = pars,
                                       n_particles = 1L,
                                       deterministic = TRUE)
      dust2::dust_system_set_state_initial(sys)
      yy <- dust2::dust_system_simulate(sys, t_grid)
      D_h <- drop(dust2::dust_unpack_state(sys, yy)$D_h)
      p_obs  <- pars$p_obs %||% 1
      lambda <- pars$lambda_baseline %||% 0
      obs_pd <- pars$obs_period %||% 1
      tibble::tibble(draw = draw_id, group = g, time = t_grid,
                     mu = p_obs * D_h + lambda * obs_pd)
    }))
  }

  # Slice each draw as a plain named numeric vector. `as_draws_matrix`
  # carries class metadata that monty's packer rejects (row-slicing
  # preserves the matrix class even with one row); a plain data.frame is
  # what the existing vignettes use, and the packer accepts the resulting
  # named numeric.
  df <- as.data.frame(posterior::as_draws_df(thinned))[, pn, drop = FALSE]
  do.call(rbind, lapply(seq_len(nrow(df)), function(i) {
    one_draw(unlist(df[i, ]), draw_id = i)
  }))
}
