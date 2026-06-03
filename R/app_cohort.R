# ------------------------------------------------------------------------------
# app_cohort.R — outbreak-level summary used by the cohort picker.
#
# Pure dplyr aggregation. Lives outside the Shiny module so it's testable
# without a reactive context, and so users exploring the dataset interactively
# can call it the same way the app does.
# ------------------------------------------------------------------------------

#' Per-outbreak summary table for the cohort picker.
#'
#' Aggregates the long-format [outbreaks] dataset to one row per outbreak,
#' with the metadata + summary stats the cohort picker displays (location,
#' year, population, duration, total deaths, peak day, observation period).
#'
#' @param data Long-format outbreaks tibble. Defaults to the package's
#'   bundled [outbreaks] dataset.
#' @return A tibble with one row per `outbreak_id`, sorted by year.
#' @examples
#' outbreak_summary()
#' @export
outbreak_summary <- function(data = NULL) {
  if (is.null(data)) data <- get("outbreaks", envir = asNamespace("yersinia"))
  data |>
    dplyr::group_by(.data$outbreak_id, .data$location, .data$year,
                    .data$population, .data$obs_period) |>
    dplyr::summarise(
      duration_days = max(.data$day, na.rm = TRUE),
      total_deaths  = sum(.data$deaths, na.rm = TRUE),
      peak_deaths   = max(.data$deaths, na.rm = TRUE),
      peak_day      = .data$day[which.max(.data$deaths)],
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$year)
}
