# ------------------------------------------------------------------------------
# app_cohort.R — outbreak-level summary: the backing table for the explorer.
#
# Pure dplyr aggregation. Lives outside the Shiny module so it's testable
# without a reactive context, and so users exploring the dataset interactively
# can call it the same way the app does.
#
# Defaults to `outbreaks_all` (130 records), not the curated nine: the picker
# is an explorer over Krauer's whole catalogue, and the curated set is now
# just a preset selection over the same table.
# ------------------------------------------------------------------------------

#' Per-outbreak summary table for the outbreak explorer.
#'
#' Aggregates the long-format [outbreaks_all] dataset to one row per outbreak,
#' with the metadata, summary statistics and screening flags the explorer
#' filters, sorts, maps and flags on.
#'
#' The returned columns beyond the obvious ones:
#'
#' \describe{
#'   \item{`label`}{Display string (`"Barcelona 1489"`), disambiguated by
#'     start month where place and year collide. For reading, never joining —
#'     join on `outbreak_id`.}
#'   \item{`fittable`}{Whether the app can fit this record at all. Requires
#'     both a population (`lab_fit_assemble()` pins `K_h` and `K_r` to it) and
#'     a fixed reporting window (the `D_h` accumulator resets every
#'     `obs_period` steps, which monthly records have no whole-day value for).
#'     52 of the 130 qualify.}
#'   \item{`unfit_reason`}{Which gate failed, in a phrase fit to show the
#'     user. `NA` when `fittable`.}
#'   \item{`attack_rate`}{Recorded deaths divided by population.}
#'   \item{`attack_flag`}{`"impossible"` above 100% (Klaipeda 1710 records
#'     9,797 deaths in a population of 4,000), `"high"` above 35% — the
#'     threshold at which a closed-population model stops being able to
#'     reproduce the series. It is a warning, not a filter: Prague 1713 and
#'     Eyam 1665 both sit at 37% and are in the curated nine.}
#'   \item{`peak_doy`}{Day of year (1-366) on which the largest observed
#'     count falls -- the outbreak's seasonal phase. Correlates with latitude
#'     at r = 0.64 across the catalogue: north of 52 N the mean peak is day
#'     242, south of 45 N it is day 180.}
#'   \item{`type`}{`"plague mortality"` or `"all-cause mortality"`. 54 of the
#'     130 are all-cause burials, for which `lambda_baseline` is carrying a
#'     different quantity — see the fitting notes in `CLAUDE.md`.}
#' }
#'
#' @param data Long-format outbreaks tibble. Defaults to the package's
#'   bundled [outbreaks_all] dataset.
#' @return A tibble with one row per `outbreak_id`, sorted by year.
#' @examples
#' outbreak_summary()
#' # Just the ones that can actually be fitted:
#' subset(outbreak_summary(), fittable)
#' @export
outbreak_summary <- function(data = NULL) {
  if (is.null(data)) data <- get("outbreaks_all", envir = asNamespace("yersinia"))
  # Columns added by the 2026-09-22 explorer rewrite. Tolerated as missing so
  # a caller passing an older long-format tibble still gets a summary rather
  # than an opaque dplyr error about an unknown column.
  optional <- c("label", "country", "lat", "lon", "type", "interval",
                "calendar", "complete", "sourcetype", "source", "fittable",
                "unfit_reason", "attack_rate", "attack_flag", "krauer_id",
                "start_date", "population_source")
  keys <- c("outbreak_id", "location", "year", "population", "obs_period",
            intersect(optional, names(data)))
  data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarise(
      n_obs         = sum(!is.na(.data$deaths)),
      n_missing     = sum(is.na(.data$deaths)),
      duration_days = max(.data$day, na.rm = TRUE),
      total_deaths  = sum(.data$deaths, na.rm = TRUE),
      peak_deaths   = max(.data$deaths, na.rm = TRUE),
      # which.max drops NAs, so this is the peak among observed values.
      peak_day      = .data$day[which.max(.data$deaths)],
      # Day-of-year of the peak: the seasonal phase of the outbreak, which
      # is what the thermal-forcing work is ultimately about. Computable for
      # all 130 only since the 2026-09 monthly-date fix -- before it, every
      # monthly record had NA dates and no phase at all.
      peak_date     = .data$date[which.max(.data$deaths)],
      peak_doy      = as.integer(format(.data$date[which.max(.data$deaths)],
                                        "%j")),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$year, .data$outbreak_id)
}
