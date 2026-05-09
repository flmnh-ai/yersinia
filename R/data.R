#' Historical plague outbreak data
#'
#' Historical plague mortality data from multiple European and Middle Eastern
#' cities spanning from 1348 to 1835, compiled by Dean et al. (2018).
#'
#' @format A tibble with 1,451 rows and 7 variables:
#' \describe{
#'   \item{outbreak_id}{Unique identifier for each outbreak (location_year)}
#'   \item{location}{City name}
#'   \item{year}{Year of the outbreak}
#'   \item{population}{Total population of the city}
#'   \item{day}{Calendar day of the observation, 1-indexed from outbreak
#'     start. For weekly outbreaks (London 1563) this is the day at the
#'     end of the reporting week (7, 14, ...).}
#'   \item{deaths}{Death count for the reporting window ending at \code{day};
#'     daily for most outbreaks, weekly for London 1563 -- see caveats
#'     below re: plague-specific vs all-cause across sources}
#'   \item{obs_period}{Length in days of the reporting window each
#'     \code{deaths} value covers (1 for daily, 7 for London 1563). Pass
#'     this to \code{\link{plague_fit_setup}} via the \code{obs_period}
#'     argument so the model's \code{D_h} accumulator aggregates over a
#'     matching window.}
#' }
#'
#' @details
#' The dataset includes plague outbreaks from:
#' \itemize{
#'   \item Barcelona 1490 (daily data, 182 days)
#'   \item Malta 1813 (daily data, 209 days)
#'   \item Florence 1400 (daily data, 180 days)
#'   \item Cairo 1835 (daily data, 366 days)
#'   \item Eyam 1665 (daily data, 145 days)
#'   \item Prague 1713 (daily data, 198 days)
#'   \item London 1563 (weekly data, 33 weeks; \code{obs_period = 7})
#'   \item Givry 1348 (daily data with some missing values, 138 days)
#' }
#'
#' Some datasets contain missing values (NA) where historical records were
#' incomplete. London 1563 carries weekly Bills of Mortality counts -- the
#' \code{obs_period = 7} flag signals that to fitting code, and \code{day}
#' steps in increments of 7.
#'
#' \strong{Cause-of-death provenance.} The source documents differ in whether
#' they record plague-specific deaths or all burials during the outbreak. This
#' matters when fitting to the model's \code{D_h} (plague deaths only). Summary:
#' \itemize{
#'   \item Plague-specific (cause distinguished in source): Barcelona 1490
#'     (\emph{cerca de morts}), London 1563 (Bills of Mortality), Malta 1813,
#'     Prague 1713, Cairo 1835
#'   \item All-cause burial records, but with negligible baseline vs. epidemic
#'     peak: Givry 1348 (parish register, ~1,200 pop), Eyam 1665 (parish
#'     register, ~700 pop). Fittable as plague-specific in practice.
#'   \item Ambiguous: Florence 1400 (\emph{Libri dei Morti}, inconsistent
#'     cause-of-death annotation)
#' }
#' See \code{data-raw/outbreaks.R} for per-outbreak primary-source citations.
#'
#' Users can filter to specific outbreaks using the outbreak_id or location/year:
#' \code{
#' # Get Barcelona data
#' barcelona <- plague_outbreaks |> filter(outbreak_id == "Barcelona_1490")
#'
#' # Get all 15th century outbreaks
#' fifteenth_century <- plague_outbreaks |> filter(year >= 1400 & year < 1500)
#' }
#'
#' @source Dean, K.R., Krauer, F., Walløe, L., Lingjærde, O.C., Bramanti, B.,
#' Stenseth, N.C. and Schmid, B.V., 2018. Human ectoparasites and the spread
#' of plague in Europe during the Second Pandemic. Proceedings of the National
#' Academy of Sciences, 115(6), pp.1304-1309.
#'
#' @examples
#' # View structure of the data
#' str(outbreaks)
#'
#' # Summary statistics by outbreak
#' outbreaks |>
#'   group_by(outbreak_id, location, year) |>
#'   summarise(
#'     duration_days = max(day),
#'     total_deaths = sum(deaths, na.rm = TRUE),
#'     peak_deaths = max(deaths, na.rm = TRUE),
#'     population = first(population)
#'   )
"outbreaks"
