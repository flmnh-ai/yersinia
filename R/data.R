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

#' Krauer's full plague-outbreak catalogue
#'
#' All 130 outbreaks from Fabienne Krauer's plague-season v3.2 dataset (84
#' places, 22 countries, 1348--1878), in the same long format and with the
#' same key scheme as [outbreaks], which is a nine-record subset of it. This
#' is the dataset the outbreak explorer works from.
#'
#' @format A tibble in long format, one row per observation:
#' \describe{
#'   \item{outbreak_id}{Krauer's catalogue id as a character string. Unique
#'     by construction. \strong{Not} \code{place_startyear}, which is not:
#'     Krauer 5 and 6 are both Alexandria 1840, and 7 and 8 are both
#'     Alexandria 1842.}
#'   \item{label}{Display string, e.g. \code{"Barcelona 1489"}, disambiguated
#'     by start month where place and year collide. For reading, not joining.}
#'   \item{location, year, country, lat, lon}{Place and time metadata.}
#'   \item{population}{Total population, or \code{NA}. Missing for 61 of the
#'     130 records.}
#'   \item{day}{Day index from the outbreak start. For daily series the day
#'     itself; for weekly and biweekly the day the reporting window closes;
#'     for monthly the day it opens.}
#'   \item{deaths}{Count for the window ending at \code{day}. \code{NA} means
#'     \emph{no observation} and must never be read as an observed zero.}
#'   \item{obs_period}{Reporting-window length in days (1, 7 or 14), or
#'     \code{NA} for monthly series, which have no whole-day window.}
#'   \item{krauer_id, start_date, date, calendar, interval, source,
#'     population_source}{Provenance, and the fields the seasonal-forcing
#'     work joins on.}
#'   \item{type}{\code{"plague mortality"} or \code{"all-cause mortality"};
#'     54 of the 130 are all-cause burials.}
#'   \item{complete, sourcetype}{Krauer's own completeness flag, and whether
#'     the series was transcribed from a table or digitised from a graph.}
#'   \item{fittable}{Whether the app can fit this record: needs both a
#'     population and a whole-day reporting window. 52 of the 130 qualify.}
#'   \item{unfit_reason}{Which requirement failed; \code{NA} when fittable.}
#'   \item{total_deaths, attack_rate}{Recorded deaths, and that over
#'     population.}
#'   \item{attack_flag}{\code{"high"} above 35% and \code{"impossible"}
#'     above 100%. A warning, not a screen — Prague 1713 and Eyam 1665 sit
#'     at 37% and are both in the curated [outbreaks] subset.}
#' }
#'
#' @source Krauer, F. plague-season v3.2 (\code{input/rawdata.csv}), vendored
#'   at \code{data-raw/krauer-plague-season-v3_2.csv}.
#' @seealso [outbreak_summary()] for the one-row-per-outbreak view,
#'   [outbreak_resolve_id()] for translating pre-2026-09 identifiers.
#' @examples
#' # How much of the catalogue is actually modellable, and why not:
#' table(outbreak_summary()$unfit_reason, useNA = "ifany")
"outbreaks_all"

#' Pre-2026-09 outbreak identifiers
#'
#' Maps the nine hand-assigned string ids the app used before outbreaks were
#' rekeyed on Krauer's catalogue id to their current ids, so saved sessions
#' and fit-library entries keep resolving. Consumed by
#' [outbreak_resolve_id()].
#'
#' @format A tibble with 9 rows and 2 variables:
#' \describe{
#'   \item{legacy_id}{The old string, e.g. \code{"Barcelona_1490"}.}
#'   \item{outbreak_id}{The current id.}
#' }
#'
#' @details `"Barcelona_1490"` maps to the outbreak now labelled *Barcelona
#' 1489*. The year really does change: Krauer's record begins 1489-11-05, and
#' the old hand-transcribed file dropped its first 125 days. The other eight
#' legacy ids matched their natural spelling exactly.
"outbreak_aliases"
