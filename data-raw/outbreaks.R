# ------------------------------------------------------------------------------
# data-raw/outbreaks.R -- build the outbreak datasets from Krauer's catalogue.
#
# SOURCE OF TRUTH: data-raw/krauer-plague-season-v3_2.csv, a vendored copy of
# the `input/rawdata.csv` from Fabienne Krauer's plague-season v3.2 dataset
# (130 outbreaks, 84 places, 1348-1878). Vendored rather than referenced so
# this package rebuilds without the black-death repo present.
#
# WHY THIS FILE WAS REWRITTEN (2026-09-22)
#
# The previous version hand-transcribed eight series with no dates and no
# source citations. Checking them against Krauer value-by-value found:
#
#   * `cairo_1835` was TWO outbreaks concatenated -- Cairo 1801 (Didelot 2017,
#     185 days, 8,018 deaths) followed by Cairo 1835 (Gaetani 1841, 181 days,
#     33,532 deaths). A single-epidemic model was being asked to explain two
#     epidemics in sequence. They are now separate outbreaks.
#   * `barcelona_1490` silently dropped the first 125 days of a 307-day record,
#     so its day 1 was 1490-03-10, not the 1489-11-05 anyone would assume.
#   * `malta_1813` and `prague_1713` recorded 0 where Krauer records *no
#     observation* (11 and 17 days). A missing record is not an observed zero;
#     the likelihood was being asked to explain troughs that may not exist.
#   * `florence_1400` was truncated by 8 days.
#
# Deriving everything from Krauer removes that whole class of error and adds
# what the seasonal-forcing work needs: a real start date and calendar system
# per outbreak.
#
# TWO DATASETS
#
#   `outbreaks`     -- the curated nine, kept as a convenience subset.
#   `outbreaks_all` -- all 130, the dataset the explorer works from.
#
# Both share one schema and one key scheme, so the app can widen its
# selection without a migration.
#
# IDENTITY (changed 2026-09-22, when the explorer widened to all 130)
#
# `outbreak_id` is Krauer's integer `id` as a character string. It used to be
# `place_startyear`, which is *not unique*: Krauer 5 and 6 are both
# "Alexandria_1840", and 7 and 8 are both "Alexandria_1842". Since
# `cohort_data()` subsets with `outbreak_id %in% cohort_ids`, each of those
# pairs would have been spliced into a single group -- exactly the
# concatenation bug the Cairo_1835 fix above was written to remove,
# reintroduced one dataset over. A derived key cannot carry a uniqueness
# guarantee; Krauer's own id can.
#
# `label` ("Barcelona 1489") is the human-readable display string, and is
# disambiguated where place+year collides. It is for reading, never joining.
#
# The nine curated outbreaks previously carried hand-assigned string ids.
# Eight matched their natural `place_startyear` spelling exactly; the ninth,
# `Barcelona_1490`, did not -- Krauer's startyear for that record is 1489,
# which is also the right one (the series begins 1489-11-05; the old
# hand-transcribed file dropped its first 125 days and so appeared to start in
# 1490). Those legacy strings are exported as `outbreak_aliases` so saved
# sessions and fit-library entries keyed on them still resolve.
# ------------------------------------------------------------------------------

library(dplyr)

RAW <- "data-raw/krauer-plague-season-v3_2.csv"

# The curated nine, keyed by Krauer id, paired with the legacy string id the
# app used before 2026-09-22. The pairing is now only an *alias* -- it is
# exported as `outbreak_aliases` and consumed by `outbreak_resolve_id()` so
# old saved cohorts keep resolving. It no longer sets `outbreak_id`.
# Cairo appears twice because the old `Cairo_1835` was two outbreaks.
CURATED <- tibble::tribble(
  ~krauer_id, ~legacy_id,
          18, "Barcelona_1490",  # Krauer startyear is 1489; see header
          82, "Malta_1813",
          42, "Florence_1400",
          31, "Cairo_1801",     # was the first 185 days of the old Cairo_1835
          32, "Cairo_1835",
          41, "Eyam_1665",
          101, "Prague_1713",
          69, "London_1563",
          49, "Givry_1348"
)

# Populations Krauer does not carry. Both were in the hand-transcribed file;
# their original source is unrecorded, so they are flagged rather than trusted
# silently. Anything relying on these two should say so.
POP_OVERRIDE <- tibble::tribble(
  ~krauer_id, ~population, ~population_source,
          42,       60000, "inherited from pre-2026 hand-transcribed file; origin unrecorded",
          49,        3000, "inherited from pre-2026 hand-transcribed file; origin unrecorded",
          # Cairo 1801 carries no population in Krauer, but Krauer cites
          # Didelot 2017 as the source for the record itself, and that paper's
          # Table 1 gives N_H = 250,000 -- the same figure inst/scenarios/
          # didelot.yaml already uses for K_h. Same outbreak, same source.
          31,      250000, "Didelot et al. (2017) Table 1 (N_H); Krauer carries no population for this record"
)

# encoding= is not optional: Krauer carries accented place names
# (Conde-sur-Noireau, Malaga), and without it the strings come back marked
# "unknown" and render as mojibake for anyone whose locale differs from
# whoever last rebuilt the .rda.
raw <- utils::read.csv(RAW, stringsAsFactors = FALSE, encoding = "UTF-8")

# `n` is the death count; blank means no observation, which must stay NA
# rather than becoming 0. The whole point of the rewrite.
raw$deaths <- suppressWarnings(as.numeric(trimws(raw$n)))
# Monthly records carry `YYYY-MM`, which `as.Date()` returns NA for -- so
# before 2026-09-22 every one of the 577 monthly rows had NA `date`, and
# therefore NA `start_date` and NA `day`, across all 46 monthly outbreaks.
# Nothing fitted them so nothing caught it; the explorer plots them. Anchor
# a month to its first day: the day index is then the first of the month the
# window opens, and `obs_period` stays NA because the window length still
# varies between 28 and 31 days.
raw$dateorig <- trimws(raw$dateorig)
raw$date <- as.Date(ifelse(nchar(raw$dateorig) == 7L,
                           paste0(raw$dateorig, "-01"), raw$dateorig))

# Reporting window in days. The odin model's D_h accumulator resets every
# `obs_period` steps, so this has to be a whole number of days: monthly series
# have no fixed window and are marked NA (and so are not fittable).
interval_days <- function(x) {
  dplyr::case_when(
    x == "daily"    ~ 1L,
    x == "weekly"   ~ 7L,
    x == "biweekly" ~ 14L,
    TRUE            ~ NA_integer_
  )
}

per_outbreak <- raw |>
  dplyr::group_by(krauer_id = .data$id) |>
  dplyr::arrange(.data$date, .by_group = TRUE) |>
  dplyr::mutate(
    start_date = min(.data$date),
    obs_period = interval_days(.data$interval),
    # Day index on the model's clock: 1-based for daily series, and for
    # coarser records the day the window CLOSES, so `deaths` is the total
    # over the obs_period days ending at `day`. Monthly series have no fixed
    # window (`obs_period` is NA), so they get the day the window OPENS
    # instead -- enough to place them on a timeline, and they are unfittable
    # anyway.
    #
    # Weekly and biweekly indices are SNAPPED to the reporting grid rather
    # than taken raw from the date difference, because the historical bills
    # are not perfectly regular: London 1563 has one six-day week
    # (1563-07-17 to 07-23), and from that point every raw index is off the
    # 7-multiple grid. `validate_obs_period()` requires each data `time` to
    # be a multiple of `obs_period` -- the D_h accumulator resets on that
    # period -- so a single six-day bill made all 27 later observations
    # unfittable. 7 of the 50 weekly/biweekly series have this.
    #
    # Rounding (not flooring) to the nearest multiple keeps genuine gaps: a
    # missing bill still steps 14 days, because it rounds to 14. Only the
    # sub-period jitter is absorbed, which is the right call -- a six-day
    # bill is a seven-day slot that the calendar shortened, not six days of
    # plague.
    raw_day = as.integer(.data$date - .data$start_date),
    day = dplyr::if_else(
      is.na(.data$obs_period),
      .data$raw_day + 1L,
      as.integer(round(.data$raw_day / .data$obs_period) *
                   .data$obs_period) + .data$obs_period
    )
  ) |>
  dplyr::ungroup()

all_outbreaks <- per_outbreak |>
  dplyr::left_join(POP_OVERRIDE, by = "krauer_id") |>
  dplyr::mutate(
    population_krauer = suppressWarnings(as.numeric(.data$population.x)),
    population = dplyr::coalesce(.data$population_krauer, .data$population.y),
    population_source = dplyr::case_when(
      !is.na(.data$population_krauer) ~ "Krauer plague-season v3.2",
      !is.na(.data$population.y)      ~ .data$population_source,
      TRUE                            ~ NA_character_
    ),
    # Krauer's integer id, as character. Unique by construction -- see the
    # IDENTITY note in this file's header for why a derived key is not.
    outbreak_id = as.character(.data$krauer_id),
    location = .data$place,
    year = as.integer(.data$startyear)
  )

# Display label. `place year` where that is unique, and disambiguated by the
# start month where it is not (Alexandria 1840 and 1842 each cover two
# distinct Krauer records). Built from the per-outbreak first row so the
# suffix is stable regardless of row order.
label_tbl <- all_outbreaks |>
  dplyr::distinct(.data$outbreak_id, .data$location, .data$year,
                  .data$start_date) |>
  dplyr::mutate(base = paste(.data$location, .data$year)) |>
  dplyr::group_by(.data$base) |>
  dplyr::mutate(
    label = if (dplyr::n() == 1L) .data$base
            else paste0(.data$base, " (",
                        format(.data$start_date, "%b"), ")")
  ) |>
  dplyr::ungroup() |>
  dplyr::select("outbreak_id", "label")

all_outbreaks <- all_outbreaks |>
  dplyr::left_join(label_tbl, by = "outbreak_id") |>
  dplyr::select(
    "outbreak_id", "label", "location", "year", "population", "day", "deaths",
    "obs_period",
    # Provenance and the fields the seasonal-forcing work needs.
    "krauer_id", "start_date", "date", "calendar", "interval",
    "country", "lat", "lon", "source", "population_source",
    # Explorer facets. `type` matters more than it looks: 54 of the 130
    # records are all-cause burials rather than plague-specific deaths, and
    # the likelihood's `lambda_baseline` means something different for each
    # (see CLAUDE.md, "Fitting").
    "type", "complete", "sourcetype"
  ) |>
  dplyr::arrange(.data$year, .data$outbreak_id, .data$day)

# ------------------------------------------------------------------------------
# Per-outbreak derived fields the explorer filters and flags on.
#
# Fittable = the app can actually fit it. Both conditions are hard
# requirements, not preferences: lab_fit_assemble() pins K_h/K_r to
# population, and the D_h accumulator needs a fixed-length window. Only 49 of
# the 130 clear both. `unfit_reason` carries *which* gate failed, because a
# greyed-out tile that cannot say why is just a broken tile.
#
# `attack_rate` is recorded deaths / population, and `attack_flag` marks the
# records a closed-population model cannot reproduce. It is a warning, not a
# filter: Prague 1713 and Eyam 1665 both sit at 37% and are in the curated
# nine, so screening at 35% would eject two outbreaks this package fits by
# default. Only Klaipeda 1710 (9,797 deaths in a population of 4,000 = 245%)
# is arithmetically impossible rather than merely hard. See stan/cohort.R,
# where the 35% screen collapsed the sampler's step size to 0.0012.
# ------------------------------------------------------------------------------
all_outbreaks <- all_outbreaks |>
  dplyr::group_by(.data$outbreak_id) |>
  dplyr::mutate(
    total_deaths = sum(.data$deaths, na.rm = TRUE),
    attack_rate  = .data$total_deaths / .data$population[1],
    fittable     = !is.na(.data$population[1]) && !is.na(.data$obs_period[1]),
    unfit_reason = dplyr::case_when(
      .data$fittable                 ~ NA_character_,
      is.na(.data$population[1]) &
        is.na(.data$obs_period[1])   ~ "no population, and monthly records have no fixed reporting window",
      is.na(.data$population[1])     ~ "no population in Krauer; K_h and K_r are pinned to it",
      TRUE                           ~ "monthly records have no fixed reporting window"
    ),
    attack_flag = dplyr::case_when(
      is.na(.data$attack_rate[1]) ~ NA_character_,
      .data$attack_rate[1] > 1    ~ "impossible",
      .data$attack_rate[1] > 0.35 ~ "high",
      TRUE                        ~ NA_character_
    )
  ) |>
  dplyr::ungroup()

outbreaks_all <- all_outbreaks

# The curated nine, as a plain subset. Same ids, same schema -- the only
# difference is which rows are present.
outbreaks <- all_outbreaks |>
  dplyr::filter(.data$krauer_id %in% CURATED$krauer_id) |>
  dplyr::arrange(.data$year, .data$outbreak_id, .data$day)

# Legacy string ids -> current ids, for resolving saved cohorts.
outbreak_aliases <- CURATED |>
  dplyr::transmute(
    legacy_id = .data$legacy_id,
    outbreak_id = as.character(.data$krauer_id)
  )

# Report *which* outbreak fails rather than just that one does -- a bare
# `all(fittable)` failure sends you hunting through 130 records.
unfittable <- outbreaks |>
  dplyr::filter(!.data$fittable) |>
  dplyr::distinct(.data$outbreak_id, .data$label, .data$unfit_reason)
if (nrow(unfittable) > 0) {
  print(as.data.frame(unfittable))
  stop("Curated outbreaks are not all fittable; see the table above. ",
       "A missing population needs a POP_OVERRIDE entry; a missing ",
       "obs_period means the series is monthly and cannot be fitted.")
}

# Identity assertions. The first is the one that matters: the whole point of
# keying on Krauer's id is that one outbreak_id is one outbreak, and the
# previous place_startyear scheme silently violated it four records over.
id_per_krauer <- all_outbreaks |>
  dplyr::distinct(.data$outbreak_id, .data$krauer_id)
# Every fittable series must sit on its own reporting grid, and no two
# observations may collide onto the same day index. validate_obs_period()
# enforces the first at fit time; catching it here names the outbreak.
grid_check <- all_outbreaks |>
  dplyr::filter(!is.na(.data$obs_period)) |>
  dplyr::group_by(.data$outbreak_id, .data$label) |>
  dplyr::summarise(off_grid = sum(.data$day %% .data$obs_period != 0),
                   collisions = dplyr::n() - dplyr::n_distinct(.data$day),
                   .groups = "drop") |>
  dplyr::filter(.data$off_grid > 0 | .data$collisions > 0)
if (nrow(grid_check) > 0) {
  print(as.data.frame(grid_check))
  stop("Some series are off their reporting grid or have colliding day ",
       "indices; see the table above.")
}

stopifnot(
  nrow(id_per_krauer) == dplyr::n_distinct(all_outbreaks$krauer_id),
  !anyDuplicated(id_per_krauer$outbreak_id),
  # Labels are for humans, but a duplicated one still makes the picker lie.
  !anyDuplicated(dplyr::distinct(all_outbreaks, .data$outbreak_id,
                                 .data$label)$label),
  dplyr::n_distinct(all_outbreaks$outbreak_id) == 130L,
  # Nine, not eight: Cairo split into 1801 and 1835.
  dplyr::n_distinct(outbreaks$outbreak_id) == 9L,
  nrow(outbreak_aliases) == 9L,
  all(outbreak_aliases$outbreak_id %in% outbreaks$outbreak_id),
  # NAs must survive: if this is 0 the missing-vs-zero bug is back.
  sum(is.na(outbreaks$deaths)) > 0
)

message("outbreaks:     ", dplyr::n_distinct(outbreaks$outbreak_id),
        " outbreaks, ", nrow(outbreaks), " rows, ",
        sum(is.na(outbreaks$deaths)), " missing observations")
message("outbreaks_all: ", dplyr::n_distinct(outbreaks_all$outbreak_id),
        " outbreaks, ",
        dplyr::n_distinct(outbreaks_all$outbreak_id[outbreaks_all$fittable]),
        " fittable, ",
        dplyr::n_distinct(
          outbreaks_all$outbreak_id[!is.na(outbreaks_all$attack_flag)]),
        " flagged on attack rate")

usethis::use_data(outbreaks, overwrite = TRUE)
usethis::use_data(outbreaks_all, overwrite = TRUE)
usethis::use_data(outbreak_aliases, overwrite = TRUE)
