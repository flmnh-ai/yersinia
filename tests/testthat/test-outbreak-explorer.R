# Explorer data layer: identity, fittability, and the screening flags the
# picker greys and badges on.

test_that("outbreak_id is unique per outbreak in the full catalogue", {
  s <- outbreak_summary()
  expect_equal(nrow(s), 130L)
  expect_false(anyDuplicated(s$outbreak_id) > 0)
  # The regression this key scheme exists for: `place_startyear` collapsed
  # Krauer 5+6 and 7+8 into two ids, splicing four Alexandria outbreaks into
  # two. If that ever comes back, these four stop being four.
  alex <- s[s$location == "Alexandria" & s$year %in% c(1840, 1842), ]
  expect_equal(nrow(alex), 4L)
  expect_equal(length(unique(alex$outbreak_id)), 4L)
})

test_that("labels are unique and disambiguate colliding place-years", {
  s <- outbreak_summary()
  expect_false(anyDuplicated(s$label) > 0)
  expect_true(all(grepl("^Alexandria 1840",
                        s$label[s$location == "Alexandria" & s$year == 1840])))
})

test_that("curated outbreaks are a subset of the full catalogue", {
  expect_true(all(unique(outbreaks$outbreak_id) %in%
                    unique(outbreaks_all$outbreak_id)))
  expect_equal(length(unique(outbreaks$outbreak_id)), 9L)
})

test_that("legacy outbreak ids still resolve", {
  expect_equal(outbreak_resolve_id("London_1563"), "69")
  # Barcelona's year genuinely moves: Krauer's record starts 1489-11-05.
  expect_equal(outbreak_label("Barcelona_1490"), "Barcelona 1489")
  # Current ids pass through, unknown ids pass through unchanged.
  expect_equal(outbreak_resolve_id("69"), "69")
  expect_equal(outbreak_resolve_id("not-an-outbreak"), "not-an-outbreak")
  expect_equal(outbreak_resolve_id(character(0)), character(0))
})

test_that("fittable requires both a population and a whole-day window", {
  s <- outbreak_summary()
  expect_equal(sum(s$fittable), 52L)
  expect_true(all(!is.na(s$population[s$fittable])))
  expect_true(all(!is.na(s$obs_period[s$fittable])))
  # Every unfittable record can say why; every fittable one stays quiet.
  expect_true(all(!is.na(s$unfit_reason[!s$fittable])))
  expect_true(all(is.na(s$unfit_reason[s$fittable])))
  # Monthly series are unfittable regardless of population.
  expect_true(all(!s$fittable[s$interval == "monthly"]))
})

test_that("the attack-rate flag warns without screening the curated set", {
  s <- outbreak_summary()
  expect_equal(s$attack_flag[s$label == "Klaipeda 1710"], "impossible")
  # The two curated outbreaks above 35% must still be fittable -- flagging
  # them is the point, filtering them out would eject them from the set this
  # package fits by default.
  for (lab in c("Prague 1713", "Eyam 1665")) {
    expect_equal(s$attack_flag[s$label == lab], "high")
    expect_true(s$fittable[s$label == lab])
  }
})

test_that("monthly outbreaks carry real dates", {
  # Before 2026-09-22 all 577 monthly rows parsed to NA, taking start_date
  # and day with them -- so 46 outbreaks had no position on any timeline.
  monthly <- outbreaks_all[outbreaks_all$interval == "monthly", ]
  expect_gt(nrow(monthly), 0)
  expect_false(any(is.na(monthly$date)))
  expect_false(any(is.na(monthly$day)))
})

test_that("cohort_check_fittable rejects unfittable outbreaks by name", {
  unfit <- outbreak_summary()
  unfit <- unfit$outbreak_id[!unfit$fittable][1]
  expect_error(cohort_check_fittable(unfit), "cannot be fitted")
  expect_silent(cohort_check_fittable("69"))
})

test_that("cohort_data accepts legacy ids and keeps outbreaks separate", {
  d <- cohort_data(c("London_1563", "Eyam_1665"))
  expect_setequal(unique(d$group), c("69", "41"))
})
