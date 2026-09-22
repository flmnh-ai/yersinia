test_that("basic model runs without error", {
  results <- run_plague_model(
    scenario = "defaults",
    npop = 1,
    n_particles = 10,
    years = 1
  )
  expect_s3_class(results, "plague_results")
  expect_true(all(results$value >= 0))
  # Compartments should be S, I, R, Q (carcass model)
  expect_true(all(c("S", "I", "R", "Q") %in% unique(results$compartment)))
})

test_that("human model runs without error", {
  results <- run_plague_model(
    scenario = "defaults",
    npop = 1,
    include_humans = TRUE,
    n_particles = 10,
    years = 1
  )
  expect_s3_class(results, "plague_results")
  expect_true(all(results$value >= 0))
  # Should have rat + human compartments
  expect_true(all(c("S", "I", "R", "Q", "Sh", "Ih", "Rh", "Dh") %in% unique(results$compartment)))
})

test_that("R0 calculation is correct", {
  params <- load_scenario("defaults")
  R0 <- calculate_R0(params)
  expect_true(is.numeric(R0))
  expect_true(R0 > 0)
})

test_that("initial infected rats are carved out of K_r", {
  sys <- dust2::dust_system_create(
    plague_stochastic_humans,
    pars = list(
      K_r = 100,
      I_ini = 5,
      r_r = 0,
      d_r = 0,
      r_h = 0,
      d_h = 0,
      beta_r = 0,
      beta_h = 0,
      beta_I = 0,
      seasonal = rep(1, 1),
      seasonal_beta = rep(1, 1)
    ),
    n_particles = 1
  )
  dust2::dust_system_set_state_initial(sys)
  state <- dust2::dust_unpack_state(sys, dust2::dust_system_state(sys))

  expect_equal(state$S, 95)
  expect_equal(state$I, 5)
  expect_equal(state$R, 0)
  expect_equal(state$S + state$I + state$R, 100)
})

test_that("R_ini > 0 partitions K_r into S + I + R", {
  sys <- dust2::dust_system_create(
    plague_stochastic_humans,
    pars = list(
      K_r = 100, I_ini = 5, R_ini = 20,
      r_r = 0, d_r = 0, r_h = 0, d_h = 0,
      beta_r = 0, beta_h = 0, beta_I = 0,
      seasonal = rep(1, 1), seasonal_beta = rep(1, 1)
    ),
    n_particles = 1
  )
  dust2::dust_system_set_state_initial(sys)
  state <- dust2::dust_unpack_state(sys, dust2::dust_system_state(sys))

  expect_equal(state$S, 75)
  expect_equal(state$I, 5)
  expect_equal(state$R, 20)
  expect_equal(state$S + state$I + state$R, 100)
})

test_that("didelot scenario keeps Cairo demography switches explicit", {
  params <- load_scenario("didelot")

  expect_equal(params$r_r, 0)
  expect_equal(params$d_r, 0)
  expect_equal(params$r_h, params$d_h)
  expect_equal(params$p_obs, 0.8)
})

test_that("all scenarios load successfully", {
  scenarios <- c("defaults", "keeling-gilligan", "modern-estimates", "historical", "didelot")
  for (s in scenarios) {
    params <- load_scenario(s)
    expect_s3_class(params, "scenario_parameters")
    # All should have core carcass model params
    expect_true(all(c("beta_r", "rho", "delta_R", "m_r", "g_r") %in% names(params)))
  }
})

test_that("obs_period = 1 reproduces the pre-accumulator deterministic LL", {
  # Regression check: when obs_period = 1, every step satisfies
  # `time %% 1 == 0`, so the if-branch is always taken and update(D_h)
  # collapses to `n_IR_h - n_recovered_h` -- the pre-2026 behaviour.
  # Baseline via expect_snapshot_value() rather than a literal, because the
  # literal went stale for a reason that had nothing to do with the model:
  # the 2026-09 data rewrite restored Barcelona's first 125 days (Krauer's
  # record starts 1489-11-05; the old hand-transcribed file began at
  # 1490-03-10), so the series went from 182 days to 307 and the likelihood
  # legitimately moved. A hard-coded number cannot tell "the model changed"
  # from "the data was corrected"; a snapshot makes the change show up as a
  # reviewable diff instead of a failure to hand-edit.
  #
  # Regenerate deliberately with testthat::snapshot_accept("models") after
  # confirming the move is explained by data, not by the model.
  data(outbreaks)
  barcelona <- outbreaks |>
    dplyr::filter(.data$outbreak_id == outbreak_resolve_id("Barcelona_1490")) |>
    dplyr::rename(time = "day") |>
    dplyr::select("time", "deaths")
  fixed_pars <- plague_fit_fixed_pars("didelot")
  fixed_pars$seasonal <- rep(1, max(barcelona$time))
  fixed_pars$seasonal_beta <- rep(1, max(barcelona$time))
  fixed_pars$obs_period <- 1
  unfilter <- dust2::dust_unfilter_create(plague_stochastic_humans,
                                          time_start = 0,
                                          data = barcelona)
  ll <- dust2::dust_likelihood_run(unfilter, fixed_pars)
  expect_snapshot_value(ll, style = "serialize", tolerance = 1e-8)
})

test_that("obs_period = 7 aggregates daily increments into weekly windows", {
  # Run the deterministic model twice over a fixed 182-step horizon (an
  # arbitrary length -- this test simulates rather than fitting, so it does
  # not depend on any outbreak's series):
  # once with obs_period = 1 (D_h is per-step deaths) and once with
  # obs_period = 7 (D_h is the running 7-day sum, resetting at each
  # multiple of 7). Then verify that summing the daily series in
  # 7-day windows matches the weekly series at week boundaries.
  fixed_pars <- plague_fit_fixed_pars("didelot")
  fixed_pars$seasonal <- rep(1, 182)
  fixed_pars$seasonal_beta <- rep(1, 182)

  run_one <- function(obs_period) {
    pars <- fixed_pars
    pars$obs_period <- obs_period
    sys <- dust2::dust_system_create(plague_stochastic_humans,
                                     pars = pars, n_particles = 1,
                                     deterministic = TRUE)
    dust2::dust_system_set_state_initial(sys)
    y <- dust2::dust_system_simulate(sys, times = seq_len(182))
    state_list <- dust2::dust_unpack_state(sys, y)
    drop(state_list$D_h)
  }
  daily   <- run_one(1)
  weekly  <- run_one(7)

  # week boundaries: time = 7, 14, ..., 182 (= 26 weeks of full data)
  week_ends <- seq(7, 182, by = 7)
  daily_sums  <- vapply(week_ends, function(t) sum(daily[(t-6):t]), numeric(1))
  weekly_at_end <- weekly[week_ends]
  expect_equal(weekly_at_end, daily_sums, tolerance = 1e-8)

  # Sanity: weekly value at non-boundary days should be a partial
  # accumulation that is < the full weekly sum.
  expect_true(all(weekly[seq(1, 6)] <= weekly[7]))
})

test_that("London 1563 fits without error at obs_period = 7", {
  data(outbreaks)
  london <- outbreaks |>
    dplyr::filter(.data$outbreak_id == outbreak_resolve_id("London_1563")) |>
    dplyr::rename(time = "day") |>
    dplyr::select("time", "deaths")
  setup <- plague_fit_setup(london,
                            scenario = "historical",
                            n_particles = 100,
                            n_threads = 1,
                            obs_period = 7L,
                            smoke_test = TRUE)
  expect_equal(setup$fixed_pars$obs_period, 7L)
})

test_that("validate_obs_period rejects mismatched cadence", {
  # obs_period > 1 but tau != 1 is incoherent (no extra resolution to give)
  expect_error(validate_obs_period(7L, tau = 7),
               "obs_period > 1 requires tau = 1")
  # data times must all be multiples of obs_period
  expect_error(validate_obs_period(7L, tau = 1, data_time = c(7, 14, 20, 28)),
               "multiples of obs_period")
  # well-formed call returns the value invisibly
  expect_equal(validate_obs_period(7L, tau = 1, data_time = c(7, 14, 21)), 7L)
})
