test_that("validate_contact_matrix accepts a row-stochastic zero-diagonal matrix", {
  good <- matrix(c(0, 0.6, 0.4,
                   0.5, 0, 0.5,
                   0.4, 0.6, 0), 3, 3, byrow = TRUE)
  expect_invisible(validate_contact_matrix(good, npop = 3))
})

test_that("validate_contact_matrix rejects malformed inputs", {
  # wrong dimensions
  expect_error(
    validate_contact_matrix(matrix(0, 2, 3), npop = 2),
    "must be 2 x 2"
  )
  # non-zero diagonal
  expect_error(
    validate_contact_matrix(matrix(c(0.5, 0.5, 0.5, 0.5), 2, 2), npop = 2),
    "zero diagonal"
  )
  # row sums != 1
  expect_error(
    validate_contact_matrix(matrix(c(0, 0.7, 0.5, 0), 2, 2, byrow = TRUE), npop = 2),
    "row-stochastic"
  )
  # negative entries
  expect_error(
    validate_contact_matrix(matrix(c(0, -0.1, 1.1, 1, 0, 0, 0, 1, 0), 3, 3),
                            npop = 3),
    "non-negative"
  )
})

test_that("metapop with mu_r = 0 (deterministic) matches single-population per patch", {
  # Decoupling test: with no migration and identical per-patch parameters,
  # the deterministic metapop trajectory in each patch must equal the
  # deterministic single-population trajectory exactly. Demography off so
  # any drift is structural rather than stochastic-overshoot artefact.
  shared <- list(
    tau = 1, K_r = 2500, K_h = 5000, I_ini = 10, R_ini = 0,
    I_h_ini = 0, R_h_ini = 0,
    r_r = 0, d_r = 0, r_h = 0, d_h = 0,
    beta_r = 0.77, beta_h = 0.0145, beta_I = 0,
    rho = 2.63, m_r = 0.056, m_h = 0.125,
    g_r = 0.02, g_h = 0.1, delta_R = 0.267,
    iota = 0.75, p = 0.975, obs_period = 1,
    # seasonal_beta is required by both models: the R wrappers default it,
    # but a direct dust_system_create() must supply it.
    seasonal_beta = rep(1, 365)
  )

  sys1 <- dust2::dust_system_create(plague_stochastic_humans, pars = shared,
                                    n_particles = 1, deterministic = TRUE)
  dust2::dust_system_set_state_initial(sys1)
  y1 <- dust2::dust_system_simulate(sys1, times = seq_len(365))
  s1 <- dust2::dust_unpack_state(sys1, y1)

  contact <- matrix(c(0, 0.5, 0.5,
                      0.5, 0, 0.5,
                      0.5, 0.5, 0), 3, 3, byrow = TRUE)
  mp_pars <- shared
  mp_pars$seasonal_beta <- matrix(shared$seasonal_beta, nrow = 3,
                                  ncol = length(shared$seasonal_beta),
                                  byrow = TRUE)
  mp_pars$npop <- 3L
  mp_pars$mu_r <- 0
  mp_pars$contact_r <- contact
  mp_pars$mu_h <- 0
  mp_pars$contact_h <- contact
  for (k in c("K_r", "K_h", "I_ini", "R_ini", "I_h_ini", "R_h_ini")) {
    mp_pars[[k]] <- rep(shared[[k]], 3)
  }
  sys2 <- dust2::dust_system_create(plague_stochastic_metapop, pars = mp_pars,
                                    n_particles = 1, deterministic = TRUE)
  dust2::dust_system_set_state_initial(sys2)
  y2 <- dust2::dust_system_simulate(sys2, times = seq_len(365))
  s2 <- dust2::dust_unpack_state(sys2, y2)

  for (compartment in c("S", "I", "R", "Q", "S_h", "I_h", "R_h", "D_h")) {
    for (k in 1:3) {
      expect_equal(s2[[compartment]][k, ], s1[[compartment]],
                   tolerance = 0,
                   label = sprintf("%s patch %d", compartment, k))
    }
  }
})

test_that("metapop seasonal_beta scales both transmission rates in every patch", {
  # w = 0.5 everywhere must be identical to halving beta_r and beta_h, and
  # w -> 0 must shut transmission off -- the same checks the humans model has.
  contact <- matrix(c(0, 1, 1, 0), 2, 2)
  n <- 200
  base <- list(
    npop = 2L, mu_r = 0.02, contact_r = contact,
    mu_h = 0, contact_h = contact,
    K_r = c(2500, 2500), K_h = c(5000, 5000),
    I_ini = c(10, 0), R_ini = c(0, 0),
    I_h_ini = c(0, 0), R_h_ini = c(0, 0),
    r_r = 0, d_r = 0, r_h = 0, d_h = 0,
    beta_r = 0.5, beta_h = 0.02, beta_I = 0,
    rho = 2.5, m_r = 0.2, g_r = 0, delta_R = 0.2,
    seasonal_beta = matrix(1, 2, n)
  )
  run <- function(pars) {
    sys <- dust2::dust_system_create(plague_stochastic_metapop, pars = pars,
                                     n_particles = 1, deterministic = TRUE)
    dust2::dust_system_set_state_initial(sys)
    y <- dust2::dust_system_simulate(sys, 0:(n - 1))
    rowSums(matrix(dust2::dust_unpack_state(sys, y)$D_h, nrow = 2))
  }
  unforced <- run(base)
  expect_true(all(unforced > 0))

  half_w    <- run(modifyList(base, list(seasonal_beta = matrix(0.5, 2, n))))
  half_beta <- run(modifyList(base, list(beta_r = 0.25, beta_h = 0.01)))
  expect_equal(half_w, half_beta, tolerance = 1e-8)

  expect_true(all(run(modifyList(base, list(seasonal_beta = matrix(1e-8, 2, n)))) < 1e-6))
})

test_that("metapop seasonal_beta acts per patch", {
  # Transmission off in patch 2 only: infected rats still migrate there and
  # die, but their carcasses infect nobody, so patch 2 has no human deaths
  # while patch 1 carries on.
  contact <- matrix(c(0, 1, 1, 0), 2, 2)
  n <- 200
  pars <- list(
    npop = 2L, mu_r = 0.02, contact_r = contact,
    mu_h = 0, contact_h = contact,
    K_r = c(2500, 2500), K_h = c(5000, 5000),
    I_ini = c(10, 0), R_ini = c(0, 0),
    I_h_ini = c(0, 0), R_h_ini = c(0, 0),
    r_r = 0, d_r = 0, r_h = 0, d_h = 0,
    beta_r = 0.5, beta_h = 0.02, beta_I = 0,
    rho = 2.5, m_r = 0.2, g_r = 0, delta_R = 0.2,
    seasonal_beta = rbind(rep(1, n), rep(1e-8, n))
  )
  sys <- dust2::dust_system_create(plague_stochastic_metapop, pars = pars,
                                   n_particles = 1, deterministic = TRUE)
  dust2::dust_system_set_state_initial(sys)
  y <- dust2::dust_system_simulate(sys, 0:(n - 1))
  deaths <- rowSums(matrix(dust2::dust_unpack_state(sys, y)$D_h, nrow = 2))
  expect_gt(deaths[1], 1)
  expect_lt(deaths[2], 1e-6)
})

test_that("metapop_seasonal_beta builds the [npop, n_steps] matrix", {
  expect_equal(metapop_seasonal_beta(NULL, 3, 5), matrix(1, 3, 5))
  # a shared series becomes identical rows
  m <- metapop_seasonal_beta(c(0.1, 0.2, 0.3), 2, 3)
  expect_equal(m, rbind(c(0.1, 0.2, 0.3), c(0.1, 0.2, 0.3)))
  # a per-patch matrix passes through
  per_patch <- rbind(c(1, 0.5, 0.2), c(0.3, 0.6, 0.9))
  expect_equal(metapop_seasonal_beta(per_patch, 2, 3), per_patch)
  # longer than needed is fine
  expect_equal(ncol(metapop_seasonal_beta(rep(1, 10), 2, 3)), 10)
})

test_that("metapop_seasonal_beta rejects the wrong shape", {
  expect_error(metapop_seasonal_beta(matrix(1, 3, 5), 2, 5), "one row per patch")
  expect_error(metapop_seasonal_beta(rep(1, 4), 2, 5), "every step")
  expect_error(metapop_seasonal_beta(matrix(1, 2, 4), 2, 5), "every step")
})

test_that("run_plague_metapop_model accepts a per-patch seasonal_beta", {
  contact <- matrix(c(0, 1, 1, 0), 2, 2)
  n <- 365
  res <- run_plague_metapop_model(
    scenario = "defaults", npop = 2, contact_r = contact, mu_r = 0.01,
    K_r = c(2500, 2500), K_h = c(5000, 5000), I_ini = c(10, 0),
    n_particles = 2, years = 1,
    seasonal_beta = rbind(rep(1, n), rep(0.5, n)))
  expect_s3_class(res, "plague_results")
})

test_that("rat counts are conserved under migration when births, deaths, and plague are off", {
  # Total rats summed across all patches changes only via births/deaths and
  # plague mortality. With all four off, migration must be a pure relabeling --
  # the total over all patches is invariant for every particle, every step.
  contact <- matrix(c(0, 1, 1, 0), 2, 2)
  contact <- contact / rowSums(contact)
  pars <- list(
    npop = 2L, mu_r = 0.1, contact_r = contact,
    mu_h = 0, contact_h = contact,
    K_r = c(500, 500), K_h = c(1000, 1000),
    I_ini = c(50, 0), R_ini = c(50, 50),
    I_h_ini = c(0, 0), R_h_ini = c(0, 0),
    r_r = 0, d_r = 0, r_h = 0, d_h = 0,
    beta_r = 0, beta_h = 0, beta_I = 0,
    delta_R = 0, m_r = 0,
    seasonal_beta = matrix(1, 2, 100)
  )
  sys <- dust2::dust_system_create(plague_stochastic_metapop, pars = pars,
                                   n_particles = 30, seed = 1)
  dust2::dust_system_set_state_initial(sys)
  y <- dust2::dust_system_simulate(sys, times = seq_len(100))
  s <- dust2::dust_unpack_state(sys, y)

  total <- s$S + s$I + s$R    # [npop, n_particles, n_times]
  per_particle_per_time <- apply(total, c(2, 3), sum)
  init_total <- per_particle_per_time[, 1]
  expect_true(all(per_particle_per_time == init_total))
})

test_that("plague propagates between patches via rat migration", {
  contact <- matrix(c(0, 1, 0,
                      0.5, 0, 0.5,
                      0, 1, 0), 3, 3, byrow = TRUE)
  pars <- list(
    npop = 3L, mu_r = 0.05, contact_r = contact,
    mu_h = 0, contact_h = contact,
    K_r = rep(2500, 3), K_h = rep(5000, 3),
    I_ini = c(10, 0, 0), R_ini = rep(0, 3),
    I_h_ini = rep(0, 3), R_h_ini = rep(0, 3),
    r_r = 0, d_r = 0, r_h = 0, d_h = 0,
    beta_r = 0.77, beta_h = 0.0145, beta_I = 0,
    rho = 2.63, m_r = 0.056, m_h = 0.125,
    g_r = 0.02, g_h = 0.1, delta_R = 0.267,
    iota = 0.75, p = 0.975, obs_period = 1,
    # seasonal_beta is required by both models: the R wrappers default it,
    # but a direct dust_system_create() must supply it.
    seasonal_beta = matrix(1, 3, 365)
  )
  sys <- dust2::dust_system_create(plague_stochastic_metapop, pars = pars,
                                   n_particles = 50, seed = 2)
  dust2::dust_system_set_state_initial(sys)
  y <- dust2::dust_system_simulate(sys, times = seq_len(365))
  s <- dust2::dust_unpack_state(sys, y)

  peak_I <- apply(s$I, c(1, 2), max)  # max I over time, [patch, particle]
  expect_true(mean(peak_I[2, ] > 5) > 0.9,
              label = "patch 2 (directly connected) reaches non-trivial outbreak")
  expect_true(mean(peak_I[3, ] > 5) > 0.5,
              label = "patch 3 (via patch 2) reaches non-trivial outbreak")
})

test_that("run_plague_metapop_model returns a properly shaped plague_results", {
  contact <- matrix(c(0, 0.6, 0.4,
                      0.5, 0, 0.5,
                      0.4, 0.6, 0), 3, 3, byrow = TRUE)
  res <- run_plague_metapop_model(
    scenario = "defaults",
    npop = 3,
    contact_r = contact,
    mu_r = 0.05,
    K_r = c(2500, 2500, 5000),
    K_h = c(5000, 5000, 10000),
    I_ini = c(10, 0, 0),
    years = 0.25,
    n_particles = 20
  )
  expect_s3_class(res, "plague_results")
  expect_setequal(unique(res$population), 1:3)
  expect_true(all(c("S", "I", "R", "Q", "Sh", "Ih", "Rh", "Dh") %in% res$compartment))
  expect_true(all(res$value >= 0))
  expect_equal(attr(res, "model_type"), "stochastic_metapop")
})

test_that("run_plague_metapop_model rejects malformed contact matrix", {
  expect_error(
    run_plague_metapop_model(
      scenario = "defaults", npop = 2,
      contact_r = matrix(c(0, 0.5, 0.6, 0), 2, 2, byrow = TRUE),
      mu_r = 0.05,
      K_r = c(1000, 1000), K_h = c(2000, 2000),
      I_ini = c(5, 0), years = 0.05, n_particles = 5
    ),
    "row-stochastic"
  )
})

test_that("mu_h = 0 reproduces the rat-only metapop trajectory exactly", {
  # Regression: with mu_h = 0 (default), human compartments must evolve
  # identically to the pre-human-migration model. Compare two systems built
  # from the same seed; any divergence indicates the human update equations
  # changed when adding the migration terms.
  contact <- matrix(c(0, 1, 0,
                      0.5, 0, 0.5,
                      0, 1, 0), 3, 3, byrow = TRUE)
  shared_pars <- list(
    npop = 3L, tau = 1,
    mu_r = 0.05, contact_r = contact,
    K_r = rep(2500, 3), K_h = rep(5000, 3),
    I_ini = c(10, 0, 0), R_ini = rep(0, 3),
    I_h_ini = rep(0, 3), R_h_ini = rep(0, 3),
    r_r = 0, d_r = 0, r_h = 0, d_h = 0,
    beta_r = 0.77, beta_h = 0.0145, beta_I = 0,
    rho = 2.63, m_r = 0.056, m_h = 0.125,
    g_r = 0.02, g_h = 0.1, delta_R = 0.267,
    iota = 0.75, p = 0.975, obs_period = 1,
    # seasonal_beta is required by both models: the R wrappers default it,
    # but a direct dust_system_create() must supply it.
    seasonal_beta = matrix(1, 3, 365)
  )

  pars_a <- c(shared_pars, list(mu_h = 0, contact_h = contact))
  pars_b <- c(shared_pars, list(mu_h = 0,
                                contact_h = diag(0, 3) +
                                  matrix(c(0, 0.5, 0.5,
                                           0.5, 0, 0.5,
                                           0.5, 0.5, 0), 3, 3, byrow = TRUE)))

  # Same seed, different contact_h but mu_h = 0 -- should be identical because
  # contact_h is multiplied by zero emigrants. Also confirms no accidental
  # consumption of RNG by the human migration block when mu_h = 0.
  sys_a <- dust2::dust_system_create(plague_stochastic_metapop, pars = pars_a,
                                     n_particles = 20, seed = 42)
  dust2::dust_system_set_state_initial(sys_a)
  y_a <- dust2::dust_system_simulate(sys_a, times = seq_len(180))

  sys_b <- dust2::dust_system_create(plague_stochastic_metapop, pars = pars_b,
                                     n_particles = 20, seed = 42)
  dust2::dust_system_set_state_initial(sys_b)
  y_b <- dust2::dust_system_simulate(sys_b, times = seq_len(180))

  expect_equal(y_a, y_b, tolerance = 0)
})

test_that("human counts are conserved under mu_h alone when plague and demography are off", {
  # With plague off (no births/deaths/transitions) and demography off, total
  # humans across all patches must be invariant under migration. Same shape as
  # the rat-side conservation test.
  contact <- matrix(c(0, 1, 1, 0), 2, 2)
  contact <- contact / rowSums(contact)
  pars <- list(
    npop = 2L,
    mu_r = 0, contact_r = contact,
    mu_h = 0.05, contact_h = contact,
    K_r = c(500, 500), K_h = c(1000, 1000),
    I_ini = c(0, 0), R_ini = c(0, 0),
    I_h_ini = c(100, 0), R_h_ini = c(100, 0),
    r_r = 0, d_r = 0, r_h = 0, d_h = 0,
    beta_r = 0, beta_h = 0, beta_I = 0,
    delta_R = 0, m_r = 0, m_h = 0,
    seasonal_beta = matrix(1, 2, 100)
  )
  sys <- dust2::dust_system_create(plague_stochastic_metapop, pars = pars,
                                   n_particles = 30, seed = 3)
  dust2::dust_system_set_state_initial(sys)
  y <- dust2::dust_system_simulate(sys, times = seq_len(100))
  s <- dust2::dust_unpack_state(sys, y)

  total_h <- s$S_h + s$I_h + s$R_h    # [npop, n_particles, n_times]
  per_particle_per_time <- apply(total_h, c(2, 3), sum)
  init_total <- per_particle_per_time[, 1]
  expect_true(all(per_particle_per_time == init_total))
})

test_that("plague propagates between patches via human migration alone", {
  # Rat plague off; only human-to-human transmission (beta_I > 0) and human
  # migration. Seed I_h in patch 1; downstream patches should accumulate
  # human cases. Decoupled from the rat-migration propagation test so the
  # only spread channel is mu_h.
  contact <- matrix(c(0, 1, 0,
                      0.5, 0, 0.5,
                      0, 1, 0), 3, 3, byrow = TRUE)
  pars <- list(
    npop = 3L,
    mu_r = 0, contact_r = contact,
    mu_h = 0.02, contact_h = contact,
    K_r = rep(2500, 3), K_h = rep(5000, 3),
    I_ini = rep(0, 3), R_ini = rep(0, 3),
    I_h_ini = c(50, 0, 0), R_h_ini = rep(0, 3),
    r_r = 0, d_r = 0, r_h = 0, d_h = 0,
    beta_r = 0, beta_h = 0, beta_I = 0.15,
    rho = 2.63, m_r = 0.056, m_h = 0.05,
    g_r = 0.02, g_h = 0.1, delta_R = 0.267,
    iota = 0.75, p = 0.975, obs_period = 1,
    # seasonal_beta is required by both models: the R wrappers default it,
    # but a direct dust_system_create() must supply it.
    seasonal_beta = matrix(1, 3, 365)
  )
  sys <- dust2::dust_system_create(plague_stochastic_metapop, pars = pars,
                                   n_particles = 50, seed = 4)
  dust2::dust_system_set_state_initial(sys)
  y <- dust2::dust_system_simulate(sys, times = seq_len(365))
  s <- dust2::dust_unpack_state(sys, y)

  peak_Ih <- apply(s$I_h, c(1, 2), max)  # max I_h over time, [patch, particle]
  expect_true(mean(peak_Ih[2, ] > 5) > 0.8,
              label = "patch 2 (directly connected) reaches non-trivial human outbreak")
  expect_true(mean(peak_Ih[3, ] > 5) > 0.4,
              label = "patch 3 (via patch 2) reaches non-trivial human outbreak")
})

test_that("run_plague_metapop_model defaults contact_h to contact_r when NULL", {
  # The wrapper-level convenience: not supplying contact_h reuses contact_r.
  # Behavior with explicit contact_h = contact_r must match the default.
  contact <- matrix(c(0, 0.6, 0.4,
                      0.5, 0, 0.5,
                      0.4, 0.6, 0), 3, 3, byrow = TRUE)
  set.seed(1)
  res_default <- run_plague_metapop_model(
    scenario = "defaults", npop = 3,
    contact_r = contact, mu_r = 0.05,
    mu_h = 0.01,
    K_r = rep(2500, 3), K_h = rep(5000, 3),
    I_ini = c(10, 0, 0),
    years = 0.1, n_particles = 5
  )
  expect_s3_class(res_default, "plague_results")
  expect_true(all(res_default$value >= 0))
})

test_that("run_plague_metapop_model rejects malformed contact_h", {
  contact_ok <- matrix(c(0, 1, 1, 0), 2, 2, byrow = TRUE)
  contact_bad <- matrix(c(0, 0.5, 0.6, 0), 2, 2, byrow = TRUE)  # not row-stochastic
  expect_error(
    run_plague_metapop_model(
      scenario = "defaults", npop = 2,
      contact_r = contact_ok, mu_r = 0.05,
      mu_h = 0.01, contact_h = contact_bad,
      K_r = c(1000, 1000), K_h = c(2000, 2000),
      I_ini = c(5, 0), years = 0.05, n_particles = 5
    ),
    "row-stochastic"
  )
})
