# Human-readable parameter labels for posterior / trace panels.

test_that("labels pair a description with the symbol", {
  expect_equal(param_label("beta_h"),
               "Carcass-to-human transmission (β_h)")
  expect_equal(param_label("kappa"),
               "Observation overdispersion (κ)")
  expect_equal(param_label("delta_R"), "Carcass decay rate (δ_R)")
})

test_that("non-Greek parameters use their own symbol", {
  expect_equal(param_label("K_r"), "Rat carrying capacity (K_r)")
  expect_equal(param_label("g_h"), "Human survival probability (g_h)")
})

test_that("with_symbol = FALSE gives the bare description", {
  expect_equal(param_label("beta_r", with_symbol = FALSE),
               "Carcass-to-rat transmission")
})

test_that("grouped names keep their group", {
  lbl <- param_label("beta_h<Eyam_1665>")
  expect_match(lbl, "Carcass-to-human transmission")
  expect_match(lbl, "Eyam_1665")
  # Group names can contain digits and underscores; the split must not eat them.
  expect_match(param_label("I_ini<Barcelona_1490>"), "Barcelona_1490$")
})

test_that("unknown parameters pass through unchanged", {
  # A parameter added to the model must still plot, just unprettified.
  expect_equal(param_label("not_a_parameter"), "not_a_parameter")
  expect_equal(param_label("newthing<Eyam_1665>"), "newthing<Eyam_1665>")
})

test_that("labelling is vectorised and length-preserving", {
  x <- c("beta_h", "kappa", "mystery")
  out <- param_label(x)
  expect_length(out, 3)
  expect_equal(out[[3]], "mystery")
  expect_equal(param_label(character(0)), character(0))
})

test_that("every fittable parameter has a description", {
  # configurable_param_names() is what the UI offers; anything there without a
  # label would show up as a raw name in a posterior panel.
  missing <- setdiff(configurable_param_names(),
                     names(.param_descriptions()))
  expect_equal(missing, character(0))
})

test_that("the source file stays ASCII", {
  # Greek is written as \uXXXX escapes; literal UTF-8 in package source trips
  # R CMD check on some platforms.
  f <- system.file("R", package = "yersinia")
  path <- file.path("..", "..", "R", "app_param_labels.R")
  skip_if_not(file.exists(path))
  raw <- readBin(path, "raw", file.size(path))
  expect_true(all(as.integer(raw) < 128))
})
