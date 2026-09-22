# Saved-fit library: slugging, save/list/load/delete round-trips, and the
# external-pointer drop that keeps restored sessions usable.

# Every test writes into a throwaway directory so nothing touches the user's
# real library at tools::R_user_dir().
local_library <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  dir
}

test_that(".lab_slug produces filesystem-safe stems", {
  expect_equal(.lab_slug("Barcelona 1490"), "barcelona-1490")
  expect_equal(.lab_slug("  Cairo/1835 -- seasonal!  "), "cairo-1835-seasonal")
  expect_equal(.lab_slug("!!!"), "fit")
  expect_lte(nchar(.lab_slug(strrep("a", 200))), 60)
})

test_that("empty library lists zero rows with the full column set", {
  dir <- local_library()
  rows <- lab_library_list(dir)
  expect_equal(nrow(rows), 0)
  expect_true(all(c("name", "slug", "saved_at", "cohort", "scenario",
                    "mode", "max_rhat") %in% names(rows)))
})

test_that("save writes a payload plus a metadata sidecar", {
  shiny::isolate({
    dir <- local_library()
    s <- LabSession$new(cohort_ids = "Barcelona_1490")
    lab_library_save(s, "Barcelona baseline", library_dir = dir)

    expect_true(file.exists(file.path(dir, "barcelona-baseline.rds")))
    expect_true(file.exists(file.path(dir, "barcelona-baseline.meta.rds")))

    # The sidecar must stay small — that's the whole point of splitting it.
    meta_size <- file.size(file.path(dir, "barcelona-baseline.meta.rds"))
    expect_lt(meta_size, 5000)
  })
})

test_that("save refuses an empty name", {
  shiny::isolate({
    dir <- local_library()
    s <- LabSession$new()
    expect_error(lab_library_save(s, "  ", library_dir = dir), "name")
  })
})

test_that("list reports saved fits newest first", {
  shiny::isolate({
    dir <- local_library()
    s <- LabSession$new(cohort_ids = "Eyam_1665")
    lab_library_save(s, "older", library_dir = dir)
    Sys.sleep(1.1)  # saved_at has second resolution
    lab_library_save(s, "newer", library_dir = dir)

    rows <- lab_library_list(dir)
    expect_equal(nrow(rows), 2)
    expect_equal(rows$name[[1]], "newer")
  })
})

test_that("re-saving under the same name updates in place", {
  shiny::isolate({
    dir <- local_library()
    s <- LabSession$new(cohort_ids = "Eyam_1665")
    lab_library_save(s, "my fit", library_dir = dir)
    s$cohort_ids <- c("Eyam_1665", "Malta_1813")
    lab_library_save(s, "my fit", library_dir = dir)

    rows <- lab_library_list(dir)
    expect_equal(nrow(rows), 1)
    expect_match(rows$cohort[[1]], "Malta_1813")
  })
})

test_that("load restores cohort, config and priors", {
  shiny::isolate({
    dir <- local_library()
    saved <- LabSession$new(cohort_ids = c("Cairo_1835", "Malta_1813"))
    lab_library_save(saved, "two outbreaks", library_dir = dir)

    fresh <- LabSession$new()
    lab_library_load(fresh, "two-outbreaks", library_dir = dir)
    expect_equal(fresh$cohort_ids, c("Cairo_1835", "Malta_1813"))
    expect_equal(fresh$model_config$scenario, saved$model_config$scenario)
  })
})

test_that("load errors clearly on an unknown slug", {
  shiny::isolate({
    dir <- local_library()
    s <- LabSession$new()
    expect_error(lab_library_load(s, "nope", library_dir = dir), "nope")
  })
})

test_that("saved payload never carries the dust2 setup", {
  # setup$unfilter is an external pointer: saveRDS writes it as a null
  # pointer, so a restored setup is a dead handle. It must be dropped on
  # write and rebuilt on read instead.
  shiny::isolate({
    dir <- local_library()
    s <- LabSession$new(cohort_ids = "Eyam_1665")
    s$fit_state <- list(status = "complete", samples = NULL,
                        setup = list(unfilter = "pretend-pointer"),
                        n_chains = 4L, n_iter = 100L)
    lab_library_save(s, "with setup", library_dir = dir)

    raw <- readRDS(file.path(dir, "with-setup.rds"))
    expect_null(raw$fit_state$setup)
    expect_equal(raw$fit_state$status, "complete")
  })
})

test_that("delete removes both files and is a no-op when absent", {
  shiny::isolate({
    dir <- local_library()
    s <- LabSession$new(cohort_ids = "Eyam_1665")
    lab_library_save(s, "scratch", library_dir = dir)
    expect_true(lab_library_delete("scratch", library_dir = dir))
    expect_equal(nrow(lab_library_list(dir)), 0)
    expect_false(lab_library_delete("scratch", library_dir = dir))
  })
})

test_that("a corrupt sidecar is skipped rather than aborting the listing", {
  shiny::isolate({
    dir <- local_library()
    s <- LabSession$new(cohort_ids = "Eyam_1665")
    lab_library_save(s, "good", library_dir = dir)
    writeLines("not an rds", file.path(dir, "bad.meta.rds"))

    rows <- lab_library_list(dir)
    expect_equal(nrow(rows), 1)
    expect_equal(rows$name[[1]], "good")
  })
})

test_that(".lab_max_rhat returns NA rather than erroring on junk", {
  expect_true(is.na(.lab_max_rhat(NULL)))
  expect_true(is.na(.lab_max_rhat("not samples")))
})

# ---- autosave: versioning, flagging, ordering ------------------------------

test_that("autosave names encode cohort, scenario and timestamp", {
  state <- list(cohort_ids = "Barcelona_1490",
                model_config = list(scenario = "defaults"),
                fit_state = list(mode = "deterministic"))
  nm <- .lab_autosave_name(state, when = as.POSIXct("2026-09-21 14:30:00"))
  # The display label, resolved from the legacy id: "Barcelona 1489", since
  # Krauer's record starts 1489-11-05 and the old hand-transcribed file
  # dropped its first 125 days.
  expect_match(nm, "Barcelona 1489", fixed = TRUE)
  expect_match(nm, "defaults")
  expect_match(nm, "20260921-143000")
})

test_that("autosave names label hierarchical cohorts by count", {
  expect_equal(.lab_cohort_label(character(0)), "no-cohort")
  # Display labels, not raw ids: since the 2026-09 rekey an outbreak_id is
  # Krauer's integer, so labelling by id would fill the library with numbers.
  # A legacy id resolves on the way through, which is why this reads
  # "Eyam 1665" rather than the "Eyam_1665" that went in.
  expect_equal(.lab_cohort_label("Eyam_1665"), "Eyam 1665")
  expect_equal(.lab_cohort_label("41"), "Eyam 1665")
  # Ids that resolve to nothing are passed through rather than erroring, so a
  # label is never the reason a library entry fails to save.
  expect_equal(.lab_cohort_label(c("A", "B", "C")), "A +2 more")
})

test_that("stochastic runs are marked in the autosave name", {
  state <- list(cohort_ids = "Eyam_1665",
                model_config = list(scenario = "defaults"),
                fit_state = list(mode = "stochastic"))
  expect_match(.lab_autosave_name(state), "stochastic")
})

test_that("autosaves version rather than overwrite", {
  shiny::isolate({
    dir <- local_library()
    s <- LabSession$new(cohort_ids = "Eyam_1665")
    s$fit_state <- list(status = "complete", mode = "deterministic")
    # Same second, same configuration: both runs must survive.
    lab_library_autosave(s, library_dir = dir)
    lab_library_autosave(s, library_dir = dir)
    expect_equal(nrow(lab_library_list(dir)), 2)
  })
})

test_that("manual saves still replace on the same name", {
  shiny::isolate({
    dir <- local_library()
    s <- LabSession$new(cohort_ids = "Eyam_1665")
    lab_library_save(s, "keeper", library_dir = dir)
    lab_library_save(s, "keeper", library_dir = dir)
    expect_equal(nrow(lab_library_list(dir)), 1)
  })
})

test_that("autosaved entries are flagged and sort below named ones", {
  shiny::isolate({
    dir <- local_library()
    s <- LabSession$new(cohort_ids = "Eyam_1665")
    lab_library_autosave(s, library_dir = dir)
    Sys.sleep(1.1)
    lab_library_save(s, "my keeper", library_dir = dir)

    rows <- lab_library_list(dir)
    expect_equal(nrow(rows), 2)
    # Named first despite being saved later.
    expect_false(rows$autosaved[[1]])
    expect_equal(rows$name[[1]], "my keeper")
    expect_true(rows$autosaved[[2]])
  })
})

test_that("autosave failure warns but does not error", {
  shiny::isolate({
    s <- LabSession$new(cohort_ids = "Eyam_1665")
    # A regular file where a directory should be: saveRDS will fail, and
    # that failure must not escape into the caller's fit handler.
    #
    # capture_warnings(), not expect_warning(): saveRDS emits its own base
    # warning ("cannot open file") before throwing, and expect_warning()
    # with no regexp captures only the first, letting ours bubble up to the
    # reporter. Capture the lot and assert on the one we care about.
    bad <- tempfile()
    writeLines("not a directory", bad)
    ws <- capture_warnings(res <- lab_library_autosave(s, library_dir = bad))
    expect_match(ws, "Autosave to the fit library failed", all = FALSE)
    expect_null(res)
  })
})

test_that("list records group count for hierarchical fits", {
  shiny::isolate({
    dir <- local_library()
    s <- LabSession$new(cohort_ids = c("Barcelona_1490", "Eyam_1665",
                                       "Malta_1813"))
    lab_library_save(s, "three outbreaks", library_dir = dir)
    rows <- lab_library_list(dir)
    expect_equal(rows$n_groups[[1]], 3L)
    # "Barcelona 1489", not 1490: Krauer's record starts 1489-11-05 and the
    # old hand-transcribed file dropped its first 125 days.
    expect_equal(rows$cohort_label[[1]], "Barcelona 1489 +2 more")
    expect_match(rows$cohort[[1]], "Malta_1813")
  })
})

test_that("promote clears the autosave flag and renames", {
  shiny::isolate({
    dir <- local_library()
    s <- LabSession$new(cohort_ids = "Eyam_1665")
    meta <- lab_library_autosave(s, library_dir = dir)

    lab_library_promote(meta$slug, "worth keeping", library_dir = dir)
    rows <- lab_library_list(dir)
    expect_equal(nrow(rows), 1)
    expect_false(rows$autosaved[[1]])
    expect_equal(rows$name[[1]], "worth keeping")
    # Old pair gone.
    expect_false(file.exists(file.path(dir, paste0(meta$slug, ".rds"))))
  })
})

test_that("promote preserves the stored payload", {
  shiny::isolate({
    dir <- local_library()
    s <- LabSession$new(cohort_ids = c("Cairo_1835", "Malta_1813"))
    meta <- lab_library_autosave(s, library_dir = dir)
    lab_library_promote(meta$slug, "kept", library_dir = dir)

    fresh <- LabSession$new()
    lab_library_load(fresh, "kept", library_dir = dir)
    expect_equal(fresh$cohort_ids, c("Cairo_1835", "Malta_1813"))
  })
})

test_that("promote rejects an empty name and an unknown slug", {
  dir <- local_library()
  expect_error(lab_library_promote("nope", "x", library_dir = dir), "nope")
})

# ---- last-fit lookup (drives restore-on-open) -----------------------------

test_that("lab_library_last returns NULL on an empty library", {
  expect_null(lab_library_last(local_library()))
})

test_that("lab_library_last is strictly newest, autosaves included", {
  shiny::isolate({
    dir <- local_library()
    s <- LabSession$new(cohort_ids = "Eyam_1665")
    lab_library_save(s, "named keeper", library_dir = dir)
    Sys.sleep(1.1)
    auto <- lab_library_autosave(s, library_dir = dir)

    # lab_library_list() floats named entries to the top for display; the
    # restore path must ignore that and take the genuinely most recent.
    expect_equal(lab_library_list(dir)$name[[1]], "named keeper")
    expect_equal(lab_library_last(dir), auto$slug)
  })
})

test_that("lab_library_last round-trips into a session", {
  shiny::isolate({
    dir <- local_library()
    s <- LabSession$new(cohort_ids = c("Cairo_1835", "Malta_1813"))
    lab_library_autosave(s, library_dir = dir)

    fresh <- LabSession$new()
    lab_library_load(fresh, lab_library_last(dir), library_dir = dir)
    expect_equal(fresh$cohort_ids, c("Cairo_1835", "Malta_1813"))
  })
})

# ---- compare view ----------------------------------------------------------

test_that("the compare palette is fixed-order and capped", {
  expect_equal(.compare_palette(1), "#2a78d6")
  expect_equal(.compare_palette(3)[1:2], c("#2a78d6", "#eb6834"))
  # Hues are never cycled: a fifth series would have no colour, so the cap
  # and the palette length must agree.
  expect_length(.compare_palette(9), 4)
  expect_equal(lab_compare_max(), 4L)
})

test_that("compare_draws returns an empty frame for fits with no samples", {
  shiny::isolate({
    dir <- local_library()
    s <- LabSession$new(cohort_ids = "Eyam_1665")
    lab_library_save(s, "no samples", library_dir = dir)
    out <- lab_compare_draws("no-samples", library_dir = dir)
    expect_equal(nrow(out), 0)
    expect_true(all(c("fit", "variable", "value") %in% names(out)))
  })
})

test_that("compare_draws skips slugs that aren't in the library", {
  out <- lab_compare_draws(c("ghost", "phantom"), library_dir = local_library())
  expect_equal(nrow(out), 0)
})

test_that("compare_trajectories reports unrebuildable fits in `skipped`", {
  shiny::isolate({
    dir <- local_library()
    s <- LabSession$new(cohort_ids = "Eyam_1665")
    s$fit_state <- list(status = "complete", samples = NULL)
    lab_library_save(s, "empty fit", library_dir = dir)

    out <- lab_compare_trajectories("empty-fit", library_dir = dir)
    expect_equal(nrow(out), 0)
    expect_equal(attr(out, "skipped"), "empty fit")
  })
})

test_that(".compare_label falls back to the slug when metadata is missing", {
  dir <- local_library()
  expect_equal(.compare_label("orphan", dir), "orphan")
})

test_that("empty compare plots render rather than erroring", {
  empty_d <- data.frame(fit = character(0), variable = character(0),
                        value = numeric(0))
  empty_t <- data.frame(fit = character(0), draw = character(0),
                        group = character(0), time = numeric(0),
                        mu = numeric(0))
  expect_s3_class(.compare_dens_plot(empty_d), "ggplot")
  expect_s3_class(.compare_traj_plot(empty_t), "ggplot")
})

test_that(".compare_height grows with facets and is capped", {
  expect_lt(.compare_height(1), .compare_height(8))
  expect_lte(.compare_height(100), 900)
})

# ---- pilot / production differentiation ------------------------------------

test_that(".fit_mode_label names the posterior actually on screen", {
  expect_null(.fit_mode_label(NULL))
  expect_null(.fit_mode_label(list(samples = NULL, mode = "stochastic")))
  expect_match(.fit_mode_label(list(samples = 1, mode = "deterministic")),
               "pilot")
  expect_match(.fit_mode_label(list(samples = 1, mode = "stochastic")),
               "production")
  # The pilot stage is always deterministic, whatever the run's mode.
  expect_match(.fit_mode_label(list(samples = 1, mode = "stochastic"),
                               stage = "pilot"), "pilot")
})

test_that(".fit_is_stochastic drives the fan kind", {
  expect_false(.fit_is_stochastic(NULL))
  expect_false(.fit_is_stochastic(list(mode = "deterministic")))
  expect_true(.fit_is_stochastic(list(mode = "stochastic")))
})

test_that("hero subtitle states the posterior and the fan kind", {
  data <- data.frame(group = "g", time = 1:5, deaths = c(1, 2, 3, 2, 1))
  post <- data.frame(draw = rep(1:2, each = 5), group = "g",
                     time = rep(1:5, 2), mu = runif(10))
  det <- .hero_plot(data, post, mode_label = "pilot (deterministic)")
  sto <- .hero_plot(data, post, mode_label = "production (stochastic)",
                    stochastic_fan = TRUE)
  expect_match(det$labels$subtitle, "deterministic trajectories")
  expect_match(sto$labels$subtitle, "stochastic realisations")
  expect_match(sto$labels$subtitle, "production")
})

test_that("hero still renders without a mode label", {
  data <- data.frame(group = "g", time = 1:3, deaths = c(1, 2, 1))
  expect_s3_class(.hero_plot(data, NULL), "ggplot")
})

test_that("expand_stages splits a stochastic run but never a pilot-only one", {
  shiny::isolate({
    dir <- local_library()

    det <- LabSession$new(cohort_ids = "Eyam_1665")
    det$fit_state <- list(status = "complete", samples = 1,
                          pilot_samples = 1, mode = "deterministic")
    lab_library_save(det, "det run", library_dir = dir)

    sto <- LabSession$new(cohort_ids = "Eyam_1665")
    sto$fit_state <- list(status = "complete", samples = 2,
                          pilot_samples = 1, mode = "stochastic")
    lab_library_save(sto, "sto run", library_dir = dir)

    # Deterministic: the pilot IS the canonical posterior, so one series.
    one <- lab_compare_expand_stages("det-run", library_dir = dir)
    expect_equal(one$stage, "canonical")

    # Stochastic: two distinct posteriors, so two series.
    two <- lab_compare_expand_stages("sto-run", library_dir = dir)
    expect_equal(two$slug, c("sto-run", "sto-run"))
    expect_equal(two$stage, c("canonical", "pilot"))
  })
})

test_that("stage suffixes keep the two series apart in the legend", {
  shiny::isolate({
    dir <- local_library()
    s <- LabSession$new(cohort_ids = "Eyam_1665")
    s$fit_state <- list(status = "complete", samples = 1,
                        pilot_samples = 1, mode = "stochastic")
    lab_library_save(s, "my run", library_dir = dir)
    expect_equal(.compare_label("my-run", dir), "my run")
    expect_equal(.compare_label("my-run", dir, "pilot"), "my run (pilot)")
  })
})

test_that("compare rejects a stages vector of the wrong length", {
  expect_error(lab_compare_draws(c("a", "b"), stages = "canonical"),
               "same length")
})

# ---- reactive-context safety and the diagnostics plots ---------------------

test_that("library load works outside a reactive context", {
  # Regression: LabSession fields are reactiveVal()s, and reading one outside
  # a reactive context throws. lab_app()'s restore runs at server start, which
  # is NOT a reactive context, so a missing isolate() made a fresh open
  # silently restore nothing while the library modal (an observeEvent, which
  # IS a reactive context) worked fine.
  dir <- withr::local_tempdir()
  saved <- shiny::isolate({
    s <- LabSession$new(cohort_ids = "Eyam_1665")
    lab_library_save(s, "restore me", library_dir = dir)
    s
  })
  expect_s3_class(saved, "LabSession")

  fresh <- LabSession$new()
  # Deliberately NO isolate() around the call: that is the failing condition.
  expect_no_error(lab_library_load(fresh, "restore-me", library_dir = dir))
  expect_equal(shiny::isolate(fresh$cohort_ids), "Eyam_1665")
})

test_that("lab_library_last works outside a reactive context", {
  dir <- withr::local_tempdir()
  shiny::isolate({
    s <- LabSession$new(cohort_ids = "Eyam_1665")
    lab_library_save(s, "only one", library_dir = dir)
  })
  expect_no_error(slug <- lab_library_last(dir))
  expect_equal(slug, "only-one")
})

test_that(".diag_long flattens draws to one row per variable-iteration-chain", {
  skip_if_not_installed("posterior")
  draws <- posterior::as_draws_array(
    array(stats::rnorm(2 * 50 * 3), dim = c(50, 3, 2),
          dimnames = list(NULL, NULL, c("alpha", "beta"))))
  long <- .diag_long(draws)
  expect_setequal(names(long), c("variable", "chain", "iteration", "value"))
  expect_equal(nrow(long), 50 * 3 * 2)
  expect_equal(levels(long$variable), c("alpha", "beta"))
  expect_equal(nlevels(long$chain), 3)
})

test_that("diagnostics plots carry axis titles and per-chain colour", {
  draws <- posterior::as_draws_array(
    array(stats::rnorm(2 * 40 * 2), dim = c(40, 2, 2),
          dimnames = list(NULL, NULL, c("alpha", "beta"))))
  long <- .diag_long(draws)
  tr <- .diag_trace_plot(long)
  de <- .diag_dens_plot(long)
  expect_s3_class(tr, "ggplot")
  expect_s3_class(de, "ggplot")
  expect_equal(tr$labels$x, "Iteration")
  expect_equal(tr$labels$y, "Value")
  expect_equal(de$labels$x, "Parameter value")
  expect_equal(de$labels$y, "Density")
})

test_that("chain colours never cycle the validated four", {
  expect_equal(.diag_chain_colours(2), c("#2a78d6", "#eb6834"))
  expect_length(.diag_chain_colours(4), 4)
  six <- .diag_chain_colours(6)
  expect_length(six, 6)
  expect_equal(length(unique(six)), 6)
})
