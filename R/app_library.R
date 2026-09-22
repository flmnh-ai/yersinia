# ------------------------------------------------------------------------------
# app_library.R — on-disk library of saved fits for the Virtual Lab.
#
# The app already knows how to snapshot itself: LabSession$to_list() returns a
# plain list and apply_list() restores it. The library is that same payload
# written to a known directory instead of round-tripping through the browser's
# download/upload, plus a small sidecar of metadata so the picker can list
# dozens of fits without deserialising megabytes of posterior draws.
#
# Layout, one pair of files per saved fit:
#
#   <library_dir>/<slug>.rds        full LabSession$to_list() payload
#   <library_dir>/<slug>.meta.rds   small named list: name, saved_at, cohort,
#                                   scenario, fitted, mode, chains, iters,
#                                   duration, max_rhat
#
# IMPORTANT: `fit_state$setup` is dropped before writing. It holds
# `setup$unfilter`, a dust2 external pointer; saveRDS() writes those as null
# pointers, so a restored setup is a dead handle and "Run production" fails on
# it. The setup is fully determined by cohort_ids + model_config + priors, so
# lab_library_load() rebuilds it with lab_fit_assemble() instead. Lossless, and
# it keeps the files a good deal smaller.
# ------------------------------------------------------------------------------

#' Directory backing the saved-fit library.
#'
#' Defaults to a per-user data directory so saved fits survive across R
#' sessions and package reinstalls. Override with the `yersinia.library_dir`
#' option or by passing `library_dir` to [lab_app()].
#'
#' @param create Create the directory if it doesn't exist. Default `TRUE`.
#' @return Path to the library directory.
#' @export
lab_library_dir <- function(create = TRUE) {
  dir <- getOption("yersinia.library_dir",
                   file.path(tools::R_user_dir("yersinia", "data"), "fits"))
  if (create && !dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  dir
}

# Filesystem-safe stem for a user-supplied name. Keeps it readable so the
# directory stays browsable by hand.
.lab_slug <- function(name) {
  s <- tolower(trimws(name))
  s <- gsub("[^a-z0-9]+", "-", s)
  s <- gsub("(^-+)|(-+$)", "", s)
  if (!nzchar(s)) s <- "fit"
  substr(s, 1, 60)
}

# Human-readable cohort label. Hierarchical fits can carry a dozen outbreaks,
# which makes a useless filename and an unreadable table cell, so name the
# first and count the rest.
.lab_cohort_label <- function(ids) {
  if (length(ids) == 0) return("no-cohort")
  # Display labels, not raw ids. Since the 2026-09 rekey an outbreak_id is
  # Krauer's integer ("41"), so naming entries by the id would fill the fit
  # library and the compare view with numbers. outbreak_label() resolves
  # legacy ids on the way through as well, so entries saved before the rekey
  # keep reading the same.
  labs <- tryCatch(outbreak_label(ids), error = function(e) as.character(ids))
  if (length(labs) == 1) return(labs[[1]])
  sprintf("%s +%d more", labs[[1]], length(labs) - 1L)
}

# Generated name for an autosaved run. Includes the timestamp so every run
# is its own entry — autosaves version, they never overwrite each other.
.lab_autosave_name <- function(state, when = Sys.time()) {
  cfg <- state$model_config %||% list()
  st  <- state$fit_state %||% list()
  parts <- c(
    .lab_cohort_label(state$cohort_ids %||% character(0)),
    cfg$scenario %||% NA_character_,
    if (identical(st$mode, "stochastic")) "stochastic" else NULL,
    format(when, "%Y%m%d-%H%M%S")
  )
  paste(parts[!is.na(parts)], collapse = " ")
}

# Largest R-hat across parameters, or NA when there's nothing to summarise.
# Wrapped because posterior::rhat() throws on degenerate draws (single
# iteration, single chain) and a metadata field is never worth an error.
.lab_max_rhat <- function(samples) {
  if (is.null(samples)) return(NA_real_)
  tryCatch({
    draws <- posterior::as_draws_array(samples)
    vars <- posterior::variables(draws)
    if (length(vars) == 0) return(NA_real_)
    rh <- vapply(vars, function(v) {
      posterior::rhat(posterior::extract_variable_matrix(draws, v))
    }, numeric(1))
    if (all(is.na(rh))) NA_real_ else max(rh, na.rm = TRUE)
  }, error = function(e) NA_real_)
}

# Build the sidecar metadata for a session snapshot.
.lab_meta <- function(state, name, slug, autosaved = FALSE) {
  st  <- state$fit_state %||% list()
  cfg <- state$model_config %||% list()
  list(
    name      = name,
    slug      = slug,
    saved_at  = Sys.time(),
    autosaved = isTRUE(autosaved),
    cohort    = state$cohort_ids %||% character(0),
    cohort_label = .lab_cohort_label(state$cohort_ids %||% character(0)),
    n_groups  = length(state$cohort_ids %||% character(0)),
    scenario = cfg$scenario %||% NA_character_,
    fitted   = sort(unique(c(cfg$shared, cfg$local))),
    mode     = st$mode %||% NA_character_,
    status   = st$status %||% "idle",
    n_chains = st$n_chains %||% NA_integer_,
    n_iter   = st$n_iter %||% NA_integer_,
    duration = st$duration %||% NA_real_,
    max_rhat = .lab_max_rhat(st$samples)
  )
}

#' Save the current session to the fit library.
#'
#' Writes `lab_session$to_list()` plus a metadata sidecar.
#'
#' Two collision policies, because manual and automatic saves want opposite
#' things. A *manual* save overwrites any existing entry with the same slug,
#' so iterating on one named fit updates it in place instead of accumulating
#' near-duplicates. An *autosave* (`autosaved = TRUE`) never overwrites: its
#' generated name already carries a timestamp, and a numeric suffix is added
#' in the unlikely event two runs land in the same second. Every run is
#' therefore recoverable.
#'
#' @param lab_session A [LabSession] instance.
#' @param name Human-readable name for this fit. Used to derive the filename.
#' @param library_dir Directory to write to. Defaults to [lab_library_dir()].
#' @param autosaved Mark this entry as automatically saved. Autosaves are
#'   flagged in the metadata so the app can filter them out of the picker,
#'   and are never overwritten.
#' @return Invisibly, the metadata list that was written.
#' @export
lab_library_save <- function(lab_session, name, library_dir = NULL,
                             autosaved = FALSE) {
  if (!nzchar(trimws(name %||% ""))) {
    cli::cli_abort("Give the fit a name.")
  }
  dir <- library_dir %||% lab_library_dir()
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  state <- lab_session$to_list()
  # Drop the dust2 external pointers — see the file header.
  if (!is.null(state$fit_state)) state$fit_state$setup <- NULL

  slug <- .lab_slug(name)
  if (isTRUE(autosaved)) {
    # Never clobber a previous autosave: disambiguate within the second.
    base <- slug
    i <- 2L
    while (file.exists(file.path(dir, paste0(slug, ".rds")))) {
      slug <- sprintf("%s-%d", base, i)
      i <- i + 1L
    }
  }
  meta <- .lab_meta(state, name, slug, autosaved = autosaved)
  saveRDS(state, file.path(dir, paste0(slug, ".rds")))
  saveRDS(meta,  file.path(dir, paste0(slug, ".meta.rds")))
  invisible(meta)
}

#' Autosave a completed fit.
#'
#' Called by the app after every successful fit. Generates a name from the
#' cohort, scenario, mode and timestamp, and writes a new versioned entry —
#' autosaves never replace one another, so re-running the same configuration
#' leaves both runs in the library to compare.
#'
#' Failures are swallowed deliberately: a full disk or an unwritable library
#' directory should never turn a successful fit into an error the user sees.
#' The condition message is returned invisibly for logging.
#'
#' @param lab_session A [LabSession] instance with a completed fit.
#' @param library_dir Directory to write to. Defaults to [lab_library_dir()].
#' @return Invisibly, the metadata list, or `NULL` if the save failed.
#' @export
lab_library_autosave <- function(lab_session, library_dir = NULL) {
  tryCatch({
    state <- lab_session$to_list()
    nm <- .lab_autosave_name(state)
    lab_library_save(lab_session, nm, library_dir = library_dir,
                     autosaved = TRUE)
  }, error = function(e) {
    warning("Autosave to the fit library failed: ", conditionMessage(e),
            call. = FALSE)
    invisible(NULL)
  })
}

#' List saved fits in the library.
#'
#' Reads only the `.meta.rds` sidecars, so listing stays fast no matter how
#' large the stored posteriors get. Entries whose sidecar is unreadable are
#' skipped rather than aborting the listing.
#'
#' @param library_dir Directory to scan. Defaults to [lab_library_dir()].
#' @return A data frame, one row per saved fit, newest first. Zero rows (with
#'   the full set of columns) when the library is empty.
#' @export
lab_library_list <- function(library_dir = NULL) {
  dir <- library_dir %||% lab_library_dir(create = FALSE)
  empty <- data.frame(
    name = character(0), slug = character(0),
    saved_at = as.POSIXct(character(0)), autosaved = logical(0),
    cohort = character(0), cohort_label = character(0),
    n_groups = integer(0), scenario = character(0), fitted = character(0),
    mode = character(0), n_chains = integer(0), n_iter = integer(0),
    duration = numeric(0), max_rhat = numeric(0), stringsAsFactors = FALSE
  )
  if (!dir.exists(dir)) return(empty)
  files <- list.files(dir, pattern = "\\.meta\\.rds$", full.names = TRUE)
  if (length(files) == 0) return(empty)

  rows <- lapply(files, function(f) {
    m <- tryCatch(readRDS(f), error = function(e) NULL)
    if (is.null(m)) return(NULL)
    data.frame(
      name     = m$name %||% NA_character_,
      slug     = m$slug %||% sub("\\.meta\\.rds$", "", basename(f)),
      saved_at = m$saved_at %||% as.POSIXct(NA),
      autosaved = isTRUE(m$autosaved),
      cohort   = paste(m$cohort, collapse = ", "),
      cohort_label = m$cohort_label %||% .lab_cohort_label(m$cohort),
      n_groups = as.integer(m$n_groups %||% length(m$cohort)),
      scenario = m$scenario %||% NA_character_,
      fitted   = paste(m$fitted, collapse = ", "),
      mode     = m$mode %||% NA_character_,
      n_chains = as.integer(m$n_chains %||% NA),
      n_iter   = as.integer(m$n_iter %||% NA),
      duration = as.numeric(m$duration %||% NA),
      max_rhat = as.numeric(m$max_rhat %||% NA),
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(empty)
  out <- do.call(rbind, rows)
  # Named keepers sort above autosaves; newest first within each group.
  out[order(out$autosaved, -as.numeric(out$saved_at)), , drop = FALSE]
}

#' Slug of the most recently saved fit.
#'
#' Strictly most-recent by timestamp, autosaves included — unlike
#' [lab_library_list()]'s display order, which floats named entries to the
#' top. Used to restore the last fit when the app opens.
#'
#' @param library_dir Directory to scan. Defaults to [lab_library_dir()].
#' @return A slug, or `NULL` when the library is empty.
#' @export
lab_library_last <- function(library_dir = NULL) {
  rows <- lab_library_list(library_dir)
  if (nrow(rows) == 0) return(NULL)
  rows$slug[[which.max(as.numeric(rows$saved_at))]]
}

#' Load a saved fit into a live session.
#'
#' Restores cohort, model config, priors, and the stored fit, then rebuilds
#' the dust2 setup that was deliberately not serialised. If the rebuild fails
#' (e.g. the bundled outbreak data changed under the saved cohort), the rest
#' of the state is still applied and `setup` is left `NULL` — the fit is
#' viewable, it just can't be promoted to production without a refit.
#'
#' @param lab_session A [LabSession] to mutate in place.
#' @param slug Slug of the saved fit (from [lab_library_list()]).
#' @param library_dir Directory to read from. Defaults to [lab_library_dir()].
#' @return Invisibly, `lab_session`.
#' @export
lab_library_load <- function(lab_session, slug, library_dir = NULL) {
  dir <- library_dir %||% lab_library_dir(create = FALSE)
  path <- file.path(dir, paste0(slug, ".rds"))
  if (!file.exists(path)) {
    cli::cli_abort("No saved fit named {.val {slug}} in the library.")
  }
  state <- readRDS(path)
  lab_session$apply_list(state)

  # isolate(): LabSession's fields are reactiveVal()s, and reading one outside
  # a reactive context is an error. Callers include app startup, which is not
  # a reactive context — see the restore block in lab_app().
  shiny::isolate({
    st <- lab_session$fit_state
    if (!is.null(st) && !is.null(st$samples) && is.null(st$setup)) {
      st$setup <- tryCatch(lab_fit_assemble(lab_session),
                           error = function(e) NULL)
      lab_session$fit_state <- st
    }
  })
  invisible(lab_session)
}

#' Promote an autosaved entry to a named keeper.
#'
#' Clears the autosave flag and renames the entry, so a run worth keeping
#' stops looking like scratch and sorts with your named fits. The payload is
#' re-read and re-written under the new slug; the old pair is removed.
#'
#' @param slug Slug of the autosaved entry.
#' @param name New human-readable name.
#' @param library_dir Directory to operate in. Defaults to [lab_library_dir()].
#' @return Invisibly, the new metadata list.
#' @export
lab_library_promote <- function(slug, name, library_dir = NULL) {
  dir <- library_dir %||% lab_library_dir(create = FALSE)
  path <- file.path(dir, paste0(slug, ".rds"))
  if (!file.exists(path)) {
    cli::cli_abort("No saved fit named {.val {slug}} in the library.")
  }
  if (!nzchar(trimws(name %||% ""))) cli::cli_abort("Give the fit a name.")
  state <- readRDS(path)
  new_slug <- .lab_slug(name)
  meta <- .lab_meta(state, name, new_slug, autosaved = FALSE)
  saveRDS(state, file.path(dir, paste0(new_slug, ".rds")))
  saveRDS(meta,  file.path(dir, paste0(new_slug, ".meta.rds")))
  if (!identical(new_slug, slug)) lab_library_delete(slug, library_dir = dir)
  invisible(meta)
}

#' Delete a saved fit from the library.
#'
#' @param slug Slug of the saved fit.
#' @param library_dir Directory to delete from. Defaults to [lab_library_dir()].
#' @return Invisibly, `TRUE` if anything was removed.
#' @export
lab_library_delete <- function(slug, library_dir = NULL) {
  dir <- library_dir %||% lab_library_dir(create = FALSE)
  paths <- file.path(dir, paste0(slug, c(".rds", ".meta.rds")))
  hit <- file.exists(paths)
  if (any(hit)) file.remove(paths[hit])
  invisible(any(hit))
}
