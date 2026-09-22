# ------------------------------------------------------------------------------
# app_compare.R — overlay several saved fits.
#
# A "variant" of a fit (a seasonality arm, a different scenario, a rerun with
# more iterations) is just another library entry. Rather than nesting variants
# under an outbreak in the library schema, comparison is a *view*: pick 2-4
# entries and see their posterior trajectory fans and marginal densities on
# shared axes, coloured by entry. That composes for any pair of fits — three
# forcing arms of one outbreak, or the same arm across two outbreaks — where a
# nested schema would only serve the first case.
#
# Densities need only `samples` and always work. Trajectories additionally need
# the dust2 setup, which is not serialised (see app_library.R) and is rebuilt
# per entry here; entries whose rebuild fails are reported and skipped rather
# than sinking the whole view.
# ------------------------------------------------------------------------------

# Categorical palette, fixed order, never cycled. Blue / orange / aqua / yellow,
# validated for colour-vision deficiency separation against a light surface
# (worst adjacent pair dE 9.1 protan, 22.9 normal). Slots are assigned by
# position in the selection and stay with an entry for the life of the view, so
# removing one entry never repaints the others.
.compare_palette <- function(n) {
  c("#2a78d6", "#eb6834", "#1baf7a", "#eda100")[seq_len(min(n, 4L))]
}

#' Maximum number of fits comparable at once.
#'
#' Four. The categorical palette has four validated slots and hues are never
#' cycled, so a fifth series would have no distinguishable colour.
#' @return Integer.
#' @export
lab_compare_max <- function() 4L

# Read one entry's payload and rebuild a session from it. Returns NULL when the
# file is gone or unreadable.
.compare_session <- function(slug, library_dir) {
  path <- file.path(library_dir %||% lab_library_dir(create = FALSE),
                    paste0(slug, ".rds"))
  if (!file.exists(path)) return(NULL)
  state <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(state)) return(NULL)
  lab_session_from_list(state)
}

# Display label for an entry: its saved name, falling back to the slug.
# A stage suffix distinguishes the two posteriors inside one stochastic entry.
.compare_label <- function(slug, library_dir, stage = "canonical") {
  meta_path <- file.path(library_dir %||% lab_library_dir(create = FALSE),
                         paste0(slug, ".meta.rds"))
  # Check first: readRDS() emits a connection *warning* before throwing, and
  # tryCatch(error=) lets that warning through to the caller.
  m <- if (file.exists(meta_path)) {
    tryCatch(readRDS(meta_path), error = function(e) NULL)
  } else NULL
  nm <- m$name %||% slug
  if (identical(stage, "pilot")) paste0(nm, " (pilot)") else nm
}

# The samples for one stage of an entry. "canonical" is what the app displays
# everywhere else — production draws after a stochastic run, pilot otherwise.
# "pilot" reaches the pilot draws that a stochastic run keeps but no other
# view exposes.
.compare_samples <- function(session, stage = "canonical") {
  if (is.null(session)) return(NULL)
  # isolate(): LabSession fields are reactiveVal()s and reading one outside a
  # reactive context errors. These helpers are exported, so they are also
  # called straight from the console.
  st <- shiny::isolate(session$fit_state)
  if (identical(stage, "pilot")) st$pilot_samples else st$samples
}

# Normalise a selection into parallel slug/stage vectors.
.compare_spec <- function(slugs, stages = NULL) {
  if (is.null(stages)) stages <- rep("canonical", length(slugs))
  if (length(stages) != length(slugs)) {
    cli::cli_abort("{.arg stages} must be the same length as {.arg slugs}.")
  }
  list(slug = slugs, stage = stages)
}

#' Expand a selection so stochastic entries contribute both stages.
#'
#' A stochastic run stores `pilot_samples` alongside the production draws;
#' nothing else in the app can show them. This turns each such entry into two
#' comparable series so a run can be checked against its own pilot.
#'
#' Entries with no separate pilot (a deterministic fit, where the pilot *is*
#' the canonical posterior) contribute one series, never a duplicate.
#'
#' @param slugs Character vector of library slugs.
#' @param library_dir Library directory. Defaults to [lab_library_dir()].
#' @return List with `slug` and `stage`, suitable for [lab_compare_draws()].
#' @export
lab_compare_expand_stages <- function(slugs, library_dir = NULL) {
  out_slug <- character(0); out_stage <- character(0)
  for (slug in slugs) {
    s <- .compare_session(slug, library_dir)
    st <- if (is.null(s)) NULL else shiny::isolate(s$fit_state)
    out_slug <- c(out_slug, slug); out_stage <- c(out_stage, "canonical")
    has_distinct_pilot <- !is.null(st$pilot_samples) &&
      identical(st$mode, "stochastic")
    if (has_distinct_pilot) {
      out_slug <- c(out_slug, slug); out_stage <- c(out_stage, "pilot")
    }
  }
  list(slug = out_slug, stage = out_stage)
}

#' Marginal posterior draws for several saved fits.
#'
#' @param slugs Character vector of library slugs (see [lab_library_list()]).
#' @param library_dir Library directory. Defaults to [lab_library_dir()].
#' @param stages Optional parallel vector of `"canonical"` / `"pilot"`,
#'   selecting which posterior to take from each entry. Defaults to all
#'   canonical. See [lab_compare_expand_stages()].
#' @return Long data frame with `fit`, `variable`, `value`. Zero rows when
#'   nothing could be read.
#' @export
lab_compare_draws <- function(slugs, library_dir = NULL, stages = NULL) {
  spec <- .compare_spec(slugs, stages)
  rows <- lapply(seq_along(spec$slug), function(i) {
    slug <- spec$slug[[i]]; stage <- spec$stage[[i]]
    s <- .compare_session(slug, library_dir)
    samples <- .compare_samples(s, stage)
    if (is.null(samples)) return(NULL)
    d <- tryCatch(posterior::as_draws_df(samples), error = function(e) NULL)
    if (is.null(d)) return(NULL)
    vars <- posterior::variables(d)
    df <- as.data.frame(d)[, vars, drop = FALSE]
    out <- utils::stack(df)
    data.frame(fit = .compare_label(slug, library_dir, stage),
               variable = as.character(out$ind),
               value = out$values,
               stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame(fit = character(0), variable = character(0),
                      value = numeric(0), stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, rows)
  out$fit <- factor(out$fit, levels = unique(out$fit))
  out
}

#' Posterior predictive trajectories for several saved fits.
#'
#' Rebuilds each entry's dust2 setup (not serialised) and forward-simulates.
#' Entries whose setup cannot be rebuilt are skipped; their labels come back in
#' the `skipped` attribute so the caller can say which and why.
#'
#' @param slugs Character vector of library slugs.
#' @param library_dir Library directory. Defaults to [lab_library_dir()].
#' @param n_draws Posterior draws per fit. Default 40 — lower than the hero's,
#'   since several fans share one panel.
#' @param stages Optional parallel vector of `"canonical"` / `"pilot"`.
#' @return Long data frame with `fit`, `draw`, `group`, `time`, `mu`, carrying
#'   a `skipped` attribute (character vector of labels).
#' @export
lab_compare_trajectories <- function(slugs, library_dir = NULL, n_draws = 40L,
                                     stages = NULL) {
  spec <- .compare_spec(slugs, stages)
  skipped <- character(0)
  rows <- lapply(seq_along(spec$slug), function(i) {
    slug <- spec$slug[[i]]; stage <- spec$stage[[i]]
    label <- .compare_label(slug, library_dir, stage)
    s <- .compare_session(slug, library_dir)
    samples <- .compare_samples(s, stage)
    if (is.null(s) || is.null(samples)) {
      skipped <<- c(skipped, label)
      return(NULL)
    }
    st <- shiny::isolate(s$fit_state)
    setup <- tryCatch(lab_fit_assemble(s), error = function(e) NULL)
    if (is.null(setup)) {
      skipped <<- c(skipped, label)
      return(NULL)
    }
    # Match the hero: a production posterior gets a stochastic fan, a pilot
    # a deterministic one. Comparing a pilot against its production run then
    # shows both differences at once — the parameters and the noise.
    stochastic <- identical(stage, "canonical") && .fit_is_stochastic(st)
    tr <- tryCatch(
      lab_fit_forward_sim(setup, samples, n_draws = n_draws,
                          deterministic = !stochastic),
      error = function(e) NULL)
    if (is.null(tr)) {
      skipped <<- c(skipped, label)
      return(NULL)
    }
    tr$fit <- label
    # Draw ids restart per fit; make them unique so ggplot's grouping can't
    # join a line across two fits.
    tr$draw <- paste(label, tr$draw, sep = "#")
    as.data.frame(tr)
  })
  rows <- Filter(Negate(is.null), rows)
  out <- if (length(rows) == 0) {
    data.frame(fit = character(0), draw = character(0), group = character(0),
               time = numeric(0), mu = numeric(0), stringsAsFactors = FALSE)
  } else {
    o <- do.call(rbind, rows)
    o$fit <- factor(o$fit, levels = unique(o$fit))
    o
  }
  attr(out, "skipped") <- skipped
  out
}

# Overlaid trajectory fans, one colour per fit, over the observed deaths.
.compare_traj_plot <- function(traj, data = NULL) {
  if (nrow(traj) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, size = 5, colour = "grey50",
                          label = "No trajectories could be rebuilt.") +
        ggplot2::theme_void()
    )
  }
  fits <- levels(traj$fit)
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = traj,
      mapping = ggplot2::aes(x = .data$time, y = .data$mu,
                             group = .data$draw, colour = .data$fit),
      alpha = 0.12, linewidth = 0.35, na.rm = TRUE
    )
  if (!is.null(data) && nrow(data) > 0) {
    p <- p + ggplot2::geom_point(
      data = data[!is.na(data$deaths), , drop = FALSE],
      mapping = ggplot2::aes(x = .data$time, y = .data$deaths),
      size = 1.1, colour = "#222", na.rm = TRUE)
  }
  p +
    ggplot2::scale_colour_manual(values = .compare_palette(length(fits)),
                                 breaks = fits, drop = FALSE) +
    # Faint fan lines make a faint legend key; override so identity is legible.
    ggplot2::guides(colour = ggplot2::guide_legend(
      override.aes = list(alpha = 1, linewidth = 1.6))) +
    ggplot2::labs(x = "Day", y = "Deaths", colour = NULL) +
    ggplot2::facet_wrap(~ group, scales = "free") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "top",
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
}

# Overlaid marginal densities, faceted by parameter, one colour per fit.
# Free x scales: parameters span wildly different magnitudes (K_r in the
# thousands next to g_h in [0, 1]), so a shared axis would flatten most panels.
.compare_dens_plot <- function(draws) {
  if (nrow(draws) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, size = 5, colour = "grey50",
                          label = "No posterior draws to compare.") +
        ggplot2::theme_void()
    )
  }
  fits <- levels(draws$fit)
  ggplot2::ggplot(draws, ggplot2::aes(x = .data$value, colour = .data$fit,
                                      fill = .data$fit)) +
    ggplot2::geom_density(alpha = 0.18, linewidth = 0.7, na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = .compare_palette(length(fits)),
                                 breaks = fits, drop = FALSE) +
    ggplot2::scale_fill_manual(values = .compare_palette(length(fits)),
                               breaks = fits, drop = FALSE) +
    ggplot2::facet_wrap(~ variable, scales = "free",
                        labeller = .param_labeller()) +
    ggplot2::labs(x = "Parameter value", y = "Density",
                  colour = NULL, fill = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "top",
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
}

# Height for the compare panels, scaled by facet count (same idea as the
# diagnostics strip: enough room for strip labels and tick values).
.compare_height <- function(n_facets, per_row = 3, row_px = 190) {
  rows <- max(1L, ceiling(n_facets / per_row))
  min(rows * row_px, 900L)
}
