# ------------------------------------------------------------------------------
# outbreak_ids.R — resolving outbreak identifiers across the 2026-09 rekey.
#
# Before 2026-09-22 the app keyed outbreaks on hand-assigned strings
# ("Barcelona_1490", "London_1563"). They are now keyed on Krauer's integer
# id as a character string, because `place_startyear` is not unique — Krauer
# 5 and 6 are both "Alexandria_1840", 7 and 8 both "Alexandria_1842" — and a
# cohort subset built with `%in%` would have spliced each pair into one group.
#
# Saved sessions and fit-library entries on disk still carry the old strings,
# so every entry point that accepts user-supplied ids runs them through
# `outbreak_resolve_id()` first.
# ------------------------------------------------------------------------------

#' Resolve outbreak identifiers, translating pre-2026-09 ids.
#'
#' Current ids pass through untouched. Legacy string ids (the nine the app
#' used before outbreaks were rekeyed on Krauer's catalogue id) are translated
#' via [outbreak_aliases]. Ids matching neither are returned unchanged, so the
#' caller's own "no data for this outbreak" error is what the user sees rather
#' than one from here.
#'
#' Note that `"Barcelona_1490"` resolves to the outbreak now labelled
#' *Barcelona 1489*. That is not an off-by-one: Krauer's record begins
#' 1489-11-05, and the old hand-transcribed file dropped its first 125 days,
#' which is what made it look like a 1490 outbreak.
#'
#' @param ids Character vector of outbreak identifiers, current or legacy.
#' @param data Long-format outbreaks tibble used to decide what counts as a
#'   current id. Defaults to the bundled [outbreaks_all].
#' @return Character vector the same length as `ids`.
#' @examples
#' outbreak_resolve_id(c("London_1563", "31"))
#' @export
outbreak_resolve_id <- function(ids, data = NULL) {
  if (length(ids) == 0) return(character(0))
  ids <- as.character(ids)
  if (is.null(data)) data <- get("outbreaks_all", envir = asNamespace("yersinia"))
  aliases <- get("outbreak_aliases", envir = asNamespace("yersinia"))
  map <- stats::setNames(aliases$outbreak_id, aliases$legacy_id)
  # Only translate what isn't already a valid current id, so a current id that
  # happened to collide with a legacy spelling would still win.
  known <- ids %in% data$outbreak_id
  hit <- !known & ids %in% names(map)
  ids[hit] <- unname(map[ids[hit]])
  ids
}

#' Display labels for outbreak identifiers.
#'
#' @param ids Character vector of outbreak identifiers, current or legacy.
#' @param data Long-format outbreaks tibble. Defaults to [outbreaks_all].
#' @return Character vector of labels; unresolvable ids are returned as-is.
#' @examples
#' outbreak_label(c("18", "69"))
#' @export
outbreak_label <- function(ids, data = NULL) {
  if (length(ids) == 0) return(character(0))
  if (is.null(data)) data <- get("outbreaks_all", envir = asNamespace("yersinia"))
  resolved <- outbreak_resolve_id(ids, data)
  lookup <- data[!duplicated(data$outbreak_id), c("outbreak_id", "label")]
  out <- lookup$label[match(resolved, lookup$outbreak_id)]
  ifelse(is.na(out), ids, out)
}
