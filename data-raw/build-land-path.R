# ------------------------------------------------------------------------------
# data-raw/build-land-path.R -- rebuild inst/extdata/europe-land-110m.path
#
# The explorer's map needs a coastline that works offline, so rather than
# calling a tile server at runtime the package ships one pre-projected SVG
# path. This script builds it. It is not run at install or load time; run it
# by hand if the projection box changes or a finer coastline is wanted.
#
# SOURCE: Natural Earth 110m land polygons, via the `world-atlas` TopoJSON
# build (public domain). Fetched over the network, so this script -- unlike
# everything else in data-raw/ -- needs internet.
#
# The output is clipped to the catalogue's own extent plus a margin and
# simplified to the rings that survive an area threshold: 10 rings, ~6.6 kB.
# Keep it small. It is inlined into every render of the map.
# ------------------------------------------------------------------------------

URL <- "https://cdn.jsdelivr.net/npm/world-atlas@2/land-110m.json"
OUT <- "inst/extdata/europe-land-110m.path"

# Must match `.explore_geo` in R/app_explore_views.R. If you change one,
# change the other -- the points and the coastline are projected separately
# and only agree because these numbers do.
LON0 <- -12; LON1 <- 50; LAT0 <- 28; LAT1 <- 62
W <- 1000; H <- 776                 # H = W * (LAT1-LAT0) / ((LON1-LON0) * cos(45))
MIN_AREA <- 0.35                    # square degrees; drops islands too small to see

topo <- jsonlite::fromJSON(URL, simplifyVector = FALSE)
sc <- topo$transform$scale; tr <- topo$transform$translate

# TopoJSON arcs are quantised and delta-encoded: cumulative-sum, then affine.
decode_arc <- function(a) {
  m <- do.call(rbind, lapply(a, unlist))
  cbind(cumsum(m[, 1]) * sc[[1]] + tr[[1]],
        cumsum(m[, 2]) * sc[[2]] + tr[[2]])
}
arcs <- lapply(topo$arcs, decode_arc)

# A ring is a list of arc indices; a negative index means "that arc, reversed"
# and is encoded as the ones' complement, so -1 is arc 0 backwards.
build_ring <- function(idx) {
  parts <- lapply(idx, function(i) {
    if (i < 0) arcs[[-i]][nrow(arcs[[-i]]):1, , drop = FALSE] else arcs[[i + 1]]
  })
  out <- parts[[1]]
  for (k in seq_along(parts)[-1]) {
    out <- rbind(out, parts[[k]][-1, , drop = FALSE])
  }
  out
}

# Sutherland-Hodgman against each edge of the lon/lat box, in geographic
# space (before projection, so the clip edges stay straight afterwards).
clip_edge <- function(poly, keep, at, axis) {
  if (nrow(poly) == 0) return(poly)
  n <- nrow(poly); out <- list()
  for (i in seq_len(n)) {
    a <- poly[if (i == 1L) n else i - 1L, ]; b <- poly[i, ]
    ia <- keep(a); ib <- keep(b)
    cross <- function(a, b) {
      t <- (at - a[axis]) / (b[axis] - a[axis])
      p <- a + t * (b - a); p[axis] <- at; p
    }
    if (ib) {
      if (!ia) out[[length(out) + 1L]] <- cross(a, b)
      out[[length(out) + 1L]] <- b
    } else if (ia) {
      out[[length(out) + 1L]] <- cross(a, b)
    }
  }
  if (length(out) == 0) matrix(numeric(0), 0, 2) else do.call(rbind, out)
}
clip_box <- function(poly) {
  poly <- clip_edge(poly, function(p) p[1] >= LON0, LON0, 1)
  poly <- clip_edge(poly, function(p) p[1] <= LON1, LON1, 1)
  poly <- clip_edge(poly, function(p) p[2] >= LAT0, LAT0, 2)
  clip_edge(poly, function(p) p[2] <= LAT1, LAT1, 2)
}

shoelace <- function(p) {
  n <- nrow(p); j <- c(n, seq_len(n - 1))
  abs(sum(p[, 1] * p[j, 2] - p[j, 1] * p[, 2])) / 2
}

geoms <- topo$objects$land$geometries
rings <- unlist(lapply(geoms, function(g) {
  if (identical(g$type, "Polygon")) list(g$arcs)
  else g$arcs                       # MultiPolygon: a list of polygons
}), recursive = FALSE)

paths <- character(0)
for (poly in rings) {
  for (r in poly) {
    p <- clip_box(build_ring(unlist(r)))
    if (nrow(p) < 3 || shoelace(p) < MIN_AREA) next
    x <- (p[, 1] - LON0) / (LON1 - LON0) * W
    y <- (LAT1 - p[, 2]) / (LAT1 - LAT0) * H
    paths <- c(paths, paste0(
      "M", paste(sprintf("%.1f,%.1f", x, y), collapse = " "), "Z"))
  }
}

dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)
writeLines(paste(paths, collapse = ""), OUT)
message(length(paths), " rings, ", file.size(OUT), " bytes -> ", OUT)
