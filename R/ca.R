# Cellular automaton tools for K&G-style plague metapopulation analysis.
#
# These functions support coarse-graining the full stochastic metapop model
# into a 3-state (S / E / P) cellular automaton whose transition probabilities
# are estimated empirically from `run_plague_metapop_model()` runs. The
# vignette `metapop-cellular-automaton.qmd` walks through the full pipeline
# end to end; the helpers here let users compose the same CA without
# copy-pasting code from the vignette.

#' Build a square-lattice CA grid
#'
#' Constructs a `n_row × n_col` regular grid with **rook (4-neighbour)**
#' adjacency, returning a list with the neighbour list, sparse adjacency
#' matrix, and per-cell `(row, col)` coordinates. Boundary cells have 2-3
#' neighbours; interior cells have 4. The neighbour ordering is row-major
#' (cell index = `(row - 1) * n_col + col`).
#'
#' @param n_row,n_col Integer dimensions of the lattice.
#' @return A list with elements:
#'   * `neighbors` — list of integer vectors, one per cell
#'   * `A` — sparse `Matrix::sparseMatrix` adjacency, dims `(npop, npop)`
#'   * `n_row`, `n_col`, `npop`
#'   * `cell_xy` — data frame with `row` and `col` columns
#' @seealso [make_hex_lattice()] for the hex variant; [run_ca()] consumes
#'   the returned list directly.
#' @export
make_square_grid <- function(n_row, n_col) {
  checkmate::assert_integerish(n_row, lower = 1, len = 1)
  checkmate::assert_integerish(n_col, lower = 1, len = 1)
  npop <- n_row * n_col
  nb <- vector("list", npop)
  for (r in 1:n_row) for (c in 1:n_col) {
    i <- (r - 1) * n_col + c
    nbi <- integer(0)
    if (r > 1)     nbi <- c(nbi, (r - 2) * n_col + c)
    if (r < n_row) nbi <- c(nbi, r * n_col + c)
    if (c > 1)     nbi <- c(nbi, (r - 1) * n_col + c - 1)
    if (c < n_col) nbi <- c(nbi, (r - 1) * n_col + c + 1)
    nb[[i]] <- nbi
  }
  rows <- rep(seq_len(npop), lengths(nb))
  cols <- unlist(nb)
  A <- Matrix::sparseMatrix(i = rows, j = cols, x = 1,
                            dims = c(npop, npop))
  list(neighbors = nb, A = A,
       n_row = as.integer(n_row), n_col = as.integer(n_col), npop = npop,
       cell_xy = expand.grid(col = 1:n_col, row = 1:n_row)[, c("row", "col")])
}

#' Build a hexagonal CA lattice
#'
#' Constructs a hex-tessellated grid covering a `side × side` bounding box
#' (one hex cell per unit area, pointy-top by default), with **6-neighbour**
#' adjacency. Requires the `sf` package. Returns the same `lattice` interface
#' as [make_square_grid()] plus an `sf` field that lets you visualize results
#' with `ggplot2::geom_sf()`.
#'
#' @param side Numeric. Side length of the square bounding box. The number
#'   of cells produced is approximately `side^2`.
#' @return A list with `neighbors`, `A`, `npop`, and `sf` (an `sf` data frame
#'   with `cell_id` and `geometry` columns).
#' @seealso [make_square_grid()] for the square variant.
#' @export
make_hex_lattice <- function(side) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    cli::cli_abort("`make_hex_lattice()` requires the `sf` package. \\
                    Install it via `install.packages('sf')`.")
  }
  checkmate::assert_number(side, lower = 1)
  bbox <- sf::st_polygon(list(rbind(c(0, 0), c(side, 0),
                                    c(side, side), c(0, side), c(0, 0))))
  bbox <- sf::st_sfc(bbox)
  hex_geom <- sf::st_make_grid(bbox, cellsize = 1, square = FALSE)
  hex_sf   <- sf::st_sf(cell_id = seq_along(hex_geom),
                        geometry = hex_geom)
  nb <- sf::st_touches(hex_sf)
  rows <- rep(seq_along(nb), lengths(nb))
  cols <- unlist(nb)
  A <- Matrix::sparseMatrix(i = rows, j = cols, x = 1,
                            dims = c(nrow(hex_sf), nrow(hex_sf)))
  list(neighbors = nb, A = A, npop = nrow(hex_sf), sf = hex_sf)
}

#' Build CA transition-probability lookups
#'
#' From a tidy data frame of empirical `T_i` / `Q_i` estimates (as produced
#' by the `metapop-cellular-automaton` vignette), build linear interpolation
#' functions for `T_E(S_frac)`, `T_P(S_frac)`, and `Q(S_frac)` at a chosen
#' coupling level `mu_r`. The returned lookup list is the form
#' [ca_step()] expects.
#'
#' Pads `T_i` only with `(S_frac = 0, T_i = 0)` (biologically required —
#' fully-resistant cells cannot trigger) and uses `approxfun(rule = 2)` to
#' extrapolate `T_i` past the empirical S_frac range as the boundary value.
#' For `Q_i` no synthetic boundary points are added, since the empirical
#' Q curve often drops at high S_frac (fully-susceptible patches burn out
#' fast) and a `Q(1) = 1` pad would inflate the lookup exactly where the
#' data says it should fall. Rare-trigger NAs in `Q_i` (n_trig < 5) are
#' dropped before interpolation.
#'
#' @param TQ Data frame with columns `source` (`"E"` or `"P"`), `mu_r`,
#'   `S_frac`, `T_i`, `Q_i`. The vignette produces this as `tq` or `ca_TQ`.
#' @param mu_r_use Coupling level at which to extract lookups (must match a
#'   value in `TQ$mu_r`).
#' @return A list of `function(S_frac)` interpolators: `T_E`, `T_P`, `Q`.
#' @export
make_lookups <- function(TQ, mu_r_use) {
  checkmate::assert_data_frame(TQ, min.rows = 1)
  required <- c("source", "mu_r", "S_frac", "T_i", "Q_i")
  checkmate::assert_names(names(TQ), must.include = required)
  if (!any(TQ$mu_r == mu_r_use)) {
    cli::cli_abort("`mu_r_use = {mu_r_use}` not found in TQ$mu_r \\
                    (available: {.val {sort(unique(TQ$mu_r))}}).")
  }

  build_T_data <- function(src) {
    sub <- TQ[TQ$source == src & TQ$mu_r == mu_r_use,
              c("S_frac", "T_i"), drop = FALSE]
    # Pad T_i with 0 at S_frac = 0 (biologically required: no susceptibles
    # in target → no triggering possible). Don't pad at S_frac = 1; let
    # rule = 2 extrapolation use the highest data point as the boundary.
    sub <- rbind(data.frame(S_frac = 0, T_i = 0), sub)
    sub[order(sub$S_frac), ]
  }
  E_T <- build_T_data("E")
  P_T <- build_T_data("P")

  # Q lookup: use the E-source Q estimates without boundary padding.
  # rule = 2 in approxfun handles extrapolation by holding the boundary y
  # value constant — at S_frac = 1 we want the empirically-observed value
  # (often < 1 due to fast burnout of fully-susceptible patches), not 1.
  Q_data <- TQ[TQ$source == "E" & TQ$mu_r == mu_r_use & !is.na(TQ$Q_i),
               c("S_frac", "Q_i"), drop = FALSE]
  Q_data <- Q_data[order(Q_data$S_frac), ]

  Q_fn <- if (nrow(Q_data) >= 2) {
    stats::approxfun(Q_data$S_frac, Q_data$Q_i, rule = 2)
  } else if (nrow(Q_data) == 1) {
    constant <- Q_data$Q_i[1]
    function(s) rep(constant, length(s))
  } else {
    function(s) rep(0.5, length(s))   # last resort
  }

  list(
    T_E = stats::approxfun(E_T$S_frac, E_T$T_i, rule = 2),
    T_P = stats::approxfun(P_T$S_frac, P_T$T_i, rule = 2),
    Q   = Q_fn
  )
}

#' Advance the CA one tick
#'
#' Vectorized one-step update of a 3-state CA (`0` = susceptible, `1` =
#' epidemic, `2` = endemic) following K&G (2000) Table 3 semantics:
#'
#' * `S → E` or `S → P` if any neighbour is in `E` or `P`, with per-target
#'   probability `1 - (1 - T_E)^n_E × (1 - T_P)^n_P` and outcome `P` with
#'   conditional probability `Q` (else `E`).
#' * `E → S_0` (deterministic — epidemics resolve in one tick).
#' * `P → S_w` with probability `d_per_tick`, sus reset to `P_recovery_sus`.
#' * Susceptibility regrows in `S` cells by `grow_per_tick` (capped at 100).
#'
#' All transitions are computed from the start-of-tick state; no
#' within-tick asynchronous updates.
#'
#' @param state Integer vector of length `npop`, values `0`/`1`/`2`.
#' @param sus Numeric vector of length `npop`, values in `[0, 100]`.
#' @param A Sparse adjacency matrix from `make_*_grid()`.
#' @param lookups List of three functions (`T_E`, `T_P`, `Q`) of
#'   `S_frac ∈ [0, 1]`. Use [make_lookups()].
#' @param d_per_tick Probability per tick that an isolated `P` cell decays
#'   back to `S`. Estimate from your metapop using a long-run `P`-state
#'   survival simulation.
#' @param P_recovery_sus Susceptibility (0-100) that recovered `P` cells
#'   land at. Default `20` is just below the band where transmission becomes
#'   reliable in the vignette's parameter regime.
#' @param grow_per_tick Increment in `sus` per tick for `S` cells.
#' @return A list with the updated `state` and `sus` vectors.
#' @export
ca_step <- function(state, sus, A, lookups, d_per_tick,
                    P_recovery_sus = 20, grow_per_tick = 12) {
  npop <- length(state)
  new_state <- state
  new_sus   <- sus

  n_E <- as.vector(A %*% (state == 1L))
  n_P <- as.vector(A %*% (state == 2L))

  S_frac_now <- sus / 100
  T_E_v <- lookups$T_E(S_frac_now)
  T_P_v <- lookups$T_P(S_frac_now)
  Q_v   <- lookups$Q(S_frac_now)

  p_inf <- 1 - (1 - T_E_v)^n_E * (1 - T_P_v)^n_P
  eligible <- (state == 0L) & (n_E + n_P > 0L)
  rolls    <- stats::runif(npop)
  infected <- eligible & (rolls < p_inf)
  outcome  <- stats::runif(npop) < Q_v
  go_to_P  <- infected &  outcome
  go_to_E  <- infected & !outcome
  new_state[go_to_P] <- 2L
  new_state[go_to_E] <- 1L
  new_sus[infected]  <- 0

  new_state[state == 1L] <- 0L

  P_idx    <- which(state == 2L)
  recovers <- P_idx[stats::runif(length(P_idx)) < d_per_tick]
  new_state[recovers] <- 0L
  new_sus[recovers]   <- P_recovery_sus

  grow_idx <- which(new_state == 0L & new_sus < 100)
  new_sus[grow_idx] <- pmin(new_sus[grow_idx] + grow_per_tick, 100)

  list(state = new_state, sus = new_sus)
}

#' Run a CA from `n_seeds` seed cells for `n_ticks`
#'
#' Initializes a fresh lattice (all `S` with full susceptibility), seeds
#' `n_seeds` cells in state `E`, and advances [ca_step()] for `n_ticks`.
#' Returns the per-tick state matrix; the `sus` history is not retained
#' (regenerable from `state` and the recovery rules if needed).
#'
#' @param lattice Lattice list from [make_square_grid()] or
#'   [make_hex_lattice()].
#' @param lookups Lookup list from [make_lookups()].
#' @param d_per_tick `P → S` decay probability per tick.
#' @param n_ticks Integer number of ticks to advance.
#' @param n_seeds Integer number of cells to seed in state `E` at tick 0.
#' @param seed Optional integer for `set.seed()` to make the run reproducible.
#' @param ... Additional arguments forwarded to [ca_step()] (e.g.
#'   `P_recovery_sus`, `grow_per_tick`).
#' @return Integer matrix of shape `(n_ticks + 1, npop)` with row `t + 1`
#'   the state at tick `t`.
#' @export
run_ca <- function(lattice, lookups, d_per_tick, n_ticks,
                   n_seeds = 5, seed = NULL, ...) {
  checkmate::assert_list(lattice)
  checkmate::assert_names(names(lattice), must.include = c("npop", "A"))
  checkmate::assert_integerish(n_ticks, lower = 1, len = 1)
  checkmate::assert_integerish(n_seeds, lower = 1, upper = lattice$npop, len = 1)

  if (!is.null(seed)) set.seed(seed)
  state <- rep(0L,  lattice$npop)
  sus   <- rep(100, lattice$npop)
  seeds <- sample.int(lattice$npop, n_seeds)
  state[seeds] <- 1L
  sus[seeds]   <- 0

  states_history <- matrix(0L, nrow = n_ticks + 1, ncol = lattice$npop)
  states_history[1, ] <- state

  for (t in seq_len(n_ticks)) {
    out   <- ca_step(state, sus, lattice$A, lookups, d_per_tick, ...)
    state <- out$state
    sus   <- out$sus
    states_history[t + 1, ] <- state
  }
  states_history
}
