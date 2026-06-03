# ------------------------------------------------------------------------------
# build.R — generates `inst/explainer/parameters.html`.
#
# Embeds inline SVG sparklines per prior (computed from `prior_density()`),
# pulls scenario values from the bundled YAMLs, and uses a structured
# parameter-metadata list as the single source of truth.
#
# Usage:
#   setwd("path/to/yersinia")
#   devtools::load_all()
#   source("inst/explainer/build.R")
# ------------------------------------------------------------------------------

stopifnot(requireNamespace("yersinia", quietly = TRUE) || exists("prior_density"))

# ---- helpers ----------------------------------------------------------------

# Inline SVG sparkline for a prior density. Returns "" if not plottable.
sparkline_svg <- function(prior, width = 220, height = 36) {
  if (is.null(prior)) return("")
  d <- tryCatch(yersinia::prior_density(prior, n = 120),
                error = function(e) NULL)
  if (is.null(d) || nrow(d) < 3) return("")
  x_min <- min(d$x); x_max <- max(d$x)
  if (!is.finite(x_min) || !is.finite(x_max) || x_max == x_min) return("")
  y_max <- max(d$density, na.rm = TRUE)
  if (!is.finite(y_max) || y_max <= 0) return("")
  x_norm <- (d$x - x_min) / (x_max - x_min) * (width - 4) + 2
  y_norm <- height - 4 - (d$density / y_max) * (height - 8)
  pts <- paste(sprintf("%.2f,%.2f", x_norm, y_norm), collapse = " ")
  fill_pts <- paste(
    c(sprintf("2,%.2f", height - 4),
      pts,
      sprintf("%.2f,%.2f", width - 2, height - 4)),
    collapse = " "
  )
  sprintf(paste0(
    '<div class="sparkline-wrap">',
    '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" ',
    'viewBox="0 0 %d %d" class="sparkline">',
    '<polygon points="%s" fill="rgba(31,78,121,0.16)" stroke="none"/>',
    '<polyline points="%s" fill="none" stroke="#1f4e79" stroke-width="1.4" ',
    'stroke-linejoin="round"/>',
    '</svg>',
    '</div>'
  ), width, height, width, height, fill_pts, pts)
}

# Pretty-print a prior list as "Family(arg1 = v1, arg2 = v2)".
prior_label <- function(prior) {
  if (is.null(prior)) return("&mdash;")
  fam <- yersinia::prior_families()[[prior$family]]
  ordered <- prior$params[names(fam$params)]
  body <- paste(names(ordered), sprintf("%g", unlist(ordered)),
                sep = " = ", collapse = ", ")
  sprintf("%s(%s)", prior$family, body)
}

# Format a scenario value cell. "—" for unset.
fmt_val <- function(v) {
  if (is.null(v) || length(v) == 0 || is.na(v)) return('<td class="na">&mdash;</td>')
  if (abs(v) >= 1000 || (abs(v) < 0.01 && v != 0)) {
    sprintf("<td>%s</td>", formatC(v, format = "g", digits = 4))
  } else {
    sprintf("<td>%s</td>", format(signif(v, 4), trim = TRUE))
  }
}

# Tag pill markup.
tag_pill <- function(kind) {
  switch(kind,
    shared = '<span class="tag tag-shared">Shared</span>',
    local  = '<span class="tag tag-local">Local</span>',
    fixed  = '<span class="tag tag-fixed">Fixed</span>',
    rare   = '<span class="tag tag-rare">Rarely fit</span>',
    sprintf('<span class="tag">%s</span>', kind)
  )
}

# Build a scenario-values <table> for a parameter (or NULL to skip).
scenario_table <- function(p_name, scenarios) {
  vals <- vapply(scenarios, function(s) {
    v <- s[[p_name]]
    if (is.null(v)) NA_real_ else as.numeric(v[1])
  }, numeric(1))
  cells <- paste(vapply(vals, fmt_val, character(1)), collapse = "")
  header <- paste(c("<th>scenario</th>",
                    sprintf("<th>%s</th>", names(scenarios))),
                  collapse = "")
  sprintf(paste0(
    '<table class="scen"><thead><tr>%s</tr></thead>',
    '<tbody><tr><td>value</td>%s</tr></tbody></table>'
  ), header, cells)
}

# ---- parameter metadata (the single source of truth) -----------------------

P <- function(symbol, name, units, group, role, description,
              prior = NULL, judgement = NULL, show_scenario = TRUE) {
  list(symbol = symbol, name = name, units = units, group = group, role = role,
       description = description, prior = prior, judgement = judgement,
       show_scenario = show_scenario)
}

PI <- list(

  # ---- Transmission --------------------------------------------------------
  P("beta_r", "carcass-to-rat transmission rate", "per day",
    "transmission", "shared",
    paste0(
      "Mass-action rate at which susceptible rats become infected from ",
      "contact with infectious carcasses (Q). Combined with <code>rho</code> ",
      "via <code>lambda_r = beta_r &middot; Q &middot; (1 &minus; exp(&minus;rho &middot; T_r / K_r)) / T_r</code>. ",
      "Drives R<sub>0</sub> on the rat side: ",
      "<code>R<sub>0</sub> &asymp; beta_r &middot; (1 &minus; g_r) &middot; (1 &minus; exp(&minus;rho)) / delta_R</code>."),
    prior = list(family = "Uniform", params = list(min = 0.1, max = 3.0)),
    judgement = paste0(
      "Plague R<sub>0</sub> in the historical literature clusters in the 1.5&ndash;3 ",
      "range (Eyam 1.6&ndash;2.2, Cairo 1801 ~2.85, Black-Death cities ",
      "2&ndash;3). The wider <code>Uniform(0.1, 3.0)</code> here lets the chain ",
      "explore weak-transmission regimes that fail to take off ",
      "(diagnostic of mis-specification) and high-transmission regimes ",
      "for rare super-spreading scenarios. Biologically, anything ",
      "above <code>1.5</code> implies very rapid rat-to-rat propagation; ",
      "this is plausible in densely packed urban warehouse settings but ",
      "not in scattered village rat populations. A more principled prior ",
      "is to sample <code>R<sub>0</sub> &sim; Gamma(3, 0.6)</code> ",
      "and convert to beta_r via <code>with_R0_to_beta_r()</code>, which is ",
      "what the Barcelona hierarchical vignette does.")),

  P("beta_h", "carcass-to-human transmission rate", "per day",
    "transmission", "local",
    paste0(
      "Force-of-infection coefficient from carcasses (Q) to susceptible ",
      "humans. <code>lambda_h = beta_h &middot; Q &middot; exp(&minus;rho &middot; T_r / K_r) / K_r</code>. ",
      "The <code>exp(&minus;rho &middot; T_r / K_r)</code> term is the fraction of fleas ",
      "that have given up looking for rats and start biting humans ",
      "&mdash; strongest when rat populations crash mid-outbreak."),
    prior = list(family = "Uniform", params = list(min = 0.001, max = 0.5)),
    judgement = paste0(
      "Almost certainly varies more than the rat-side biology does ",
      "&mdash; it folds in housing density, sanitation, plague awareness, ",
      "and human flea-bite tolerance. Didelot's posterior for Cairo 1801 ",
      "is <code>0.0145</code> (CI <code>[0.005, 0.046]</code>); Black-Death ",
      "estimates push higher (~0.1+) because medieval European cities ",
      "were rat-dense and flea-exposed. The default <code>Uniform(0.001, 0.5)</code> ",
      "matches the seasonal vignette and covers Eyam through Givry-style ",
      "Black-Death outbreaks. If posterior pins against 0.5, that's ",
      "evidence the bubonic-only pathway is insufficient and the ",
      "outbreak needs pneumonic transmission (<code>beta_I</code>) ",
      "in the fitted set. The lower bound near zero is non-negotiable: ",
      "in modern well-ventilated settings, carcass-to-human transmission ",
      "is essentially zero.")),

  P("beta_I", "human-to-human transmission rate", "per day",
    "transmission", "fixed",
    paste0(
      "Direct human-to-human transmission, dominant in pneumonic ",
      "plague (respiratory droplet pathway). <code>lambda_hh = beta_I &middot; I_h / T_h</code>. ",
      "Set to zero in pure-bubonic scenarios."),
    prior = list(family = "Uniform", params = list(min = 0, max = 0.1)),
    judgement = paste0(
      "Most well-recorded plague outbreaks were predominantly bubonic, ",
      "with at most a small pneumonic minority. Didelot's posterior of ",
      "<code>0.0215</code> contributes ~18% of Cairo 1801 cases &mdash; ",
      "consistent with a small pneumonic component. The 1910&ndash;11 ",
      "Manchuria epidemic was the canonical 100% pneumonic outbreak ",
      "with beta_I &gt; 1/day and case-fatality near 100%. For ",
      "your average Bills-of-Mortality outbreak: leave fixed at 0 ",
      "unless you have line-list evidence of household clusters or ",
      "rapid secondary attacks within days of an index case.")),

  P("rho", "carcass infectivity range", "dimensionless",
    "transmission", "shared",
    paste0(
      "Controls how quickly fleas \"give up\" on a dead rat and look ",
      "for a new host. <code>(1 &minus; exp(&minus;rho &middot; T_r / K_r))</code> is the ",
      "fraction of fleas that have found a new rat host; the complement ",
      "<code>exp(&minus;rho &middot; T_r / K_r)</code> is the fraction that bite ",
      "humans. Larger rho &rArr; fleas redistribute fast, less time biting humans. ",
      "<em>Not</em> a spatial \"range\" despite the name."),
    prior = list(family = "Uniform", params = list(min = 0.3, max = 6.0)),
    judgement = paste0(
      "Biologically informed by <em>Xenopsylla cheopis</em> behavior &mdash; ",
      "the canonical plague flea. A starved flea can survive 1&ndash;6 ",
      "weeks off-host (laboratory; Bacot-Martin curves), but in practice ",
      "seeks a new host within days. Translating that to rho is fraught ",
      "because rho also folds in the rat-density-dependent encounter ",
      "rate. K&amp;G's <code>rho = 10</code> reflects their fine-spatial ",
      "coarse-graining (small block = high rat density), not biology &mdash; ",
      "don't transplant it to outbreak fits. For most population-level ",
      "fits, <code>2&ndash;3</code> is the consensus; if forced to pin, ",
      "<code>2.5</code> is the safe choice. The prior is wide because ",
      "rho is weakly identified from death-count data alone &mdash; it ",
      "couples with beta_r and K_r in complicated ways.")),

  # ---- Carcass dynamics ----------------------------------------------------
  P("delta_R", "carcass decay rate", "per day",
    "carcass", "shared",
    paste0(
      "Rate at which infectious rat carcasses become non-infectious ",
      "(fleas die off, body decomposes). <code>1 / delta_R</code> is the ",
      "mean carcass infectious period. Temperature- and humidity-sensitive; ",
      "the <code>seasonal</code> vector multiplies this rate to model ",
      "wet/dry season effects."),
    prior = list(family = "Uniform", params = list(min = 0.05, max = 1.5)),
    judgement = paste0(
      "Empirical lab observations of <em>X. cheopis</em> persistence on ",
      "rat carcasses give 3&ndash;7 day infectious windows in temperate ",
      "climates (so <code>delta_R</code> ~0.14&ndash;0.33) and as short ",
      "as 1&ndash;2 days in hot tropical settings (delta_R 0.5&ndash;1.0). ",
      "Didelot's <code>0.267</code> (3.7-day window) is a reasonable ",
      "mid-temperate baseline. The upper bound of 1.5 in the lab prior ",
      "is permissive &mdash; biologically I'd cap at <code>1.0</code> ",
      "unless you have hot-summer carcass data. K&amp;G's <code>0.0274</code> ",
      "(36-day window) is suspect &mdash; that's longer than most fleas ",
      "survive off-host; it likely absorbs other model deficiencies in ",
      "their formulation.")),

  # ---- Capacity & demography ----------------------------------------------
  P("K_h", "human carrying capacity (population)", "count",
    "capacity_demography", "fixed",
    paste0(
      "Initial and equilibrium human population. The model uses this ",
      "as the denominator for force-of-infection terms. Should match ",
      "the historical population at outbreak onset."),
    prior = list(family = "Uniform", params = list(min = 500, max = 3e5)),
    judgement = paste0(
      "There is no reason to fit this unless you genuinely don't know ",
      "the population. The <code>outbreaks</code> dataset carries a ",
      "<code>population</code> column for every outbreak (drawn from ",
      "historical records / Krauer's curated dataset); the lab pins ",
      "K_h from that by default. Fitting K_h trades off with beta_h ",
      "and I_ini (the same death curve fits a range of (K, beta, seed) ",
      "triples), so the posterior will be wider than it needs to be. ",
      "Exception: medieval pre-census records where the historical ",
      "population is genuinely uncertain &mdash; even then, prefer a ",
      "tight informative prior over the default <code>LogNormal</code>.")),

  P("K_r", "rat carrying capacity", "count",
    "capacity_demography", "fixed",
    paste0(
      "Initial and equilibrium rat population. Often taken as roughly ",
      "equal to <code>K_h</code> (Didelot's convention: <code>K_r = K_h = ",
      "250000</code> for Cairo). Unobservable directly &mdash; calibrated ",
      "from human death dynamics, which is why pinning makes sense."),
    prior = list(family = "Uniform", params = list(min = 500, max = 3e5)),
    judgement = paste0(
      "Pre-modern rat populations in urban settings were probably 1:1 ",
      "with humans (rats follow humans for grain and waste). Modern ",
      "cities have far lower ratios. Setting K_r = K_h is the safe ",
      "convention for historical fits; setting it independently invites ",
      "identifiability issues since K_r interacts with beta_r and rho ",
      "in the rat force-of-infection. Only fit it if you have ",
      "side information about rat densities (rare).")),

  P("r_r", "rat birth rate", "per day",
    "capacity_demography", "fixed",
    paste0(
      "Logistic per-day rat birth rate. Drives rat-population recovery ",
      "during long outbreaks. Field-rat fecundity is high: 5&ndash;6 ",
      "litters/year, 6&ndash;8 pups/litter, female maturity at 2 months. ",
      "Per-day birth rate ~0.01&ndash;0.02 covers the realistic range."),
    judgement = paste0(
      "Didelot pins <code>r_r = 0</code> because Cairo 1801 is a 150-day ",
      "outbreak where rat demography doesn't matter on that timescale. ",
      "For year-spanning Black Death-style fits, this matters &mdash; ",
      "rat populations DO recover after die-off. Pin from scenario; ",
      "don't fit from death data (under-identified).")),

  P("r_h", "human birth rate", "per day",
    "capacity_demography", "fixed",
    paste0(
      "Per-day human birth rate. Essentially zero on the timescale of ",
      "a single outbreak (years to matter; outbreaks are months)."),
    judgement = paste0(
      "Pre-modern crude birth rates are ~30&ndash;40 per 1000 per year ",
      "&mdash; on a per-day basis, 8&ndash;11 &times; 10<sup>-5</sup>. ",
      "All five scenarios sit in this range. Numerically inert for ",
      "outbreak fits; pin and move on.")),

  P("d_r", "rat natural death rate", "per day",
    "capacity_demography", "fixed",
    paste0(
      "Background per-day non-plague rat mortality. Field-rat lifespan ",
      "~1&ndash;2 years gives <code>d_r</code> ~5 &times; 10<sup>-4</sup>."),
    judgement = paste0(
      "Pinned at the scenario value. Set equal to <code>r_r</code> ",
      "when you want a population at carrying capacity equilibrium ",
      "(Didelot does this implicitly with both at 0).")),

  P("d_h", "human natural death rate", "per day",
    "capacity_demography", "fixed",
    paste0(
      "Background all-cause non-plague human mortality. <code>1/d_h</code> ",
      "is roughly life expectancy in days. Pre-modern life expectancy ",
      "~30&ndash;35 years; modern ~75&ndash;80 years."),
    judgement = paste0(
      "Note that this is the <em>background</em> non-plague rate &mdash; ",
      "plague deaths come out of the <code>m_h &middot; (1 &minus; g_h)</code> ",
      "channel. <code>historical</code> bumps <code>d_h</code> to ",
      "<code>2.19 &times; 10<sup>-4</sup></code> reflecting Black-Death-era ",
      "life expectancy ~30 years; modern values are half that. For ",
      "single-outbreak fits (months), the value barely matters; for ",
      "year-spanning fits it does.")),

  P("m_r", "plague resolution rate in rats", "per day; 1/m_r = mean infectious duration",
    "capacity_demography", "fixed",
    paste0(
      "Rate at which infected rats resolve their infection (either ",
      "recover or die). <code>1/m_r</code> is the mean time from infection ",
      "to outcome."),
    judgement = paste0(
      "Lab data on plague-infected <em>Rattus norvegicus</em>: 3&ndash;5 ",
      "days from infection to death. So <code>m_r</code> should be ",
      "<code>0.2&ndash;0.33</code>. Most package scenarios use ",
      "<code>0.04&ndash;0.06</code> (16&ndash;25-day window) &mdash; ",
      "these are model-fit-derived values that absorb other dynamics. ",
      "Didelot's <code>m_r = m_h = 0.0556</code> is a simplifying choice ",
      "the paper makes (assumes same plague timescale in both hosts) ",
      "that isn't biologically defensible &mdash; rats die faster than ",
      "untreated humans. For mechanistic interpretation, fix at ",
      "<code>0.2</code> and let the model do the rest of the work.")),

  P("m_h", "plague resolution rate in humans", "per day; 1/m_h = mean infectious duration",
    "capacity_demography", "fixed",
    paste0(
      "Rate at which infected humans resolve to either recovery or ",
      "death. <code>1/m_h</code> is the mean disease duration."),
    judgement = paste0(
      "Clinical untreated bubonic plague: 10&ndash;18 days from onset ",
      "to outcome (so <code>m_h</code> ~0.055&ndash;0.1). Didelot's ",
      "<code>1/18</code> is at the slow end; <code>historical = 0.08</code> ",
      "(~12.5 days) is more typical. Pneumonic plague is much faster ",
      "(2&ndash;3 days, <code>m_h</code> ~0.33&ndash;0.5) but the package ",
      "doesn't separate routes &mdash; <code>m_h</code> is the bubonic ",
      "default. Modern treatment shortens this dramatically, which is why ",
      "<code>modern-estimates</code> uses <code>0.15</code>.")),

  P("g_r", "rat survival probability", "probability &isin; [0, 1]",
    "capacity_demography", "rare",
    paste0(
      "Probability that an infected rat survives plague. For <em>Rattus ",
      "norvegicus</em>, virtually 0% &mdash; primary plague target species, ",
      "uniformly fatal."),
    prior = list(family = "Uniform", params = list(min = 0, max = 0.3)),
    judgement = paste0(
      "Should be 0 unless you're modeling <em>Rattus rattus</em> with ",
      "partial resistance (some populations show 10&ndash;30% survival). ",
      "Didelot pins at 0 to simplify; the prior allowing up to 0.3 is ",
      "generous. Fitting it is masochistic &mdash; it trades off with ",
      "<code>beta_r</code>, <code>rho</code>, and <code>delta_R</code> ",
      "and won't be identified from human death counts.")),

  P("g_h", "human survival probability", "probability &isin; [0, 1]",
    "capacity_demography", "rare",
    paste0(
      "Probability that an infected human survives plague. ",
      "<code>1 &minus; g_h</code> is the case fatality rate (CFR)."),
    prior = list(family = "Uniform", params = list(min = 0.05, max = 0.5)),
    judgement = paste0(
      "Untreated bubonic CFR is 50&ndash;90% (<code>g_h</code> 0.10&ndash;0.50). ",
      "Pneumonic and septicemic are near-100% fatal (<code>g_h</code> ~0). ",
      "Black-Death-era and Cairo records consistently show 85&ndash;95% ",
      "CFR (<code>historical g_h = 0.05</code>, <code>didelot g_h = 0.10</code>). ",
      "Modern treated cases survive 85%+ (<code>modern g_h = 0.15</code>). ",
      "Weakly identified from death curves alone because beta_h, rho, and ",
      "g_h all shift the death-count shape similarly &mdash; pin from ",
      "clinical literature unless you have explicit case-vs-death data.")),

  P("p", "heritable resistance probability", "probability &isin; [0, 1]",
    "capacity_demography", "fixed",
    paste0(
      "Probability that a rat born to a resistant parent inherits ",
      "resistance. Only active when <code>r_r &gt; 0</code> AND ",
      "<code>R_ini &gt; 0</code>."),
    judgement = paste0(
      "There's no empirical basis for this parameter &mdash; it's a ",
      "modeling abstraction for the K&amp;G genetic-resistance framework. ",
      "Values clustered near 1 (the scenarios all use 0.96&ndash;0.98) ",
      "prevent the resistance trait from washing out in the population. ",
      "Inert in Didelot-style fits and most outbreak work; leave alone.")),

  P("iota", "resistant-rat fecundity multiplier", "dimensionless",
    "capacity_demography", "fixed",
    paste0(
      "Reduces the birth rate of resistant rats vs susceptibles ",
      "(<code>iota = 0.75</code> &rArr; resistants reproduce 25% slower). ",
      "Models the fitness cost of resistance."),
    judgement = paste0(
      "Universally fixed at <code>0.75</code> across all scenarios ",
      "(except Didelot who sets it to <code>1.0</code> since resistance is ",
      "inert in his setup). There's NO empirical basis for the specific ",
      "value &mdash; 25% fitness cost is a plausible-sounding modeling ",
      "choice, not measured. Could be anywhere in <code>0.5&ndash;0.95</code> ",
      "with no data to constrain. Explicitly tagged \"always fixed, never ",
      "inferred\" in the package docs. Treat it as a structural assumption, ",
      "not a parameter.")),

  # ---- Initial conditions --------------------------------------------------
  P("I_ini", "initial infected rats", "absolute count",
    "initial", "local", show_scenario = FALSE,
    description = paste0(
      "Number of rats infected at <code>t = 0</code>. Absolute count, ",
      "not a fraction (changed 2026-05 from <code>S_ini</code> fraction)."),
    prior = list(family = "Uniform", params = list(min = 10, max = 5e4)),
    judgement = paste0(
      "Mathematically equivalent to changing time-of-introduction, so ",
      "the posterior reflects \"how many seeds + how long before ",
      "records start.\" Historical context for picking the right scale: ",
      "Eyam 1665 started from one ship-borne case (~1&ndash;10 rats); ",
      "Cairo 1801 had ongoing endemic transmission (~3000&ndash;7000, ",
      "matching Didelot's posterior fraction 0.005&ndash;0.07 of K_r); ",
      "Black-Death cities arrived with grain-ship cargoes carrying many ",
      "infected rats (10<sup>3</sup>&ndash;10<sup>4</sup>). The wide ",
      "<code>LogNormal(median 400)</code> covers all of these with mass ",
      "concentrated on the small-to-moderate seed counts that most ",
      "outbreaks have. If you're ",
      "winter-starting under strong seasonal suppression, you need huge ",
      "seeds to ignite at all &mdash; the upper bound matters then.")),

  P("R_ini", "initial heritably-resistant rats", "absolute count",
    "initial", "fixed", show_scenario = FALSE,
    description = paste0(
      "Number of rats with the heritable resistance trait at ",
      "<code>t = 0</code>. Defaults to <code>0</code> &mdash; Didelot's ",
      "all-susceptible start. Only meaningful when rat demography is ",
      "active and can propagate the trait."),
    judgement = paste0(
      "Use only when deliberately exploring K&amp;G-style trait propagation ",
      "(Figure 5 replication). For real outbreak fits, leave at 0. There ",
      "is no historical record of pre-existing rat resistance to seed ",
      "a fit from.")),

  P("I_h_ini", "initial infected humans", "absolute count",
    "initial", "local", show_scenario = FALSE,
    description = paste0(
      "Number of humans infected at <code>t = 0</code>. Useful when ",
      "records start mid-outbreak with already-elevated death counts."),
    prior = list(family = "Uniform", params = list(min = 0, max = 5000)),
    judgement = paste0(
      "Default 0 is correct when outbreak records start at first death ",
      "(Eyam, Givry). Krauer's curated dataset includes mid-outbreak ",
      "records (Cairo 1801 id 32, Cairo 1835) where deaths are already ",
      "elevated on day 1 &mdash; fit it then. The Cairo 1835 records ",
      "(Gaetani's military clinic) start with ~10&ndash;50 daily deaths, ",
      "implying I_h_ini in the hundreds to low thousands. The ",
      "<code>Exponential(0.005)</code> default puts mode at 0 (correct for ",
      "records that start at outbreak ignition) while still putting ",
      "enough tail mass on the hundreds-to-thousands range for ",
      "mid-outbreak records.")),

  P("R_h_ini", "initial recovered / immune humans", "absolute count",
    "initial", "rare", show_scenario = FALSE,
    description = paste0(
      "Pre-existing immunity from prior plague exposure. Matters only ",
      "for long outbreaks where herd immunity dynamics are visible in ",
      "the death curve (&gt;20% attack rate)."),
    prior = list(family = "Uniform", params = list(min = 0, max = 1000)),
    judgement = paste0(
      "Genuinely identifiable only when the outbreak reaches a large ",
      "fraction of the population (20%+ attack rate) so the susceptible ",
      "pool depletes visibly. Cairo at 13% attack rate &mdash; ",
      "unidentifiable, pin at 0. Eyam at 25% &mdash; arguably ",
      "identifiable since the village exhausted itself. For most ",
      "Black-Death-era fits, residual immunity from prior waves is ",
      "plausible but won't ever be teased out from death counts.")),

  # ---- Observation model ---------------------------------------------------
  P("kappa", "NegBinomial overdispersion (size)", "dimensionless, &gt; 0",
    "observation", "shared", show_scenario = FALSE,
    description = paste0(
      "The \"size\" parameter of the NegBin observation likelihood. ",
      "Large kappa &rarr; Poisson-like; small kappa &rarr; over-dispersed. ",
      "Mean is set by <code>p_obs &middot; D_h + lambda_baseline &middot; obs_period</code>."),
    prior = list(family = "Gamma", params = list(shape = 2, rate = 0.05)),
    judgement = paste0(
      "Historical mortality records are almost always over-dispersed ",
      "(<code>kappa &lt; 10</code>) because of clustered family deaths, ",
      "scribal variability, and bursty reporting. Cleaner records ",
      "(Cairo 1801 Desgenettes' military diary) tighten kappa toward 20+. ",
      "Chronicle-based records (Florence 1400, medieval European cities) ",
      "stay below 10. The <code>Gamma(2, 0.05)</code> prior puts mass on ",
      "<code>kappa &asymp; 20&ndash;40</code> which is moderate ",
      "overdispersion, and the data can pull it tighter or wider. ",
      "If a fit returns kappa &lt; 5, that's a record-quality warning ",
      "&mdash; the diagnostic chip catches this.")),

  P("p_obs", "observation / ascertainment probability", "probability &isin; [0, 1]",
    "observation", "fixed", show_scenario = FALSE,
    description = paste0(
      "Fraction of true plague deaths that show up in the records. ",
      "Models incomplete reporting and classification errors away ",
      "from plague."),
    prior = list(family = "Uniform", params = list(min = 0, max = 1)),
    judgement = paste0(
      "Didelot's 0.8 is a calibrated convention for clean military ",
      "records and it's a reasonable default. Historical reality: ",
      "Cairo 1801 (Desgenettes, professional army doctor) &asymp; 0.8; ",
      "Florence chronicle data &asymp; 0.4&ndash;0.6 (mass graves, ",
      "delayed reporting); Black-Death cities &asymp; 0.3&ndash;0.6 ",
      "(plague pits, scribal collapse). Partially confounded with ",
      "<code>lambda_baseline</code> (both rescale the simulated series) ",
      "&mdash; pin one, fit the other. Default convention: pin p_obs ",
      "at 0.8, let lambda_baseline carry the misclassification signal.")),

  P("lambda_baseline", "baseline non-plague deaths reported as plague",
    "per day",
    "observation", "local", show_scenario = FALSE,
    description = paste0(
      "Constant per-day count of non-plague deaths attributed to plague ",
      "in the records. Captures (1) misclassification &mdash; other ",
      "diseases attributed to plague during scares &mdash; and (2) ambient ",
      "background mortality lumped into outbreak counts."),
    prior = list(family = "Exponential", params = list(rate = 0.1)),
    judgement = paste0(
      "Empirically diagnostic of record quality. Cairo 1801 (Desgenettes' ",
      "careful military records) posterior &asymp; 1; Cairo 1835 (Gaetani, ",
      "broader civilian clinical dataset) posterior &asymp; 20. Florence ",
      "1400 chronicle data should run higher still. For a fresh outbreak ",
      "without prior pilots: <code>Exponential(0.1)</code> (mean 10) is ",
      "conservative &mdash; concentrated near zero but with mass out to ",
      "tens. If the posterior pins against the prior's mode (near 0), ",
      "your records are clean; if it pushes into the tens, your records ",
      "are murky.")),

  P("obs_period", "observation aggregation window", "days; integer",
    "observation", "fixed", show_scenario = FALSE,
    description = paste0(
      "How many days of model output get summed between data points. ",
      "Most outbreaks have daily records (<code>obs_period = 1</code>); ",
      "London 1563 has weekly Bills of Mortality (<code>obs_period = 7</code>). ",
      "The model accumulates <code>D_h</code> over this window before ",
      "resetting."),
    judgement = paste0(
      "Lives in the <code>outbreaks</code> dataset metadata. Pin from ",
      "the outbreak; the lab errors if you try to mix cadences in one ",
      "fit. Don't be tempted to coarsen daily data to weekly to mix ",
      "with London 1563 &mdash; you'd lose information without ",
      "gaining anything.")),

  P("seasonal", "per-day delta_R multiplier vector", "length-T_max numeric",
    "observation", "fixed", show_scenario = FALSE,
    description = paste0(
      "Per-day multiplier on <code>delta_R</code>. Encodes seasonal ",
      "modulation of carcass decay (and hence carcass-mediated ",
      "transmission). Defaults to <code>rep(1, T_max)</code> (no ",
      "seasonality). Length must match the number of simulation days."),
    judgement = paste0(
      "Not inferred &mdash; it's <em>input data</em>. Build from climate ",
      "records via the <code>with_briere_seasonal()</code> packer wrapper ",
      "(temperature &rarr; Bri&egrave;re function &rarr; multiplier), or ",
      "from a hand-built annual sine via <code>with_alpha_seasonal()</code>. ",
      "Seasonal forcing's <em>strength</em> can be inferred via a fitted ",
      "<code>alpha</code> reparameterization (see the Barcelona mechanistic ",
      "vignette). The lab v1 doesn't expose alpha/Bri&egrave;re; ",
      "<code>seasonal</code> stays at 1.")),

  NULL
)
PI <- Filter(Negate(is.null), PI)

# ---- composition ------------------------------------------------------------

scenarios <- {
  names_sc <- c("defaults", "didelot", "historical", "keeling-gilligan",
                "modern-estimates")
  out <- lapply(names_sc, function(n) yersinia::load_scenario(n))
  names(out) <- names_sc
  out
}

GROUPS <- list(
  list(id = "transmission", title = "Disease transmission",
       subtitle = "how plague moves between hosts"),
  list(id = "carcass", title = "Carcass dynamics",
       subtitle = "how long Q sticks around"),
  list(id = "capacity_demography", title = "Capacity &amp; demography",
       subtitle = "populations, births, deaths, infection durations"),
  list(id = "initial", title = "Initial conditions",
       subtitle = "where the outbreak starts"),
  list(id = "observation", title = "Observation model",
       subtitle = "how counts come out of dynamics")
)

# Render one parameter card.
render_card <- function(p) {
  scen_html <- if (isTRUE(p$show_scenario)) {
    scenario_table(p$symbol, scenarios)
  } else ""
  prior_html <- if (!is.null(p$prior)) {
    sprintf(paste0(
      '<div class="prior">',
      '<div class="prior-row">',
      '<div class="prior-text"><strong>Default prior:</strong> ',
      '<code>%s</code></div>',
      '%s',
      '</div></div>'),
      prior_label(p$prior), sparkline_svg(p$prior))
  } else ""
  judgement_html <- if (!is.null(p$judgement)) {
    sprintf(paste0(
      '<div class="judgement"><div class="judgement-label">My take</div>',
      '<p>%s</p></div>'), p$judgement)
  } else ""
  sprintf(paste0(
    '<div class="pcard">',
    '<div class="ptitle">',
    '<span class="psym">%s</span>',
    '<span class="pname">%s</span>',
    '<span class="punits">%s</span>',
    '</div>',
    '<p>%s</p>',
    '%s',
    '%s',
    '%s',
    '<div class="role-row">%s</div>',
    '</div>'),
    p$symbol, p$name, p$units, p$description, scen_html, prior_html,
    judgement_html, tag_pill(p$role))
}

# Render summary table row.
render_summary_row <- function(p) {
  prior_str <- if (is.null(p$prior)) "&mdash;" else prior_label(p$prior)
  sprintf("<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
          p$symbol, p$name, tag_pill(p$role), prior_str)
}

render_summary_group <- function(group) {
  rows <- vapply(PI[vapply(PI, function(p) p$group == group$id, logical(1))],
                 render_summary_row, character(1))
  paste0(
    sprintf("<tr class='group'><td colspan='4'>%s</td></tr>", group$title),
    paste(rows, collapse = "")
  )
}

render_group <- function(group) {
  cards <- vapply(PI[vapply(PI, function(p) p$group == group$id, logical(1))],
                  render_card, character(1))
  sprintf(paste0(
    '<h2>%s <span class="h2-sub">%s</span></h2>',
    '<div class="group-cards">%s</div>'),
    group$title, group$subtitle, paste(cards, collapse = "\n"))
}

groups_html <- paste(vapply(GROUPS, render_group, character(1)),
                     collapse = "\n")
summary_html <- paste(vapply(GROUPS, render_summary_group, character(1)),
                      collapse = "\n")

# ---- final HTML -------------------------------------------------------------

css <- '
  :root {
    --ink: #1a1d24; --muted: #5a6373; --rule: #e3e6ec; --bg: #fdfdfb;
    --accent: #1f4e79; --accent-soft: #e9eef7;
    --tag-shared: #cfe3f3; --tag-shared-ink: #1f4e79;
    --tag-local: #f9e3c8; --tag-local-ink: #84541b;
    --tag-fixed: #dde2e7; --tag-fixed-ink: #4a5160;
    --tag-rare: #f3d6dc; --tag-rare-ink: #832b3a;
    --judge-bg: #fdf6e3; --judge-rule: #d4b87a; --judge-ink: #5b3f0a;
  }
  * { box-sizing: border-box; }
  html { font-size: 16px; }
  body {
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto,
                 "Helvetica Neue", sans-serif;
    color: var(--ink); background: var(--bg);
    line-height: 1.55; margin: 0; padding: 0;
  }
  .wrap { max-width: 980px; margin: 0 auto; padding: 3rem 1.5rem 6rem; }
  header { border-bottom: 1px solid var(--rule); padding-bottom: 1.25rem;
           margin-bottom: 2rem; }
  h1 { font-size: 1.8rem; margin: 0 0 0.5rem; letter-spacing: -0.01em; }
  header p { color: var(--muted); margin: 0.25rem 0; font-size: 0.95rem; }
  h2 {
    font-size: 1.25rem; margin: 3.5rem 0 1.25rem;
    padding-bottom: 0.4rem; border-bottom: 2px solid var(--accent);
    letter-spacing: -0.005em;
  }
  h2 .h2-sub {
    color: var(--muted); font-weight: 400; font-size: 0.85rem;
    margin-left: 0.5rem;
  }
  h3 { font-size: 1rem; margin: 1.75rem 0 0.4rem; letter-spacing: -0.005em; }
  p { margin: 0.55rem 0; }
  code {
    font-family: ui-monospace, "SF Mono", Menlo, Consolas, monospace;
    background: var(--accent-soft); padding: 1px 5px; border-radius: 4px;
    font-size: 0.9em; color: var(--accent);
  }
  .lead { font-size: 1.02rem; }
  .muted { color: var(--muted); }
  hr.divider { border: none; border-top: 1px solid var(--rule);
               margin: 3rem 0 1.5rem; }

  .pcard {
    background: #fff; border: 1px solid var(--rule); border-radius: 8px;
    padding: 1rem 1.2rem; margin: 1rem 0;
  }
  .pcard > * + * { margin-top: 0.5rem; }
  .pcard .ptitle {
    display: flex; align-items: baseline; gap: 0.65rem; flex-wrap: wrap;
    margin: 0;
  }
  .pcard .psym {
    font-family: ui-monospace, "SF Mono", Menlo, Consolas, monospace;
    font-size: 1.05rem; font-weight: 600; color: var(--accent);
  }
  .pcard .pname { color: var(--ink); font-size: 0.96rem; }
  .pcard .punits {
    color: var(--muted); font-size: 0.8rem; font-style: italic;
    margin-left: auto;
  }
  .pcard p { font-size: 0.93rem; margin: 0; }
  .role-row { margin-top: 0.6rem !important; }
  .tag {
    display: inline-block; padding: 0.1rem 0.55rem; border-radius: 999px;
    font-size: 0.72rem; font-weight: 600; letter-spacing: 0.03em;
    text-transform: uppercase;
  }
  .tag-shared { background: var(--tag-shared); color: var(--tag-shared-ink); }
  .tag-local  { background: var(--tag-local);  color: var(--tag-local-ink);  }
  .tag-fixed  { background: var(--tag-fixed);  color: var(--tag-fixed-ink);  }
  .tag-rare   { background: var(--tag-rare);   color: var(--tag-rare-ink);   }

  table.scen {
    width: 100%; border-collapse: collapse; font-size: 0.83rem;
    font-family: ui-monospace, "SF Mono", Menlo, Consolas, monospace;
    margin: 0;
  }
  table.scen th, table.scen td {
    padding: 0.22rem 0.55rem; text-align: right;
    border-bottom: 1px solid var(--rule);
  }
  table.scen th:first-child, table.scen td:first-child { text-align: left; }
  table.scen th { font-weight: 600; color: var(--muted); border-bottom-width: 2px; }
  table.scen td.na { color: #b7bdc7; }

  .prior {
    padding: 0.55rem 0.75rem; background: var(--accent-soft);
    border-left: 3px solid var(--accent); border-radius: 0 6px 6px 0;
    font-size: 0.9rem;
  }
  .prior strong { color: var(--accent); }
  .prior code { background: white; color: var(--ink); }
  .prior-row {
    display: flex; gap: 0.8rem; align-items: center;
    justify-content: space-between; flex-wrap: wrap;
  }
  .prior-text { flex: 1 1 200px; }

  .sparkline-wrap {
    flex: 0 0 auto; display: flex; flex-direction: column; align-items: flex-end;
    gap: 0.1rem;
  }
  .sparkline { display: block; }
  .sparkline-range {
    font-family: ui-monospace, "SF Mono", Menlo, Consolas, monospace;
    font-size: 0.7rem; color: var(--accent); opacity: 0.75;
  }

  .judgement {
    background: var(--judge-bg); border-left: 3px solid var(--judge-rule);
    border-radius: 0 6px 6px 0; padding: 0.55rem 0.85rem;
    color: var(--judge-ink); font-size: 0.9rem;
  }
  .judgement-label {
    font-size: 0.7rem; font-weight: 700; letter-spacing: 0.05em;
    text-transform: uppercase; color: var(--judge-rule);
    margin-bottom: 0.2rem;
  }
  .judgement p { color: var(--judge-ink); margin: 0; }
  .judgement code { background: #fff; color: var(--judge-ink); }

  .legend {
    background: #fff; border: 1px solid var(--rule); border-radius: 8px;
    padding: 1rem 1.2rem; margin: 1rem 0 2rem; font-size: 0.9rem;
  }
  .legend dl {
    display: grid; grid-template-columns: max-content 1fr;
    gap: 0.4rem 0.9rem; margin: 0;
  }
  .legend dt { white-space: nowrap; }
  .legend dd { margin: 0; color: var(--muted); }

  ul, ol { margin: 0.5rem 0 0.5rem 1.2rem; padding: 0; }
  li { margin: 0.2rem 0; }

  .summary {
    width: 100%; border-collapse: collapse; font-size: 0.85rem;
    margin: 0.5rem 0 1.5rem;
  }
  .summary th, .summary td {
    padding: 0.4rem 0.55rem; text-align: left;
    border-bottom: 1px solid var(--rule);
  }
  .summary th {
    font-weight: 600; color: var(--muted); font-size: 0.78rem;
    text-transform: uppercase; letter-spacing: 0.03em;
  }
  .summary td:first-child {
    font-family: ui-monospace, "SF Mono", Menlo, Consolas, monospace;
    color: var(--accent); white-space: nowrap;
  }
  .summary tbody tr:hover { background: #f6f8fb; }
  .summary .group {
    background: var(--accent-soft); color: var(--accent);
    font-weight: 700; font-size: 0.75rem; text-transform: uppercase;
    letter-spacing: 0.04em;
  }

  @media (max-width: 640px) {
    .wrap { padding: 1.5rem 1rem 4rem; }
    h1 { font-size: 1.45rem; }
    .punits { margin-left: 0; width: 100%; }
  }
'

intro <- '
<header>
  <h1>yersinia &mdash; parameter &amp; prior explainer</h1>
  <p class="lead">A parameter-by-parameter guide for fitting plague models with
  the yersinia package. For each parameter: biological meaning, units, what the
  prebuilt scenarios use, what prior to put on it when you fit it, my own
  judgement based on the historical / clinical literature, and whether it&rsquo;s
  typically fixed, shared, or per-outbreak local.</p>
  <p class="muted">All rates are <strong>per day</strong>. Carcass-based
  transmission formulation (Didelot et al. 2017). States: rats <code>S</code>,
  <code>I</code>, <code>R</code>, <code>Q</code>; humans <code>S_h</code>,
  <code>I_h</code>, <code>R_h</code>, <code>D_h</code>.</p>
</header>

<section class="legend">
  <strong>How to read each parameter card</strong>
  <dl style="margin-top: 0.6rem;">
    <dt><span class="tag tag-shared">Shared</span></dt>
    <dd>Biology that doesn&rsquo;t change between outbreaks. Single value across the cohort.</dd>
    <dt><span class="tag tag-local">Local</span></dt>
    <dd>Outbreak-specific. One value per outbreak in a hierarchical fit.</dd>
    <dt><span class="tag tag-fixed">Fixed</span></dt>
    <dd>Pinned at the scenario or metadata value; rarely fit.</dd>
    <dt><span class="tag tag-rare">Rarely fit</span></dt>
    <dd>Identifiable in theory; mortality-only data won&rsquo;t tell you. Leave fixed unless you have side information.</dd>
  </dl>
</section>
'

quick_ref <- sprintf('
<h2>Quick reference <span class="h2-sub">all configurable parameters, role, default prior</span></h2>

<table class="summary">
  <thead>
    <tr><th>Param</th><th>Meaning</th><th>Role</th><th>Default prior</th></tr>
  </thead>
  <tbody>
%s
  </tbody>
</table>
', summary_html)

# Cohort-vs-outbreak structure (static)
varies_section <- '
<h2>What varies where <span class="h2-sub">cohort &middot; outbreak &middot; within-outbreak structure</span></h2>

<h3>Universally fixed (across all scenarios and outbreaks)</h3>
<ul>
  <li><code>iota = 0.75</code> &mdash; resistance fecundity cost. Documented as
  &ldquo;always fixed, never inferred.&rdquo;</li>
  <li><code>tau = 1</code> &mdash; discretization timestep. Match data cadence
  via <code>obs_period</code>, not tau.</li>
</ul>

<h3>Fixed per scenario, shared across an outbreak cohort</h3>
<p>Biological parameters that don&rsquo;t vary between Eyam-1665 and Cairo-1801
because they&rsquo;re properties of plague itself: <code>m_r</code>,
<code>m_h</code>, <code>g_r</code>, <code>rho</code>, <code>p</code>,
<code>r_r</code>, <code>r_h</code>, <code>d_r</code>, <code>d_h</code>. They
do vary across <em>scenarios</em> (Black Death era vs modern) but not within
a scenario.</p>

<h3>Fit at the cohort level, shared across outbreaks</h3>
<ul>
  <li><code>beta_r</code> &mdash; rat-side transmission biology.</li>
  <li><code>rho</code> &mdash; flea redistribution.</li>
  <li><code>kappa</code> &mdash; overdispersion, when sources are similar quality.</li>
  <li><code>delta_R</code> &mdash; baseline carcass decay (unmodulated case).</li>
</ul>

<h3>Fit per outbreak</h3>
<ul>
  <li><code>beta_h</code> &mdash; varies city-to-city (housing, sanitation).</li>
  <li><code>I_ini</code> &mdash; outbreak ignition seed.</li>
  <li><code>lambda_baseline</code> &mdash; record quality, varies by scribe / era.</li>
  <li><code>I_h_ini</code> &mdash; if records start mid-outbreak.</li>
</ul>

<h3>Pinned per outbreak from metadata (not fit)</h3>
<ul>
  <li><code>K_h</code> &mdash; known historical population.</li>
  <li><code>K_r</code> &mdash; defaults to <code>K_h</code>.</li>
  <li><code>obs_period</code> &mdash; record cadence (daily / weekly).</li>
  <li><code>seasonal</code> &mdash; when constructed from climate data.</li>
</ul>

<h3>Within-outbreak variation</h3>
<p>The only parameter that varies <em>within</em> a single outbreak is
<code>seasonal</code> &mdash; a length-T vector of per-day multipliers on
<code>delta_R</code>. Everything else is constant over the outbreak&rsquo;s
duration. Time-varying transmission rates are not currently supported without
extending the model.</p>
'

scenario_philosophy <- '
<h2>Scenario philosophy <span class="h2-sub">when to pick which</span></h2>

<div class="pcard">
  <h3 style="margin-top:0;">defaults</h3>
  <p>Didelot/K&amp;G blend. Sensible biological values without commitment to a
  specific outbreak. Good for experimentation; not calibrated to any one
  dataset.</p>
  <div class="judgement">
    <div class="judgement-label">My take</div>
    <p>Use for sandbox exploration. Don&rsquo;t fit real outbreak data starting
    from this &mdash; pick the era-appropriate scenario.</p>
  </div>
</div>

<div class="pcard">
  <h3 style="margin-top:0;">didelot</h3>
  <p>Cairo 1801 posterior means from Didelot et al. 2017. Includes
  <code>K_h = K_r = 250000</code> and <code>p_obs = 0.8</code> baked in.
  <code>beta_I = 0.0215</code> non-zero (pneumonic component).
  Rat demography off (<code>r_r = d_r = 0</code>) for short timescale.</p>
  <div class="judgement">
    <div class="judgement-label">My take</div>
    <p>The right starting point for any short urban outbreak (~150 days).
    The fixed K and p_obs are the calibrations the paper depends on; if you
    use a different outbreak with this scenario, update K from the outbreak
    metadata. Don&rsquo;t fit Eyam from here &mdash; rat demography matters
    there.</p>
  </div>
</div>

<div class="pcard">
  <h3 style="margin-top:0;">historical</h3>
  <p>Black-Death-era (~1347&ndash;1700). Higher <code>d_h</code>
  (life expectancy ~30 years), lower <code>g_h</code> (5% survival), higher
  <code>r_r</code>, lower <code>delta_R</code> (cooler climates).</p>
  <div class="judgement">
    <div class="judgement-label">My take</div>
    <p>The right starting point for medieval / early modern European fits
    (Givry 1348, Florence 1400, Eyam 1665). The 95% CFR reflects the
    overwhelming clinical evidence from that era. Watch out for cold-season
    starts &mdash; if seasonality matters, add the seasonal vector or use the
    Bri&egrave;re wrapper from <code>packer_helpers.R</code>.</p>
  </div>
</div>

<div class="pcard">
  <h3 style="margin-top:0;">keeling-gilligan</h3>
  <p>Keeling &amp; Gilligan 2000 spatial model adapted to the carcass
  formulation via the flea quasi-equilibrium. Tiny <code>beta_r</code> (0.0863)
  and high <code>rho</code> (10) because their carrying capacity is per-block.</p>
  <div class="judgement">
    <div class="judgement-label">My take</div>
    <p>This is NOT an outbreak-fitting scenario. It exists to replicate K&amp;G
    figures and to drive the cellular-automaton tools
    (<code>R/ca.R</code>). The parameter values don&rsquo;t translate to
    population-level historical fits &mdash; don&rsquo;t use them for that.</p>
  </div>
</div>

<div class="pcard">
  <h3 style="margin-top:0;">modern-estimates</h3>
  <p>Contemporary post-antibiotic-era plague. Shorter durations
  (<code>m_h = 0.15</code>, 6.7-day disease), higher <code>g_h = 0.15</code>
  (15% untreated CFR survival).</p>
  <div class="judgement">
    <div class="judgement-label">My take</div>
    <p>Use for Madagascar 2017, US Southwest sporadic cases, etc. Modern
    surveillance gives much better <code>p_obs</code> &mdash; consider pinning
    it at 0.95 rather than 0.8 if you&rsquo;re fitting modern data.</p>
  </div>
</div>
'

footer <- '
<hr class="divider">
<p class="muted" style="font-size: 0.82rem;">
  Generated from <code>R/app_priors.R::prior_default()</code>,
  <code>R/app_model_config.R::model_config_default()</code>, and
  <code>inst/scenarios/*.yaml</code> by
  <code>inst/explainer/build.R</code>. Rerun the script to refresh after
  prior or scenario edits.</p>
'

html <- sprintf(paste0(
  '<!doctype html>\n<html lang="en">\n<head>\n',
  '<meta charset="utf-8">\n',
  '<title>yersinia &mdash; parameter &amp; prior explainer</title>\n',
  '<meta name="viewport" content="width=device-width, initial-scale=1">\n',
  '<style>%s</style>\n</head>\n<body>\n<div class="wrap">\n%s\n%s\n%s\n%s\n%s\n%s\n</div>\n</body>\n</html>\n'
), css, intro, quick_ref, groups_html, varies_section, scenario_philosophy, footer)

out_path <- file.path("inst", "explainer", "parameters.html")
writeLines(html, out_path)
message("Wrote ", out_path, " (", nchar(html), " chars)")
