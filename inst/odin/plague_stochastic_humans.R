## Single-population stochastic plague model with infectious rat carcasses
## and human dynamics including human-to-human transmission.
## The transmission core follows Didelot, Whittles & Hall (2017; Cairo 1801):
## rat carcasses Q drive rat and human infection, and human-to-human
## transmission is frequency-scaled by the initial human population. Optional
## rat and human demography are added around that outbreak core. The rat R
## state is an optional immunity extension: with R_ini = 0 and g_r = 0, it is
## inert and the rat side reduces to Didelot's S_R, I_R, Q system.
##
## Written in odin2 DSL (Binomial, parameter, data). Compiled at runtime
## via odin2::odin() for simulation and via dust2::dust_filter_create()
## for pMCMC fitting.
##
## ALL rates are per day. tau is the time step in days (default 1).
## (Named `tau` -- the stochastic-process convention for a discrete time
## step -- because `dt` is reserved by odin2 as the internal step handle.)
##
## Single source of truth: monty.R and vignettes/monty.qmd should source
## this file via odin2::odin(system.file("odin/plague_stochastic_humans.R",
## package = "yersinia")) rather than inlining a copy.

# Core equations for transitions between compartments:
update(S) <- S - n_SI + n_susceptible_births - n_deaths_S
update(I) <- I + n_SI - n_IR - n_deaths_I
update(R) <- R + n_recovered + n_resistant_births - n_deaths_R
update(Q) <- Q + n_new_carcasses - n_carcass_decay
## D_r is NEW rat plague deaths per the current observation window (see D_h
## comment below). Not used in likelihood; exposed so posterior-predictive
## plots can show rat mortality alongside human deaths.
update(D_r) <- if (time %% obs_period == 0) n_new_carcasses else (D_r + n_new_carcasses)

# Human compartments. D_h is NEW plague deaths over the current observation
# window of length `obs_period` days -- not cumulative and not all-cause.
# Natural deaths still remove people from S_h/I_h/R_h but are excluded from
# D_h so that the likelihood compares like-for-like with historical outbreak
# records, which only count plague deaths. With obs_period = 1 (default) the
# accumulator resets every step and D_h is per-step (back-compatible with
# pre-2026 fits). With obs_period = 7 the accumulator sums seven daily
# steps, so D_h at time = 7, 14, ... matches a weekly death count. The
# manual reset (`time %% obs_period == 0`) is a workaround for odin2 not
# yet allowing parameterized `zero_every`.
update(S_h) <- S_h - n_SI_h + births_h - n_deaths_S_h
update(I_h) <- I_h + n_SI_h - n_IR_h - n_deaths_I_h
update(R_h) <- R_h + n_recovered_h - n_deaths_R_h
update(D_h) <- if (time %% obs_period == 0) (n_IR_h - n_recovered_h) else (D_h + n_IR_h - n_recovered_h)

### Rats
## Intermediate calculations
T_r <- S + I + R  # total rat population

rat_birth_rate_S <- r_r * (1 - T_r / K_r)  # per capita birth rate (per day)
rat_birth_rate_R <- r_r * iota * (1 - T_r / K_r)
# Clip negative birth rates when T_r > K_r (stochastic overshoots of carrying capacity).
# Replaces "negative births" (nonsensical, would break the Bernoulli draw below) with
# "no births while overcrowded" -- effectively a hard zero on density-dependent feedback
# above K_r. In practice this rarely fires because natural + disease mortality pull T_r
# back below K_r quickly. If it fires often, K_r is probably too low relative to initial
# conditions or demographic stochasticity is driving the population above K_r.
rat_birth_rate_S_clipped <- if(rat_birth_rate_S > 0) rat_birth_rate_S else 0
rat_birth_rate_R_clipped <- if(rat_birth_rate_R > 0) rat_birth_rate_R else 0

## Infection forces (Didelot formulation)
# From carcasses to rats: fleas preferentially seek live rats
lambda_r <- if(T_r > 0) beta_r * Q * (1 - exp(-rho * T_r / K_r)) / T_r else 0

# From carcasses to humans: fleas that don't find rats reach humans
lambda_h <- if(K_r > 0) beta_h * Q * exp(-rho * T_r / K_r) / K_r else 0

# Human-to-human transmission (pneumonic plague / ectoparasites).
# Didelot et al. scale this by the initial human population N_H; here K_h is
# that population-scale parameter, even when births/deaths move T_h slightly.
lambda_hh <- if(K_h > 0) beta_I * I_h / K_h else 0

## Seasonal forcing on carcass decay. `seasonal` is a per-day vector
## supplied at run time; when omitted, the R-side wrappers fill in 1s
## (no forcing). Indexing assumes tau = 1 (the model is daily) so that
## `time` is integer-valued and `time + 1` is the 1-based R index.
delta_R_eff <- delta_R * seasonal[time + 1]

## Individual probabilities of transition for rats. The if-else clamps
## guard against the rare case where 1 - exp(-rate * tau) returns a tiny
## negative value due to floating-point rounding when rate is very small;
## dust2's Binomial rejects p < 0, which crashes the simulation. Clamping
## to >= 0 is mathematically a no-op for any well-defined rate.
p_SI_raw <- 1 - exp(-lambda_r * tau)
p_SI <- if (p_SI_raw > 0) p_SI_raw else 0
p_IR <- 1 - exp(-m_r * tau)
p_rat_birth_S <- 1 - exp(-rat_birth_rate_S_clipped * tau)
p_rat_birth_R <- 1 - exp(-rat_birth_rate_R_clipped * tau)
p_rat_death <- 1 - exp(-d_r * tau)

## Draws from binomial distributions for rats
n_deaths_S <- Binomial(S, p_rat_death)
n_deaths_I <- Binomial(I, p_rat_death)
n_deaths_R <- Binomial(R, p_rat_death)
n_SI <- Binomial(S - n_deaths_S, p_SI)
n_IR <- Binomial(I - n_deaths_I, p_IR)          # rats whose plague resolves
n_recovered <- Binomial(n_IR, g_r)               # rats who survive infection
n_births_S <- Binomial(S - n_deaths_S, p_rat_birth_S)
n_births_R <- Binomial(R - n_deaths_R, p_rat_birth_R)
n_resistant_births <- Binomial(n_births_R, p)
n_susceptible_births <- n_births_S + n_births_R - n_resistant_births

### Carcasses
# Rats that die of plague become infectious carcasses
n_new_carcasses <- n_IR - n_recovered
# Carcasses lose infectivity (flea death/dispersal)
p_carcass_decay <- 1 - exp(-delta_R_eff * tau)
n_carcass_decay <- Binomial(Q, p_carcass_decay)

## Human transitions
p_human_death <- 1 - exp(-d_h * tau)
birth_rate_h <- r_h
birth_rate_h_clipped <- if(birth_rate_h > 0) birth_rate_h else 0
p_human_birth <- 1 - exp(-birth_rate_h_clipped * tau)

# Combined human infection probability (carcasses + human-to-human).
# Same clamp as p_SI: when lambda_h + lambda_hh is tiny, FP rounding can
# rarely produce p < 0 which dust2's Binomial rejects.
p_SI_h_raw <- 1 - exp(-(lambda_h + lambda_hh) * tau)
p_SI_h <- if (p_SI_h_raw > 0) p_SI_h_raw else 0
p_IR_h <- 1 - exp(-m_h * tau)

# Actual transitions
n_deaths_S_h <- Binomial(S_h, p_human_death)
n_deaths_I_h <- Binomial(I_h, p_human_death)
n_deaths_R_h <- Binomial(R_h, p_human_death)
births_h <- Binomial(S_h + R_h, p_human_birth)
n_SI_h <- Binomial(S_h - n_deaths_S_h, p_SI_h)
n_IR_h <- Binomial(I_h - n_deaths_I_h, p_IR_h)
n_recovered_h <- Binomial(n_IR_h, g_h)

## Data comparison (only evaluated when data is attached via dust_filter_create).
## Inert during plain simulation via dust_system_create.
## p_obs rescales simulated deaths before likelihood evaluation to allow for
## incomplete case ascertainment (mirrors the 2009 flu example in the
## odin-monty book, where observed cases = rho * incidence). Default 1 =
## perfect ascertainment, matching prior behavior.
##
## `lambda_baseline` is the expected count of non-plague deaths reported as
## plague per day — captures historical misclassification (other infectious
## diseases attributed to plague during outbreak periods) and sporadic
## background mortality lumped into "plague" records. Default 1 preserves
## the earlier hardcoded +1 floor (a strict generalization). Fit it to let
## the data inform the baseline (especially valuable when the outbreak tail
## stays at sustained low non-zero counts that pure plague dynamics can't
## explain). Identifiable from p_obs because the data has both high-plague
## periods (peak dominated by p_obs * D_h) and low-plague periods (tail
## dominated by lambda_baseline).
deaths <- data()
## Scale the per-day baseline up to the observation window so the
## interpretation of `lambda_baseline` is invariant across daily and weekly
## fits (it always means "expected non-plague deaths reported as plague per
## day"). With obs_period = 1 this is a no-op.
mu_deaths <- p_obs * D_h + lambda_baseline * obs_period
deaths ~ NegativeBinomial(size = kappa, mu = mu_deaths)

## Initial states
## Rat population starts at K_r total, partitioned into S + I + R. I_ini and
## R_ini are absolute counts (not fractions); susceptibles fill the remainder.
## With R_ini = 0 (default) and an infected seed I_ini, the rat side reduces
## to Didelot's S_R, I_R, Q system. Set R_ini > 0 to seed initial heritable
## resistance — only meaningful if rat demography (r_r, d_r, g_r, p, iota)
## is also active so the resistant pool can propagate via births.
##
## Clamp guards against negative initial S when the user passes K_r <
## I_ini + R_ini; in that pathological case the susceptible pool is zero
## and the I/R counts stand as given (the population starts smaller than
## K_r, which is itself unusual but not catastrophic).
initial_S <- K_r - I_ini - R_ini
initial(S) <- if (initial_S > 0) initial_S else 0
initial(I) <- I_ini
initial(R) <- R_ini
initial(Q) <- 0
initial(D_r) <- 0
## Human initial conditions. Default I_h_ini = R_h_ini = 0 reproduces the
## "outbreak begins at t = 0 with all humans susceptible" convention. Set
## I_h_ini > 0 (and optionally R_h_ini > 0) to fit data that begins
## mid-outbreak, where some humans are already infected or have recovered
## by the time records start. S_h is the residual.
initial(S_h) <- if (K_h > I_h_ini + R_h_ini) K_h - I_h_ini - R_h_ini else 0
initial(I_h) <- I_h_ini
initial(R_h) <- R_h_ini
initial(D_h) <- 0

## User defined parameters -- ALL rates per day.
## Defaults aligned with inst/scenarios/defaults.yaml (Didelot + K&G blend).
## Fitting pipelines override via the monty packer.
tau <- parameter(1)         # time step in days
I_ini <- parameter(10)          # initial infected rats (absolute count)
R_ini <- parameter(0)           # initial heritably-resistant rats (absolute count)
K_r <- parameter(2500)          # rat carrying capacity
K_h <- parameter(5000)          # human carrying capacity
r_r <- parameter(0.01370)       # rat population growth rate (per day, = 5/365)
r_h <- parameter(0.000123)      # human population growth rate (per day, = 0.045/365)
p <- parameter(0.975)           # probability of inherited resistance
d_r <- parameter(0.000548)      # natural death rate of rats (per day, = 0.2/365)
d_h <- parameter(0.000110)      # natural death rate of humans (per day, = 0.04/365)
beta_r <- parameter(0.77)       # transmission rate from carcasses to rats (per day)
beta_h <- parameter(0.0145)     # transmission rate from carcasses to humans (per day)
beta_I <- parameter(0.0)        # human-to-human transmission rate (per day)
rho <- parameter(2.63)          # rat carcass infectivity range (dimensionless)
m_r <- parameter(0.056)         # plague resolution rate in rats (per day)
m_h <- parameter(0.125)         # plague resolution rate in humans (per day)
g_r <- parameter(0.02)          # probability rat survives infection
g_h <- parameter(0.1)           # probability human survives infection
delta_R <- parameter(0.267)     # carcass decay rate (per day)
kappa <- parameter(5)           # NegBinomial overdispersion (size); larger = more Poisson-like
p_obs <- parameter(1)           # observation / ascertainment probability (0-1); 1 = all deaths reported
iota <- parameter(0.75)         # fecundity multiplier for resistant rats
I_h_ini <- parameter(0)         # initial infected humans (for mid-outbreak data)
R_h_ini <- parameter(0)         # initial recovered/immune humans (residual immunity)
lambda_baseline <- parameter(1) # baseline non-plague deaths reported as plague (per day)
# Observation window in days (integer). 1 = daily reporting (default,
# pre-2026 behaviour). 7 = weekly reporting; the D_h / D_r accumulators
# reset every 7 steps so the value at time = 7, 14, ... is the death count
# for the preceding week. Requires tau = 1 -- the R wrappers enforce this.
obs_period <- parameter(1)
# Per-day seasonal multiplier on carcass decay (dimensionless). Length must
# equal the number of simulation days. Required by the model itself, but the
# R wrappers default to rep(1, n_days) when not supplied (= no seasonality).
seasonal <- parameter()
dim(seasonal) <- parameter(rank = 1)
