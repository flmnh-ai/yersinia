## Single-population stochastic plague model with infectious rat carcasses
## and human dynamics including human-to-human transmission.
## Based on Didelot et al. (2017) combined with Keeling & Gilligan (2000).
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

# Human compartments. D_h is NEW deaths per step (not cumulative) so it
# can drive the NegBinomial likelihood directly. Cumulative deaths, if
# needed for plotting, are derived via cumsum() in post-processing.
update(S_h) <- S_h - n_SI_h + births_h - n_deaths_S_h
update(I_h) <- I_h + n_SI_h - n_IR_h - n_deaths_I_h
update(R_h) <- R_h + n_recovered_h - n_deaths_R_h
update(D_h) <- n_deaths_S_h + n_deaths_I_h + n_deaths_R_h + (n_IR_h - n_recovered_h)

# (Debug print() warnings for negative compartments were dropped during the
# odin2 port -- odin2 doesn't accept top-level print() statements the way
# odin.dust v1 did. The Binomial draws guarantee non-negative transitions
# structurally, so this is a small loss.)

### Rats
## Intermediate calculations
T_r <- S + I + R  # total rat population
T_h <- S_h + I_h + R_h  # total human population

rat_birth_rate <- r_r * (1 - T_r / K_r)  # per capita birth rate (per day)
# Clip negative birth rates when T_r > K_r (stochastic overshoots of carrying capacity).
# Replaces "negative births" (nonsensical, would break the Bernoulli draw below) with
# "no births while overcrowded" -- effectively a hard zero on density-dependent feedback
# above K_r. In practice this rarely fires because natural + disease mortality pull T_r
# back below K_r quickly. If it fires often, K_r is probably too low relative to initial
# conditions or demographic stochasticity is driving the population above K_r.
rat_birth_rate_clipped <- if(rat_birth_rate > 0) rat_birth_rate else 0

## Infection forces (Didelot formulation)
# From carcasses to rats: fleas preferentially seek live rats
lambda_r <- if(T_r > 0) beta_r * Q * (1 - exp(-rho * T_r / K_r)) / T_r else 0

# From carcasses to humans: fleas that don't find rats reach humans
lambda_h <- if(K_r > 0) beta_h * Q * exp(-rho * T_r / K_r) / K_r else 0

# Human-to-human transmission (pneumonic plague / ectoparasites)
lambda_hh <- if(T_h > 0) beta_I * I_h / T_h else 0

# --- Seasonal forcing stub (not yet active) -------------------------------
# To add Lewnard-Townsend style seasonality, multiply delta_R by a seasonal
# factor here, e.g.:
#   seasonal <- 1 + eps * cos(2 * pi * (time - phi) / 365)
#   delta_R_eff <- delta_R * seasonal
# and replace `delta_R` with `delta_R_eff` in the carcass decay probability
# below. In the DWH carcass framework, temperature enters through flea
# survival on carcasses, which maps onto delta_R (carcass infectivity decay).
# Requires additional parameters eps (amplitude, dimensionless) and phi
# (phase, days). Left out for now to keep the Proc B single-outbreak fits
# focused on the demographic-coupling channel.
# ---------------------------------------------------------------------------

## Individual probabilities of transition for rats:
p_SI <- 1 - exp(-lambda_r * tau)         # S to I probability for rats
p_IR <- 1 - exp(-m_r * tau)              # plague resolution
p_rat_birth <- 1 - exp(-rat_birth_rate_clipped * tau)
p_rat_death <- 1 - exp(-d_r * tau)       # natural rat death probability

## Draws from binomial distributions for rats
n_deaths_S <- Binomial(S, p_rat_death)
n_deaths_I <- Binomial(I, p_rat_death)
n_deaths_R <- Binomial(R, p_rat_death)
n_SI <- Binomial(S - n_deaths_S, p_SI)
n_IR <- Binomial(I - n_deaths_I, p_IR)          # rats whose plague resolves
n_recovered <- Binomial(n_IR, g_r)               # rats who survive infection
n_births_S <- Binomial(S - n_deaths_S, p_rat_birth)
n_births_R <- Binomial(R - n_deaths_R, p_rat_birth)
n_resistant_births <- Binomial(n_births_R, p)
n_susceptible_births <- n_births_S + n_births_R - n_resistant_births

### Carcasses
# Rats that die of plague become infectious carcasses
n_new_carcasses <- n_IR - n_recovered
# Carcasses lose infectivity (flea death/dispersal)
p_carcass_decay <- 1 - exp(-delta_R * tau)
n_carcass_decay <- Binomial(Q, p_carcass_decay)

## Human transitions
p_human_death <- 1 - exp(-d_h * tau)
birth_rate_h <- r_h
birth_rate_h_clipped <- if(birth_rate_h > 0) birth_rate_h else 0
p_human_birth <- 1 - exp(-birth_rate_h_clipped * tau)

# Combined human infection probability (carcasses + human-to-human)
p_SI_h <- 1 - exp(-(lambda_h + lambda_hh) * tau)
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
deaths <- data()
mu_deaths <- if (D_h > 0) D_h else 1e-6
deaths ~ NegativeBinomial(size = kappa, mu = mu_deaths)

## Initial states
initial(S) <- K_r * S_ini
initial(I) <- I_ini
initial(R) <- K_r * (1 - S_ini)
initial(Q) <- 0
initial(S_h) <- K_h
initial(I_h) <- 0
initial(R_h) <- 0
initial(D_h) <- 0

## User defined parameters -- ALL rates per day.
## Defaults aligned with inst/scenarios/defaults.yaml (Didelot + K&G blend).
## Fitting pipelines override via the monty packer.
tau <- parameter(1)         # time step in days
I_ini <- parameter(10)          # initial infected rats
S_ini <- parameter(1)           # proportion initially susceptible
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
