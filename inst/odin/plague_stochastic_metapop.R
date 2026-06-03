## Metapopulation stochastic plague model with infectious rat carcasses
## (Didelot et al. 2017 transmission core) plus rat and human dispersal
## between patches. Migration rates `mu_r` and `mu_h` each apply uniformly
## to their species' S, I, R compartments; carcasses Q are sessile. The
## rat-mediated mechanism (live infected rats migrating, dying elsewhere,
## producing local carcasses) is the canonical spatial-spread channel;
## adding human migration also lets directly human-to-human transmission
## (`beta_I`) drive intercity spread.
##
## All compartments are arrayed [npop]. Setting `mu_r = 0` and `mu_h = 0`
## decouples patches entirely; with identical per-patch parameters and
## deterministic mode, each patch's trajectory matches a single-population
## run.
##
## Note on human migration semantics: I_h represents all infected humans
## including the (mobile) 2-7 day incubation window before bubonic symptoms
## manifest; sessile-I_h would require an explicit E_h compartment to be
## defensible. Treating S_h/I_h/R_h symmetrically with a single mu_h is
## the parsimonious choice -- empirically small mu_h captures
## "sedentary-on-outbreak-timescales" behaviour without forcing a strong
## structural assumption. State-dependent mu_h (flight from infected
## cities) belongs in a separate v2 extension.
##
## Written in odin2 DSL. ALL rates per day. tau is the time step in days
## (default 1).

# Core compartment updates -- rats migrate (S, I, R), Q does not.
# (Human migration is in the human block below.)
update(S[]) <- S[i] - n_SI[i] + n_susceptible_births[i] - n_deaths_S[i] -
               n_emigrate_S[i] + n_immigrate_S[i]
update(I[]) <- I[i] + n_SI[i] - n_IR[i] - n_deaths_I[i] -
               n_emigrate_I[i] + n_immigrate_I[i]
update(R[]) <- R[i] + n_recovered[i] + n_resistant_births[i] - n_deaths_R[i] -
               n_emigrate_R[i] + n_immigrate_R[i]
update(Q[]) <- Q[i] + n_new_carcasses[i] - n_carcass_decay[i]
update(D_r[]) <- if (time %% obs_period == 0) n_new_carcasses[i] else (D_r[i] + n_new_carcasses[i])

# Human compartments -- S_h, I_h, R_h migrate at mu_h via contact_h
# (mirror of the rat block; D_h is an accumulator, doesn't migrate).
update(S_h[]) <- S_h[i] - n_SI_h[i] + births_h[i] - n_deaths_S_h[i] -
                 n_emigrate_S_h[i] + n_immigrate_S_h[i]
update(I_h[]) <- I_h[i] + n_SI_h[i] - n_IR_h[i] - n_deaths_I_h[i] -
                 n_emigrate_I_h[i] + n_immigrate_I_h[i]
update(R_h[]) <- R_h[i] + n_recovered_h[i] - n_deaths_R_h[i] -
                 n_emigrate_R_h[i] + n_immigrate_R_h[i]
update(D_h[]) <- if (time %% obs_period == 0) (n_IR_h[i] - n_recovered_h[i]) else (D_h[i] + n_IR_h[i] - n_recovered_h[i])

### Rats -- intermediate calculations (per patch)
T_r[] <- S[i] + I[i] + R[i]

rat_birth_rate_S[] <- r_r * (1 - T_r[i] / K_r[i])
rat_birth_rate_R[] <- r_r * iota * (1 - T_r[i] / K_r[i])
rat_birth_rate_S_clipped[] <- if (rat_birth_rate_S[i] > 0) rat_birth_rate_S[i] else 0
rat_birth_rate_R_clipped[] <- if (rat_birth_rate_R[i] > 0) rat_birth_rate_R[i] else 0

## Infection forces (per patch, Didelot formulation)
lambda_r[] <- if (T_r[i] > 0) beta_r * Q[i] * (1 - exp(-rho * T_r[i] / K_r[i])) / T_r[i] else 0
lambda_h[] <- if (K_r[i] > 0) beta_h * Q[i] * exp(-rho * T_r[i] / K_r[i]) / K_r[i] else 0
lambda_hh[] <- if (K_h[i] > 0) beta_I * I_h[i] / K_h[i] else 0

## Seasonal forcing on carcass decay (shared across patches in v1; same
## calendar everywhere). Indexing assumes tau = 1 -- the R wrapper enforces
## this when seasonal is non-trivial.
delta_R_eff <- delta_R * seasonal[time + 1]

## Probabilities (scalars are per-step survival/transition rates;
## arrays are per-patch infection probabilities)
p_SI_raw[] <- 1 - exp(-lambda_r[i] * tau)
p_SI[] <- if (p_SI_raw[i] > 0) p_SI_raw[i] else 0
p_IR <- 1 - exp(-m_r * tau)
p_rat_birth_S[] <- 1 - exp(-rat_birth_rate_S_clipped[i] * tau)
p_rat_birth_R[] <- 1 - exp(-rat_birth_rate_R_clipped[i] * tau)
p_rat_death <- 1 - exp(-d_r * tau)
p_carcass_decay <- 1 - exp(-delta_R_eff * tau)
p_migrate_r <- 1 - exp(-mu_r * tau)
p_migrate_h <- 1 - exp(-mu_h * tau)

## Rat draws
n_deaths_S[] <- Binomial(S[i], p_rat_death)
n_deaths_I[] <- Binomial(I[i], p_rat_death)
n_deaths_R[] <- Binomial(R[i], p_rat_death)
n_SI[] <- Binomial(S[i] - n_deaths_S[i], p_SI[i])
n_IR[] <- Binomial(I[i] - n_deaths_I[i], p_IR)
n_recovered[] <- Binomial(n_IR[i], g_r)
n_births_S[] <- Binomial(S[i] - n_deaths_S[i], p_rat_birth_S[i])
n_births_R[] <- Binomial(R[i] - n_deaths_R[i], p_rat_birth_R[i])
n_resistant_births[] <- Binomial(n_births_R[i], p)
n_susceptible_births[] <- n_births_S[i] + n_births_R[i] - n_resistant_births[i]
n_new_carcasses[] <- n_IR[i] - n_recovered[i]
n_carcass_decay[] <- Binomial(Q[i], p_carcass_decay)

## Migration of rats. Emigrants are drawn from rats remaining in their
## compartment after deaths and within-compartment transitions this step
## (matches the convention of the 2024-vintage spatial K&G model). Carcasses
## don't migrate; recovered individuals (n_recovered) entered R this step
## and migrate next step.
n_emigrate_S[] <- Binomial(S[i] - n_deaths_S[i] - n_SI[i], p_migrate_r)
n_emigrate_I[] <- Binomial(I[i] - n_deaths_I[i] - n_IR[i], p_migrate_r)
n_emigrate_R[] <- Binomial(R[i] - n_deaths_R[i], p_migrate_r)

## Multinomial routing via sequential conditional binomials.
## contact_r[i, j] is the row-stochastic destination probability for an
## emigrant leaving patch i (zero diagonal). The sequential trick draws
## flow[i, j] conditional on already-assigned destinations 1..j-1, with
## conditional probability contact_r[i, j] / sum(contact_r[i, j:npop]).
##
## sum_remaining_contact[i, j] = sum(contact_r[i, j:npop]) -- equals
## 1 - sum(contact_r[i, 1:(j-1)]) when contact_r is row-stochastic. Guards
## division-by-zero when all probability has been consumed (rare; see
## cond_p_r below).
sum_remaining_contact[, ] <- sum(contact_r[i, j:npop])
cond_p_r[, ] <- if (sum_remaining_contact[i, j] > 0) contact_r[i, j] / sum_remaining_contact[i, j] else 0

S_flow[, 1] <- Binomial(n_emigrate_S[i], cond_p_r[i, 1])
S_flow[, 2:npop] <- Binomial(n_emigrate_S[i] - sum(S_flow[i, 1:(j-1)]),
                             cond_p_r[i, j])
n_immigrate_S[] <- sum(S_flow[, i])

I_flow[, 1] <- Binomial(n_emigrate_I[i], cond_p_r[i, 1])
I_flow[, 2:npop] <- Binomial(n_emigrate_I[i] - sum(I_flow[i, 1:(j-1)]),
                             cond_p_r[i, j])
n_immigrate_I[] <- sum(I_flow[, i])

R_flow[, 1] <- Binomial(n_emigrate_R[i], cond_p_r[i, 1])
R_flow[, 2:npop] <- Binomial(n_emigrate_R[i] - sum(R_flow[i, 1:(j-1)]),
                             cond_p_r[i, j])
n_immigrate_R[] <- sum(R_flow[, i])

## Human transitions (per patch)
p_human_death <- 1 - exp(-d_h * tau)
birth_rate_h <- r_h
birth_rate_h_clipped <- if (birth_rate_h > 0) birth_rate_h else 0
p_human_birth <- 1 - exp(-birth_rate_h_clipped * tau)

p_SI_h_raw[] <- 1 - exp(-(lambda_h[i] + lambda_hh[i]) * tau)
p_SI_h[] <- if (p_SI_h_raw[i] > 0) p_SI_h_raw[i] else 0
p_IR_h <- 1 - exp(-m_h * tau)

n_deaths_S_h[] <- Binomial(S_h[i], p_human_death)
n_deaths_I_h[] <- Binomial(I_h[i], p_human_death)
n_deaths_R_h[] <- Binomial(R_h[i], p_human_death)
births_h[] <- Binomial(S_h[i] + R_h[i], p_human_birth)
n_SI_h[] <- Binomial(S_h[i] - n_deaths_S_h[i], p_SI_h[i])
n_IR_h[] <- Binomial(I_h[i] - n_deaths_I_h[i], p_IR_h)
n_recovered_h[] <- Binomial(n_IR_h[i], g_h)

## Migration of humans. Same pool convention as rats: emigrants drawn from
## the cohort remaining after deaths and within-compartment transitions
## this step. I_h migrates -- the model has no E_h compartment, so I_h
## includes the mobile incubation window; small mu_h captures sedentary
## behaviour without forcing sessile-I_h structurally.
n_emigrate_S_h[] <- Binomial(S_h[i] - n_deaths_S_h[i] - n_SI_h[i], p_migrate_h)
n_emigrate_I_h[] <- Binomial(I_h[i] - n_deaths_I_h[i] - n_IR_h[i], p_migrate_h)
n_emigrate_R_h[] <- Binomial(R_h[i] - n_deaths_R_h[i], p_migrate_h)

## Sequential multinomial routing for human emigrants over contact_h
## (analogous to contact_r routing for rats above).
sum_remaining_contact_h[, ] <- sum(contact_h[i, j:npop])
cond_p_h[, ] <- if (sum_remaining_contact_h[i, j] > 0) contact_h[i, j] / sum_remaining_contact_h[i, j] else 0

S_h_flow[, 1] <- Binomial(n_emigrate_S_h[i], cond_p_h[i, 1])
S_h_flow[, 2:npop] <- Binomial(n_emigrate_S_h[i] - sum(S_h_flow[i, 1:(j-1)]),
                               cond_p_h[i, j])
n_immigrate_S_h[] <- sum(S_h_flow[, i])

I_h_flow[, 1] <- Binomial(n_emigrate_I_h[i], cond_p_h[i, 1])
I_h_flow[, 2:npop] <- Binomial(n_emigrate_I_h[i] - sum(I_h_flow[i, 1:(j-1)]),
                               cond_p_h[i, j])
n_immigrate_I_h[] <- sum(I_h_flow[, i])

R_h_flow[, 1] <- Binomial(n_emigrate_R_h[i], cond_p_h[i, 1])
R_h_flow[, 2:npop] <- Binomial(n_emigrate_R_h[i] - sum(R_h_flow[i, 1:(j-1)]),
                               cond_p_h[i, j])
n_immigrate_R_h[] <- sum(R_h_flow[, i])

## Initial states (per patch). Each patch starts at K_r[i] total rats
## partitioned into S + I + R; humans similarly partitioned to K_h[i].
initial_S[] <- K_r[i] - I_ini[i] - R_ini[i]
initial(S[]) <- if (initial_S[i] > 0) initial_S[i] else 0
initial(I[]) <- I_ini[i]
initial(R[]) <- R_ini[i]
initial(Q[]) <- 0
initial(D_r[]) <- 0
initial_S_h[] <- K_h[i] - I_h_ini[i] - R_h_ini[i]
initial(S_h[]) <- if (initial_S_h[i] > 0) initial_S_h[i] else 0
initial(I_h[]) <- I_h_ini[i]
initial(R_h[]) <- R_h_ini[i]
initial(D_h[]) <- 0

## Dimensions
dim(S) <- npop
dim(I) <- npop
dim(R) <- npop
dim(Q) <- npop
dim(D_r) <- npop
dim(S_h) <- npop
dim(I_h) <- npop
dim(R_h) <- npop
dim(D_h) <- npop
dim(T_r) <- npop
dim(rat_birth_rate_S) <- npop
dim(rat_birth_rate_R) <- npop
dim(rat_birth_rate_S_clipped) <- npop
dim(rat_birth_rate_R_clipped) <- npop
dim(lambda_r) <- npop
dim(lambda_h) <- npop
dim(lambda_hh) <- npop
dim(p_SI_raw) <- npop
dim(p_SI) <- npop
dim(p_SI_h_raw) <- npop
dim(p_SI_h) <- npop
dim(p_rat_birth_S) <- npop
dim(p_rat_birth_R) <- npop
dim(n_deaths_S) <- npop
dim(n_deaths_I) <- npop
dim(n_deaths_R) <- npop
dim(n_SI) <- npop
dim(n_IR) <- npop
dim(n_recovered) <- npop
dim(n_births_S) <- npop
dim(n_births_R) <- npop
dim(n_resistant_births) <- npop
dim(n_susceptible_births) <- npop
dim(n_new_carcasses) <- npop
dim(n_carcass_decay) <- npop
dim(n_emigrate_S) <- npop
dim(n_emigrate_I) <- npop
dim(n_emigrate_R) <- npop
dim(n_immigrate_S) <- npop
dim(n_immigrate_I) <- npop
dim(n_immigrate_R) <- npop
dim(n_deaths_S_h) <- npop
dim(n_deaths_I_h) <- npop
dim(n_deaths_R_h) <- npop
dim(births_h) <- npop
dim(n_SI_h) <- npop
dim(n_IR_h) <- npop
dim(n_recovered_h) <- npop
dim(n_emigrate_S_h) <- npop
dim(n_emigrate_I_h) <- npop
dim(n_emigrate_R_h) <- npop
dim(n_immigrate_S_h) <- npop
dim(n_immigrate_I_h) <- npop
dim(n_immigrate_R_h) <- npop
dim(initial_S) <- npop
dim(initial_S_h) <- npop
dim(K_r) <- npop
dim(K_h) <- npop
dim(I_ini) <- npop
dim(R_ini) <- npop
dim(I_h_ini) <- npop
dim(R_h_ini) <- npop
dim(contact_r) <- c(npop, npop)
dim(contact_h) <- c(npop, npop)
dim(sum_remaining_contact) <- c(npop, npop)
dim(cond_p_r) <- c(npop, npop)
dim(S_flow) <- c(npop, npop)
dim(I_flow) <- c(npop, npop)
dim(R_flow) <- c(npop, npop)
dim(sum_remaining_contact_h) <- c(npop, npop)
dim(cond_p_h) <- c(npop, npop)
dim(S_h_flow) <- c(npop, npop)
dim(I_h_flow) <- c(npop, npop)
dim(R_h_flow) <- c(npop, npop)

## User-defined parameters -- ALL rates per day.
## Per-patch (length npop): K_r, K_h, I_ini, R_ini, I_h_ini, R_h_ini.
## Shared scalars: all rates (biological constants, not place-specific).
npop <- parameter(2, type = "integer", constant = TRUE)
tau <- parameter(1)
mu_r <- parameter(0)               # rat migration rate (per day; 0 = decoupled)
contact_r <- parameter()           # row-stochastic destination matrix [npop, npop]
mu_h <- parameter(0)               # human migration rate (per day; 0 = sessile humans)
contact_h <- parameter()           # row-stochastic destination matrix for humans [npop, npop]
K_r <- parameter()                 # rat carrying capacity per patch
K_h <- parameter()                 # human carrying capacity per patch
I_ini <- parameter()               # initial infected rats per patch (counts)
R_ini <- parameter()               # initial heritably-resistant rats per patch
I_h_ini <- parameter()             # initial infected humans per patch
R_h_ini <- parameter()             # initial recovered humans per patch
r_r <- parameter(0.01370)          # rat population growth rate (per day)
r_h <- parameter(0.000123)         # human population growth rate (per day)
p <- parameter(0.975)              # probability of inherited resistance
d_r <- parameter(0.000548)         # natural death rate of rats (per day)
d_h <- parameter(0.000110)         # natural death rate of humans (per day)
beta_r <- parameter(0.77)          # carcass-to-rat transmission (per day)
beta_h <- parameter(0.0145)        # carcass-to-human transmission (per day)
beta_I <- parameter(0.0)           # human-to-human transmission (per day)
rho <- parameter(2.63)             # rat carcass infectivity range
m_r <- parameter(0.056)            # plague resolution rate in rats (per day)
m_h <- parameter(0.125)            # plague resolution rate in humans (per day)
g_r <- parameter(0.02)             # probability rat survives infection
g_h <- parameter(0.1)              # probability human survives infection
delta_R <- parameter(0.267)        # carcass decay rate (per day)
iota <- parameter(0.75)            # fecundity multiplier for resistant rats
obs_period <- parameter(1)         # observation window in days for D_h / D_r
seasonal <- parameter()            # per-day multiplier on carcass decay
dim(seasonal) <- parameter(rank = 1)
