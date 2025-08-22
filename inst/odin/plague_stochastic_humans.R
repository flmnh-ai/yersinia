# Core equations for transitions between compartments:
update(S) <- S - n_SI + n_susceptible_births - n_deaths_S
update(I) <- I + n_SI - n_IR - n_deaths_I
update(R) <- R + n_recovered + n_resistant_births - n_deaths_R
update(D) <-  n_deaths_S + n_deaths_I + n_deaths_R + (n_IR - n_recovered)

update(F) <- F + n_new_free_fleas - n_fleas_to_rats - n_flea_deaths
update(N) <- N + flea_growth_rate + n_fleas_to_rats / T_r

# Human compartments
update(S_h) <- S_h - n_SI_h + births_h - n_deaths_S_h
update(I_h) <- I_h + n_SI_h - n_IR_h - n_deaths_I_h
update(R_h) <- R_h + n_recovered_h - n_deaths_R_h
update(D_h) <-  n_deaths_S_h + n_deaths_I_h + n_deaths_R_h + (n_IR_h - n_recovered_h)


# Print warnings for negative values
print("susceptibles {S}", when = S < 0)
print("infected {I}", when = I < 0)
print("recovered {R}", when = R < 0)
print("fleas {F}", when = F < 0)
print("human susceptibles {S_h}", when = S_h < 0)
print("human infected {I_h}", when = I_h < 0)
print("human recovered {R_h}", when = R_h < 0)

### Rats
## Intermediate calculations
T_r <- S + I + R  # total rat population
T_h <- S_h + I_h + R_h  # total human population

print("total rats {T_r}", when = T_r <= 0)

rat_birth_rate <- r_r * (1 - T_r / K_r)  # per capita
rat_birth_rate_clipped <- if(rat_birth_rate > 0) rat_birth_rate else 0

## Flea dynamics
# Fleas attempt to find rats
p_flea_to_rat <- 1 - exp(-a * T_r * dt)
n_fleas_to_rats <- rbinom(F, p_flea_to_rat)

# Remaining fleas may die
remaining_fleas <- F - n_fleas_to_rats
p_flea_death <- 1 - exp(-d_f * dt)
n_flea_deaths <- rbinom(remaining_fleas, p_flea_death)

## Infection forces
infection_force <- beta_r * n_fleas_to_rats / T_r
# Human infection force based on fleas that don't find rats
infection_force_h <- beta_h * remaining_fleas

## Individual probabilities of transition for rats:
p_SI <- 1 - exp(-infection_force)  # S to I
p_IR <- 1 - exp(-m_r * dt)  # I to R
p_rat_birth <- 1 - exp(-rat_birth_rate_clipped * dt)  # natural rat birth probability
p_rat_death <- 1 - exp(-d_r * dt)  # natural rat death probability

## Draws from binomial distributions for rats
n_deaths_S <- rbinom(S, p_rat_death)
n_deaths_I <- rbinom(I, p_rat_death)
n_deaths_R <- rbinom(R, p_rat_death)
n_SI <- rbinom(S - n_deaths_S, p_SI)
n_IR <- rbinom(I - n_deaths_I, p_IR)  # rats whose disease ends
n_recovered <- rbinom(n_IR, g_r)  # rats who recover AND survive
n_births_S <- rbinom(S - n_deaths_S, p_rat_birth)
n_births_R <- rbinom(R - n_deaths_R, p_rat_birth * 0.75)
n_resistant_births <- rbinom(n_births_R, p)
n_susceptible_births <- n_births_S + n_births_R - n_resistant_births

## Human transitions
# Human demographic rates
p_human_death <- 1 - exp(-d_h * dt)
birth_rate_h <- r_h #* (1 - T_h / K_h)  # logistic growth
birth_rate_h_clipped <- if(birth_rate_h > 0) birth_rate_h else 0
p_human_birth <- 1 - exp(-birth_rate_h_clipped * dt)

# Disease transitions
p_SI_h <- 1 - exp(-infection_force_h * dt)
p_IR_h <- 1 - exp(-m_h * dt)

# Actual transitions
n_deaths_S_h <- rbinom(S_h, p_human_death)
n_deaths_I_h <- rbinom(I_h, p_human_death)
n_deaths_R_h <- rbinom(R_h, p_human_death)
births_h <- rbinom(S_h + R_h, p_human_birth)
n_SI_h <- rbinom(S_h - n_deaths_S_h, p_SI_h)
n_IR_h <- rbinom(I_h - n_deaths_I_h, p_IR_h)
n_recovered_h <- rbinom(n_IR_h, g_h)

### Fleas
# dead infected rats x flea index
n_new_free_fleas <- floor(N * (n_deaths_I + (n_IR - n_recovered)))
flea_growth_rate <- r_f * N * (1 - N / K_f) * dt  # absolute growth rate in flea index

## Initial states
initial(S) <- K_r * S_ini
initial(I) <- I_ini
initial(R) <- K_r * (1 - S_ini)
initial(D) <- 0
initial(N) <- K_f
initial(F) <- 0
initial(S_h) <- K_h
initial(I_h) <- 0
initial(R_h) <- 0
initial(D_h) <- 0

## User defined parameters
dt <- user(1/365)    # time step
I_ini <- user(10)    # initial infected rats
S_ini <- user(1)     # proportion initial susceptible rats
K_r <- user(2500)    # Rat carrying capacity
K_h <- user(5000)    # Human carrying capacity
r_r <- user(5)       # Rat population growth rate
r_h <- user(0.045)   # Human population growth rate
p <- user(0.975)     # Probability of inherited resistance
d_r <- user(0.2)     # Natural death rate of rats
d_h <- user(0.04)    # Natural death rate of humans
beta_r <- user(4.7)  # Rat infection rate from fleas
beta_h <- user(0.01) # Human infection rate from remaining fleas
a <- user(4e-3)      # Flea search efficiency for rats
m_r <- user(20)      # Infected rat recovery rate
m_h <- user(26)      # Infected human recovery rate
g_r <- user(0.02)    # Probability rat survives infection
g_h <- user(0.1)     # Probability human survives infection
r_f <- user(20)      # Flea reproduction rate
K_f <- user(6.57)    # Flea carrying capacity per rat
d_f <- user(10)      # Death rate of free fleas
