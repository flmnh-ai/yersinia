## Core equations for transitions between compartments:
update(S[]) <- S[i] - n_SI[i] + n_susceptible_births[i] - n_deaths_S[i] - n_emigrate_S[i] + n_immigrate_S[i]
update(I[]) <- I[i] + n_SI[i] - n_IR[i] - n_deaths_I[i]- n_emigrate_I[i] + n_immigrate_I[i]
update(R[]) <- R[i] + n_recovered[i] + n_resistant_births[i] - n_deaths_R[i]- n_emigrate_R[i] + n_immigrate_R[i]
update(F[]) <- F[i] + n_new_free_fleas[i] - n_flea_deaths[i] - n_emigrate_F[i] + n_immigrate_F[i] - n_fleas_to_rats[i]
update(N[]) <- N[i] + flea_growth_rate[i] + n_fleas_to_rats[i] / T_r[i]

### Rats
## Intermediate calculations
T_r[] <- S[i] + I[i] + R[i] # total rat population
rat_birth_rate[] <- r_r * (1 - T_r[i] / K_r) # per capita
rat_birth_rate_clipped[] <- if(rat_birth_rate[i] > 0) rat_birth_rate[i] else 0

# old version with island like force of infeciton
#infection_force[] <- beta_r / T_r[i] * (1 - exp(-a * T_r[i])) * ((1 - mu_r) * F[i] + mu_r / (npop - 1) * (sum(F) - F[i]))
## older version with structure force of infection
# First calculate the denominator (number of neighbors)
#n_contacts[] <- sum(contact[i,])
#dim(n_contacts) <- npop

# Then calculate the sum of neighbor fleas
#neighbor_fleas[,] <- contact[i,j] * F[j]
# dim(neighbor_fleas) <- c(npop,npop)

#  nonlocal_fleas[] <- sum(neighbor_fleas[i,])
# dim(nonlocal_fleas) <- npop

# Finally calculate force of infection using these intermediate values
#infection_force[] <- beta_r / T_r[i] * (1 - exp(-a * T_r[i])) *
#                    ((1 - mu_r) * F[i] + mu_r * nonlocal_fleas[i] / n_contacts[i])

#infection_force[] <- beta_r / T_r[i] * (1 - exp(-a * T_r[i])) * ((1 - mu_r) * F[i] + mu_r * sum(contact[i,j] * F[j]) / sum(contact[i, ]))

infection_force[] <- beta_r * (F[i] / T_r[i]) * (1 - exp(-a * T_r[i]))

# seaonal change in k_f
season[] <- user()
dim(season) <- user()
season_t <- season[step + 1]
K_f_seasonal <- K_f * (1 + 1) ^ season_t # the + 1 should be a paramter, but fixed to approximate KG for now

## Individual probabilities of transition:
p_SI[] <- 1 - exp(-infection_force[i] * dt) # S to I
p_IR[] <- 1 - exp(-m_r * dt) # I to R
p_rat_birth[] <- 1 - exp(-rat_birth_rate_clipped[i] * dt) # natural rat birth probability
p_rat_death <- 1 - exp(-d_r * dt) # natural rat death probability

## Draws from binomial distributions
n_deaths_S[] <- rbinom(S[i], p_rat_death)
n_deaths_I[] <- rbinom(I[i], p_rat_death)
n_deaths_R[] <- rbinom(R[i], p_rat_death)
n_SI[] <- rbinom(S[i] - n_deaths_S[i], p_SI[i])
n_IR[] <- rbinom(I[i] - n_deaths_I[i], p_IR[i]) # rats whose disease ends
n_recovered[] <- rbinom(n_IR[i], g_r) # rats who recover AND survive
n_births_S[] <- rbinom(S[i] - n_deaths_S[i], p_rat_birth[i])
n_births_R[] <- rbinom(R[i] - n_deaths_R[i], p_rat_birth[i] * 0.75) # hacky reduction in resistant birth rate, just temporary
n_resistant_births[] <- rbinom(n_births_R[i], p)
n_susceptible_births[] <- n_births_S[i] + n_births_R[i] - n_resistant_births[i]


### Fleas
p_flea_death <- 1 - exp(-d_f * dt)  # natural free flea death probability
p_flea_to_rat[] <- 1 - exp(-a * T_r[i] * dt)
# fleas that don't find hosts might die
n_fleas_to_rats[] <- rbinom(F[i], p_flea_to_rat[i])
n_flea_deaths[] <- rbinom(F[i] - n_fleas_to_rats[i], p_flea_death)
# dead infected rats x flea index
n_new_free_fleas[] <- floor(N[i] * (n_deaths_I[i] + (n_IR[i] - n_recovered[i])))
flea_growth_rate[] <- r_f * N[i] * (1 - N[i] / K_f_seasonal) * dt # absolute growth rate in flea index


# Add migration parameters
mu_r <- user(0.03)  # rat movement rate
mu_f <- user(0.008) # flea movement rate
contact[,] <- user() # contact matrix
dim(contact) <- c(npop, npop)

## Calculate migrations
# For rats
p_migrate_r <- 1 - exp(-mu_r * dt)
n_emigrate_S[] <- rbinom(S[i] - n_deaths_S[i], p_migrate_r)
n_emigrate_I[] <- rbinom(I[i] - n_deaths_I[i] - (n_IR[i] - n_recovered[i]), p_migrate_r)
n_emigrate_R[] <- rbinom(R[i] - n_deaths_R[i], p_migrate_r)

# # Multinomial draws for S rats
S_flow[,1] <- rbinom(n_emigrate_S[i], contact[i,1])
S_flow[,2:npop] <- rbinom(n_emigrate_S[i] - sum(S_flow[i,1:(j-1)]),
                          contact[i,j] / sum(contact[i,j:npop]))
n_immigrate_S[] <- sum(S_flow[,i])

# contact[i,j] / (if(sum(contact[i,j:npop]) > 0)
#   #                                     sum(contact[i,j:npop]) else 1))
# # Multinomial draws for I rats
I_flow[,1] <- rbinom(n_emigrate_I[i], contact[i,1])
I_flow[,2:npop] <- rbinom(n_emigrate_I[i] - sum(I_flow[i,1:(j-1)]),
                          contact[i,j] / sum(contact[i,j:npop])) # why doesn't this cause a divide by zero error sometimes??
n_immigrate_I[] <- sum(I_flow[,i])
#
# # Multinomial draws for R rats
R_flow[,1] <- rbinom(n_emigrate_R[i], contact[i,1])
R_flow[,2:npop] <- rbinom(n_emigrate_R[i] - sum(R_flow[i,1:(j-1)]),
                          contact[i,j] / sum(contact[i,j:npop]))
n_immigrate_R[] <- sum(R_flow[,i])
#
# # For fleas
p_migrate_f <- 1 - exp(-mu_f * dt)
n_emigrate_F[] <- rbinom(F[i], p_migrate_f)
#
# # Multinomial draws for fleas
F_flow[,1] <- rbinom(n_emigrate_F[i], contact[i,1])
F_flow[,2:npop] <- rbinom(n_emigrate_F[i] - sum(F_flow[i,1:(j-1)]),
                          contact[i,j] / sum(contact[i,j:npop]))
n_immigrate_F[] <- sum(F_flow[,i])


## Initial states
initial(S[]) <- K_r * S_ini
initial(I[]) <- I_ini[i]
initial(R[]) <- K_r * (1 - S_ini)
initial(N[]) <- K_f
initial(F[]) <- 0


## Dimensions
npop <- user(25)
dim(I_ini) <- npop
dim(S) <- npop
dim(I) <- npop
dim(R) <- npop
dim(N) <- npop
dim(F) <- npop
dim(rat_birth_rate_clipped) <- npop
dim(T_r) <- npop
dim(rat_birth_rate) <- npop
dim(p_SI) <- npop
dim(p_IR) <- npop
dim(p_rat_birth) <- npop
dim(n_SI) <- npop
dim(n_IR) <- npop
dim(n_recovered) <- npop
dim(flea_growth_rate) <- npop
dim(infection_force) <- npop
dim(p_flea_to_rat) <- npop
dim(n_fleas_to_rats) <- npop
dim(n_deaths_S) <- npop
dim(n_deaths_I) <- npop
dim(n_deaths_R) <- npop
dim(n_births_S) <- npop
dim(n_births_R) <- npop
dim(n_resistant_births) <- npop
dim(n_susceptible_births) <- npop
dim(n_new_free_fleas) <- npop
dim(n_flea_deaths) <- npop

dim(n_emigrate_S) <- npop
dim(n_emigrate_I) <- npop
dim(n_emigrate_R) <- npop
dim(n_emigrate_F) <- npop
dim(n_immigrate_S) <- npop
dim(n_immigrate_I) <- npop
dim(n_immigrate_R) <- npop
dim(n_immigrate_F) <- npop
# Add dimensions for flow matrices
dim(S_flow) <- c(npop, npop)
dim(I_flow) <- c(npop, npop)
dim(R_flow) <- c(npop, npop)
dim(F_flow) <- c(npop, npop)

## User defined parameters
dt <- user(1/365)    # time step
I_ini[] <- user()
S_ini <- user(1) # proportion
K_r <- user(2500)     # Rat carrying capacity
r_r <- user(5)        # Rat population growth rate
p <- user(0.975)      # Probability of inherited resistance
d_r <- user(0.2)      # Natural death rate of rats
beta_r <- user(4.7)   # Rat infection rate from fleas
a <- user(4e-3)      # Flea search efficiency
m_r <- user(20)       # Infected rat recovery rate
g_r <- user(0.02)     # Probability rat survives infection
r_f <- user(20)       # Flea reproduction rate
K_f <- user(6.57)     # Flea carrying capacity per rat
d_f <- user(10)       # Death rate of free fleas
