# Human deterministic plague model (rats, fleas, and humans)
# This file contains the odin model definition for the rat-flea-human plague model

## State variables initial conditions
initial(S_r) <- K_r - I_ini         # Susceptible rats
initial(I_r) <- I_ini               # Infected rats 
initial(R_r) <- 0                   # Resistant rats
initial(N) <- K_f                   # Flea index (fleas per rat)
initial(F) <- 0                     # Free infectious fleas
initial(S_h) <- K_h                 # Susceptible humans
initial(I_h) <- 0                   # Infected humans
initial(R_h) <- 0                   # Recovered humans

## Total populations
T_r <- S_r + I_r + R_r  
T_h <- S_h + I_h + R_h

## Force of infection in humans
lambda_h <- F * exp(-a * T_r)
output(lambda_h) <- lambda_h  

## Differential equations for rat population
deriv(S_r) <- r_r * S_r * (1 - T_r/K_r) +                # Births from susceptibles
              r_r * R_r * (1 - p) -                       # Non-immune births from resistant
              d_r * S_r -                                 # Natural deaths
              beta_r * (S_r/T_r) * F * (1 - exp(-a*T_r))  # Infections

deriv(I_r) <- beta_r * (S_r/T_r) * F * (1 - exp(-a*T_r)) -  # New infections
              (d_r + m_r) * I_r                              # Death/Recovery

deriv(R_r) <- r_r * R_r * (p - T_r/K_r) +   # Inherited immune births
              m_r * g_r * I_r -              # Recovery
              d_r * R_r                      # Natural death

## Human population dynamics
deriv(S_h) <- r_h * (S_h + R_h) * (1 - T_h/K_h) -    # Births
              d_h * S_h -                              # Natural deaths
              beta_h * S_h * lambda_h                  # Infections

deriv(I_h) <- beta_h * S_h * lambda_h -               # New infections
              (d_h + m_h) * I_h                       # Death/Recovery

deriv(R_h) <- m_h * g_h * I_h -                       # Recovery
              d_h * R_h                               # Natural death

## Flea dynamics
deriv(N) <- r_f * N * (1 - N/K_f) +                   # Logistic growth
            F * (1 - exp(-a*T_r)) / T_r               # Free fleas finding rats

deriv(F) <- (d_r + m_r * (1 - g_r)) * I_r * N -       # Free infected fleas from dead rats
            d_f * F                                    # Free flea deaths

## Rat parameters  
K_r <- user(2500)     # Rat carrying capacity
r_r <- user(5)        # Rat population growth rate 
p <- user(0.975)      # Probability of inherited resistance
d_r <- user(0.2)      # Natural death rate of rats
beta_r <- user(4.7)   # Rat infection rate from fleas
a <- user(0.004)      # Flea search efficiency
m_r <- user(20)       # Infected rat mortality rate
g_r <- user(0.02)     # Probability rat survives infection
I_ini <- user(1)      # Initial infected rats

## Human parameters
K_h <- user(5000)     # Human carrying capacity
r_h <- user(0.045)    # Human population growth rate
d_h <- user(0.04)     # Natural death rate of humans
beta_h <- user(0.01)  # Human infection rate from fleas
m_h <- user(26)       # Infected human recovery rate
g_h <- user(0.1)      # Probability human survives infection

## Flea parameters
r_f <- user(20)       # Flea reproduction rate
K_f <- user(6.57)     # Flea carrying capacity per rat
d_f <- user(10)       # Death rate of free fleas