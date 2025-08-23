# Basic deterministic plague model (rats and fleas only)
# This file contains the odin model definition for the basic rat-flea plague model

## State variables initial conditions
initial(S_r) <- K_r - I_ini         # Susceptible rats
initial(I_r) <- I_ini               # Infected rats 
initial(R_r) <- 0                   # Resistant rats
initial(N) <- K_f                   # Flea index (fleas per rat)
initial(F) <- 0                     # Free infectious fleas

## Total rat population
T_r <- S_r + I_r + R_r  

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

## Flea dynamics
deriv(N) <- r_f * N * (1 - N/K_f) +                   # Logistic growth
            F * (1 - exp(-a*T_r)) / T_r               # Free fleas finding rats

deriv(F) <- (d_r + m_r * (1 - g_r)) * I_r * N -       # Free infected fleas from dead rats
            d_f * F                                    # Free flea deaths

## Parameters  
K_r <- user(2500)     # Rat carrying capacity
r_r <- user(5)        # Rat population growth rate 
p <- user(0.975)      # Probability of inherited resistance
d_r <- user(0.2)      # Natural death rate of rats
beta_r <- user(4.7)   # Rat infection rate from fleas
a <- user(4e-3)       # Flea search efficiency
m_r <- user(20)       # Infected rat mortality rate
g_r <- user(0.02)     # Probability rat survives infection
r_f <- user(20)       # Flea reproduction rate
K_f <- user(6.57)     # Flea carrying capacity per rat
d_f <- user(10)       # Death rate of free fleas
I_ini <- user(1)      # Initial infected rats