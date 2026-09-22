# Minimal continuous-time (ODE) version of the carcass plague model.
# FEASIBILITY SPIKE: the only question is whether odin2 can generate an adjoint
# (gradients) for this structure, which is what HMC needs.
#
# Deliberately stripped down relative to inst/odin/plague_stochastic_humans.R:
#   - no rat demography (births, natural death, resistance)
#   - no human demography
#   - no human-to-human transmission
#   - no if/else clamps (they are not differentiable)
# Everything load-bearing for the thermal question is kept: carcass-mediated
# transmission, the Keeling-Gilligan host-preference term, and per-day thermal
# forcing on beta via interpolation.

## thermal forcing on transmission, supplied as a per-day series
w_beta <- interpolate(w_time, w_value, "linear")

T_r <- S + I

## Didelot transmission with the flea host-preference split
pref     <- exp(-rho * T_r / K_r)
lambda_r <- beta_r * w_beta * Q * (1 - pref) / T_r
lambda_h <- beta_h * w_beta * Q * pref / K_r

deriv(S)   <- -lambda_r * S
deriv(I)   <-  lambda_r * S - m_r * I
deriv(Q)   <-  m_r * I * (1 - g_r) - delta_R * Q

deriv(S_h) <- -lambda_h * S_h
deriv(I_h) <-  lambda_h * S_h - m_h * I_h
## D_h accumulates plague deaths within each observation window and resets each
## unit of time, so its value at integer times is that day's death count.
deriv(D_h) <-  m_h * I_h * (1 - g_h)

initial(S)   <- K_r - I_ini
initial(I)   <- I_ini
initial(Q)   <- 0
initial(S_h) <- K_h
initial(I_h) <- 0
initial(D_h, zero_every = 1) <- 0

## parameters
K_r     <- parameter(2500)
K_h     <- parameter(5000)
I_ini   <- parameter(5)
beta_r  <- parameter(0.5)
beta_h  <- parameter(0.02)
rho     <- parameter(2.5)
m_r     <- parameter(0.2)
m_h     <- parameter(0.08)
g_r     <- parameter(0)
g_h     <- parameter(0.1)
delta_R <- parameter(0.2)

kappa           <- parameter(15)
p_obs           <- parameter(0.8)
lambda_baseline <- parameter(1)

w_time  <- parameter()
w_value <- parameter()
dim(w_time)  <- parameter(rank = 1)
dim(w_value) <- parameter(rank = 1)

## observation model
deaths <- data()
deaths ~ NegativeBinomial(size = kappa, mu = p_obs * D_h + lambda_baseline)
