## -------------------------------------------------------------
## 1.  Load the deSolve package
## -------------------------------------------------------------
library(deSolve)

## -------------------------------------------------------------
## 2.  Derivative function (moves the algebra from Stan)
## -------------------------------------------------------------
plague <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    ## auxiliary expressions
    RTot     <- RR + SR + IR
    eps      <- 1e-8                        # avoid division by zero
    RTot_eps <- RTot + eps
    fracF    <- Fl / RTot_eps
    fracD    <- d_F/RTot_eps
    fracSR   <- SR / RTot_eps
    infect   <- 1 - exp(-alpha * RTot)
    
    ## ODEs (the “_” symbols in the Stan snippet were placeholders for “*”)
    dH  <- - d_H  * fracF * H
    dF  <-   (d_R  + m_R * (1 - g_R)) * IR * Np - d_F * Fl
    dNp <-   r_F * Np * (1 - Np /( K_F)) + fracD * infect 
    dIR <-   b_R * fracSR * Fl * infect - (d_R + m_R) * IR
    dRR <-   r_R * RR * (p - RTot / K_R) + m_R * g_R * IR - d_R * RR
    dSR <-   r_R * SR * (1 - RTot / K_R) + r_R * RR * (1 - p)
    - d_R * SR - b_R * fracSR * Fl * infect
    
    list(c(dH, dF, dNp, dIR, dRR, dSR))
  })
}

## -------------------------------------------------------------
## 3.  Define initial state vector
## -------------------------------------------------------------
init <- c(
  H  = 25000.0,   # Humans
  Fl  = 1e2,   # Infected Fleas
  Np = 5e5,   # Uninfected Fleas
  IR = 5e2,   # Infected Rats
  RR = 5e2,   # Resistant Rats
  SR = 5e6    # Susceptible Rats
)

## -------------------------------------------------------------
## 4.  Set parameter values (replace with your own!)
## -------------------------------------------------------------

## Parameters from Keeling and Giligan

params <- c(
  d_H  = 0.04,   # Human death rate
  d_R  = 0.2,     # Rat natural death rate
  m_R  = 20,     # Rate at which infected rats become infectious
  g_R  = 0.02,      
  d_F  = 10,    # Flea mortality
  K_F  = 20,      # Flea carrying capacity
  alpha= 4e-3,     # Transmission scale
  b_R  = 4.7,     # Rat infection rate from fleas
  r_R  = 5,      # Rat growth rate
  r_F  = 20,      # Flea growth rate
  p    = 0.5,      # Proportion that become resistant
  K_R  = 2500       # Rat carrying capacity
)

###################################################

## "Working" parameters

params <- c(
  d_H  = 0.0001,   # Human death rate
  d_R  = 0.001,     # Rat natural death rate
  m_R  = 0.005,     # Rate at which infected rats become infectious
  g_R  = 0.51,      
  d_F  = 0.001,    # Flea mortality
  K_F  = 1e9,      # Flea carrying capacity
  alpha= 0.0002,     # Transmission scale
  b_R  = 0.05,     # Rat infection rate from fleas
  r_R  = 0.1,      # Rat growth rate
  r_F  = 0.005,      # Flea growth rate
  p    = 0.5,      # Proportion that become resistant
  K_R  = 1e6       # Rat carrying capacity
)

## -------------------------------------------------------------
## 5.  Run the solver
## -------------------------------------------------------------
times <- seq(0, 100, by = 1)           # 0 to 100 time units
out   <- ode(y = init, times = times,
             func = plague, parms = params)

## -------------------------------------------------------------
## 6.  Quick plot to visualise the runs
## -------------------------------------------------------------

par(mfrow=c(3,2))

plot(out[,2]~times,type="l",main="Humans",ylab="Population",xlab="Time") # H
plot(out[,3]~times,type="l",main="Infected Fleas",ylab="Population",xlab="Time") # F
plot(out[,4]~times,type="l",main="Uninfected Fleas",ylab= "Population",xlab="Time") # N
plot(out[,5]~times,type="l",main="Infected Rats",xlab="Time",ylab="Population") # IR
plot(out[,6]~times,type="l",main="Resistant Rats",xlab="Time",ylab="Population") # RR
plot(out[,7]~times,type="l",main="Susceptible Rats",xlab="Time",ylab="Population") # SR

