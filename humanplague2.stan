functions {
  // ODE System
  array[] real plague( // ODE function name
    real t,
    array[] real y,
    array[] real theta,
    array[] real x_r, // Unchanged
    array[] int x_i   // Unchanged
  ) {
    array[6] real dydt;

    real H  = y[1];   // Humans
    real F  = y[2];   // Infected Fleas
    real Np = y[3];   // Uninfected Fleas
    real IR = y[4];   // Infected Rats
    real RR = y[5];   // Resistent Rats
    real SR = y[6];   // Susceptible Rats 

    real d_H  = theta[1];
    real d_R  = theta[2];
    real m_R  = theta[3];
    real g_R  = theta[4];
    real d_F  = theta[5];
    real K_F  = theta[6];
    real alpha= theta[7];
    real b_R  = theta[8];
    real r_R  = theta[9];
    real r_F  = theta[10];
    real p    = theta[11];
    real K_R  = theta[12];

    // Total Rat population with function to avoid dividing by 0
    real eps = 1e-8;
    real RTot = RR + SR + IR;
    real RTot_eps = RTot + eps;
    real fracF  = F / RTot_eps;
    real fracD = d_F / RTot_eps;
    real fracSR = SR / RTot_eps;
    real infect_term = 1 - exp(-alpha * RTot);

    // ODEs
    dydt[1] = -d_H * fracF * H;
    dydt[2] = d_R + m_R * (1 - g_R) * IR * Np - d_F * F;
    dydt[3] = r_F * Np * (1 - Np / K_F) + fracD * infect_term;
    dydt[4] = b_R * fracSR * F * infect_term - (d_R + m_R) * IR;
    dydt[5] = r_R * RR * (p - RTot / K_R) + m_R * g_R * IR - d_R * RR;
    dydt[6] = r_R * SR * (1 - RTot / K_R) + r_R * RR * (1 - p) - d_R * SR - b_R * fracSR * F * infect_term;

    return dydt;
  }
}

data {
  int<lower=1> N;                // number of observed time steps (observations for t=1..N)
  real<lower=0> init;            // initial H
  array[N] int D;                // observed Deaths at times 1..N
}

transformed data {
  array[N+1] real times_measured;
  for (i in 1:N+1) times_measured[i] = i;  // solve at times 1..N
}

parameters {
  // initial states 
  real<lower=0> F_init; 
  real<lower=0> N_init;
  real<lower=0> IR_init;
  real<lower=0> RR_init;
  real<lower=0> SR_init;

  // biologically constrained parameters
  real<lower=0> d_H;
  real<lower=0> d_R;
  real<lower=0> m_R;
  real<lower=0> g_R;
  real<lower=0> d_F;
  real<lower=0> K_F;
  real<lower=0,upper=1> alpha;
  real<lower=0> b_R;
  real<lower=0> r_R;
  real<lower=0> r_F;
  real<lower=0,upper=1> p;
  real<lower=0> K_R;
}

transformed parameters {
  array[12] real theta;
  theta[1]  = d_H;
  theta[2]  = d_R;
  theta[3]  = m_R;
  theta[4]  = g_R;
  theta[5]  = d_F;
  theta[6]  = K_F;
  theta[7]  = alpha;
  theta[8]  = b_R;
  theta[9]  = r_R;
  theta[10] = r_F;
  theta[11] = p;
  theta[12] = K_R;

  // ODE solution: N output times, 6 states
  array[N+1,6] real y_hat;

  {
    array[6] real y_init;
    y_init[1] = init;
    y_init[2] = F_init;
    y_init[3] = N_init;
    y_init[4] = IR_init;
    y_init[5] = RR_init;
    y_init[6] = SR_init;

    y_hat = integrate_ode_rk45(plague, y_init, 0.0, times_measured, theta,
                               rep_array(0.0, 0), rep_array(0, 0),
                               1e-6, 1e-6, 1e7);
  }
}

model {
  // Priors (adjust to domain knowledge)
  F_init ~ normal(1e5, 100);
  N_init ~ normal(1e5, 100);
  IR_init ~ normal(15000, 50);
  RR_init ~ normal(15000, 50);
  SR_init ~ normal(15000, 50);

  d_H ~ exponential(10);
  d_R ~ exponential(10);
  m_R ~ exponential(10);
  g_R ~ exponential(10);
  d_F ~ exponential(10);
  K_F ~ exponential(10);
  alpha ~ normal(0.01, 0.05);
  r_R ~ exponential(10);
  r_F ~ exponential(10);
  b_R ~ exponential(10);
  p ~ beta(3,2);
  K_R ~ normal(1e5, 50);
  
  array[N] real Deaths;

  // Likelihood: deaths = drop in H between time steps
  for (t in 2:N+1) {
    Deaths[t-1] = y_hat[t-1][1] - y_hat[t][1];
    Deaths[t-1] = fmax(Deaths[t-1], 0);
    D[t-1] ~ poisson(Deaths[t-1] + 1e-6);
  }
}
