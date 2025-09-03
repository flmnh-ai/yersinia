functions {
  array[] real dpop_dt(real t,
                 array[] real init,
                 array[] real beta,
                 array[] real x_r, array[] int x_i) {
    real H = init[1];
    real d = beta[1];
    real dH_dt = -d * H;
    return { dH_dt };
  }
}
data {
  int<lower=0> N;
  array[N] int<lower=0> D;
  real<lower=0> init;
}
transformed data {
  array[N+1] real times_measured;
  for (i in 1:(N+1)) {
    times_measured[i] = i;
  }
}
parameters {
  array[1] real beta;
}
transformed parameters {
  array[N+1] real pop;                       // trajectory of H
  array[1] real y0;                          // initial state
  array[N+1, 1] real ode_sol;          // solver output
  y0[1] = init;
  ode_sol = integrate_ode_rk45(
    dpop_dt, y0, 0, times_measured, beta,
    rep_array(0.0,0), rep_array(0,0),
    1e-5, 1e-3, 5e2
  );
  for (t in 1:(N+1)) {
    pop[t] = ode_sol[t,1];             // extract single state variable
  }
}
model {
  array[N] real Deaths;

  beta ~ normal(0, 0.25);

  for (t in 2:(N+1)) {
    Deaths[t-1] = fmax(pop[t-1] - pop[t], 0);
    D[t-1] ~ poisson(Deaths[t-1]);
  }
}
