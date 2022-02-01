data{
  int <lower=1> N;
  vector[N] y;
  vector[N] x1;
  vector[N] x2;
  vector[N] x3;
  real mu0;
  real <lower=0> g0;
  real mu1;
  real <lower=0> g1;
  real mu2;
  real <lower=0> g2;
  real mu3;
  real <lower=0> g3;
  real <lower=0> a;
  real <lower=0> b;
}

parameters{
  real beta0;
  real beta1;
  real beta2;
  real beta3;
  real <lower=0> sigma;
}

transformed parameters{
  vector[N] mu;
  mu = beta0 + beta1*x1 + beta2*x2 + beta3*x3;
}

model{
  beta0 ~ normal(mu0, g0);
  beta1 ~ normal(mu1, g1);
  beta2 ~ normal(mu2, g2);
  beta3 ~ normal(mu3, g3);
  sigma ~ inv_gamma(a, b);
  y ~ normal(mu, sigma);
}