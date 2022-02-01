data{
  int <lower=1> N;
  int <lower=1> K;
  int<lower=0, upper=1> y[N];
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
}

parameters{
  real beta0;
  real beta1;
  real beta2;
  real beta3;
}

transformed parameters{
  vector[N] mu;
  mu = beta0 + beta1*x1 + beta2*x2 + beta3*x3;
}

model{
  beta3 ~ normal(mu3, g3);
  beta0 ~ normal(mu0, g0);
  beta1 ~ normal(mu1, g1);
  beta2 ~ normal(mu2, g2);
  y ~ bernoulli_logit(mu);
}