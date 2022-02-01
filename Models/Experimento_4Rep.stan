data{
  int <lower=1> N;
  int<lower=0, upper=1> y[N];
  vector[N] x1;
  vector[N] x2;
  real mu0;
  real <lower=0> g0;
  real mu1;
  real <lower=0> g1;
  real mu2;
  real <lower=0> g2;
}

parameters{
  real beta0;
  real beta1;
  real beta2;
}

transformed parameters{
  vector[N] mu;
  vector[N] x_1_trans;
  vector[N] x_2_trans;
  x_1_trans = (x1 - mean(x1))/sd(x1);
  x_2_trans = (x2 - mean(x2))/sd(x2);
  mu = beta0 + beta1*x_1_trans + beta2*x_2_trans;
}

model{
  beta0 ~ normal(mu0, g0);
  beta1 ~ normal(mu1, g1);
  beta2 ~ normal(mu2, g2);
  y ~ bernoulli_logit(mu);
}