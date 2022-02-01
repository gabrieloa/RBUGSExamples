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
  vector[N] x_1_trans;
  vector[N] x_2_trans;
  vector[N] x_3_trans;
  x_1_trans = x1 - mean(x1);
  x_2_trans = x2 - mean(x2);
  x_3_trans = x3 - mean(x3);
  mu = beta0 + beta1*x_1_trans + beta2*x_2_trans +beta3*x_3_trans;
}



model{
  beta0 ~ normal(mu0, g0);
  beta1 ~ normal(mu1, g1);
  beta2 ~ normal(mu2, g2);
  sigma ~ inv_gamma(a, b);
  y ~ normal(mu, sigma);
}