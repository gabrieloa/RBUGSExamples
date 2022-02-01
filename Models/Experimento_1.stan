data {
  int <lower=1> n;
  vector[n] x;
}

parameters{
  real mu;
  real <lower=0> sigma;
}

model{
  mu ~ normal(0, sigma);
  sigma ~ inv_gamma(0.001, 0.001);
  x ~ normal(mu, sigma);
}