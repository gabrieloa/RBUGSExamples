data {
  int <lower=1> n;
  vector[n] x;
  vector[n] y;
}

parameters{
  real beta0;
  real beta1;
  real <lower=0> sigma;
}

model{
  beta0 ~ normal(0, 0.001);
  beta1 ~ normal(0, 0.001);
  
  sigma ~ inv_gamma(0.001, 0.001);
  y ~ normal(beta0 + beta1*x, sigma);
}