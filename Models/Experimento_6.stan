data{
  int <lower=1> N;
  vector[N] y;
}

parameters{
  real phi;
  real <lower=0> sigma;
  }


model{
  phi ~ normal(0, 0.001);
  sigma ~ inv_gamma(0.001, 0.001);
  for(n in 2:N){
    y[n] ~ normal(phi*y[n-1], sigma);
  }
}