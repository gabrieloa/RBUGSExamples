data{
  int <lower=1> N;
  int <lower=1> p;
  vector[N] y;
}

parameters{
  vector[p] phi;
  real <lower=0> sigma;
}


model{
  phi ~ normal(0, 0.001);
  sigma ~ inv_gamma(0.001, 0.001);
  for(n in (p+1):N){
    real mu = 0;
    for(k in 1:p){
      mu += phi[k] * y[n-k];
    }
    y[n] ~ normal(mu, sigma);
  }
}