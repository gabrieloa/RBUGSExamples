### Exemplo 1 Para um modelo de regressão ####

rm(list=ls())
require(rjags)
require(coda)

set.seed(42)

##### Gerando os dados ####
sample_size <- 30

x <- rnorm(sample_size)

int_true <- 30
slope_true <- 10
mu <- int_true + slope_true * x
sigma <- 4

y <- rnorm(sample_size, mean = mu, sd = sigma)

data_frame <- data.frame(y = y, x = x)
head(data_frame)

#### Definindo os dados #### 
data_model <- list('n'= sample_size, 'y' = y, 'x' = x)

#inits = function(){list(beta0 = rnorm(1), beta1 = rnorm(1), 
#                        tau = rgamma(1,1,1))}

#### Gerando o modelo e coletando amostra (sem descarte de burn-in) ####
model <- stan('Experimento_2.stan', data = data_model,
              chains=2, iter=2100, warmup = 100)


collected_sample <- As.mcmc.list(model)

par(mfrow=c(2,2))
traceplot(collected_sample)

#### Gerando o modelo e coletando amostra (com burn-in de 500 iterações) ####
model <- stan('Experimento_2.stan', data = data_model,
     chains=4, iter=1500, warmup = 500)

collected_sample <- As.mcmc.list(model)

#### Análises ####
gelman.diag(collected_sample)

plot(collected_sample)
plot(collected_sample[[1]])

summary(collected_sample[[1]])

lattice::densityplot(collected_sample[[1]])
par(mfrow=c(2,2))
traceplot(collected_sample[[1]])


autocorr.plot(collected_sample[[1]])
autocorr.diag(collected_sample[[1]])

effectiveSize(collected_sample[[1]])
BayesianTools::correlationPlot(collected_sample[[1]])
