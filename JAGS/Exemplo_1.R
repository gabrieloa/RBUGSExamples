#Exemplo Para um modelo de regressão

rm(list=ls())
require(rjags)
require(coda)

set.seed(42)

##Gerando os dados
sample_size <- 30

x <- rnorm(sample_size)

int_true <- 30
slope_true <- 10
mu <- int_true + slope_true * x
sigma <- 4

y <- rnorm(sample_size, mean = mu, sd = sigma)

data_frame <- data.frame(y = y, x = x)
head(data_frame)

##Definindo os dados 
data_model <- list('n'= sample_size, 'y' = y, 'x' = x)

####Gerando o modelo e coletando amostra (sem descarte de burn-in)####
model <- jags.model(file = 'Experimento_1.txt',
                    data = data_model, 
                    n.chains = 2)

#Coletando a amostra
collected_sample <- coda.samples(model, 
                                variable.names = c('beta0', 'beta1', 'sigma'),
                                n.iter = 2000)

par(mfrow=c(2,2))
traceplot(collected_sample)

####Gerando o modelo e coletando amostra (com burn-in de 500 iterações)####
model <- jags.model(file = 'Experimento_1.txt', data = data_model, 
                    n.chains = 4, n.adapt = 0)


update(model, 500)
#Coletando a amostra
collected_sample <- coda.samples(model, 
                                variable.names = c('beta0', 'beta1', 'sigma'),
                                n.iter = 1000)

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
