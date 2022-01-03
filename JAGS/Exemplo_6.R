### Exemplo 5 Para um modelo AR ####
rm(list=ls())
require(rjags)
require(coda)

#### Gerando os dados ####
set.seed(1234)

N <- 100
sigma <- 4
phi <- 0.4

y <- vector()

y[1] <- rnorm(1, 0, sigma)

for(t in 2:N){
  y[t] <- rnorm(1, phi*y[t-1], sigma)
}

#### Coletando dados ####
data_model <- list('y'= y, 'N' = N)

####Gerando o modelo e coletando amostra (sem descarte de burn-in)####
model <- jags.model(file = 'Experimento_6.txt',
                    data = data_model, 
                    n.chains = 2)

#Coletando a amostra
collected_sample <- coda.samples(model, 
                                 variable.names = c('phi', 'sigma'),
                                 n.iter = 2000)

par(mfrow=c(2,2))
traceplot(collected_sample)

####Gerando o modelo e coletando amostra (com burn-in)####
model <- jags.model(file = 'Experimento_6.txt', data = data_model, 
                    n.chains = 4, n.adapt = 0)


update(model, 500)
#Coletando a amostra
collected_sample <- coda.samples(model, 
                                 variable.names = c('phi', 'sigma'),
                                 n.iter = 1000)
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

#### AR(2) ####
N <- 100
sigma <- 4
p <- 2
phi <- c(0.4, 0.1)

y <- vector()

y[1] <- rnorm(1, 0, sigma)
y[2] <- rnorm(1, y[1], sigma)

for(t in (p+1):N){
  y[t] <- rnorm(1, phi*y[(t-p):(t-1)], sigma)
}

data_model <- list('y'= y, 'N' = N, 'p'=p)

####Gerando o modelo e coletando amostra ####
model <- jags.model(file = 'Experimento_6_1.txt',
                    data = data_model, 
                    n.chains = 2)

#Coletando a amostra
collected_sample <- coda.samples(model, 
                                 variable.names = c('phi', 'sigma'),
                                 n.iter = 2000)

par(mfrow=c(2,2))
traceplot(collected_sample)

model <- jags.model(file = 'Experimento_6_1.txt', data = data_model, 
                    n.chains = 4, n.adapt = 0)


update(model, 500)
#Coletando a amostra
collected_sample <- coda.samples(model, 
                                 variable.names = c('phi', 'sigma'),
                                 n.iter = 1000)
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
