### Exemplo 5 Para um modelo AR ####
rm(list=ls())
require(R2WinBUGS)
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
inits = function(){list(phi = rnorm(1), 
                        tau = rgamma(1,1,1))}

collected_sample <- bugs(data_model, inits, c('phi', 'sigma'),
                         'Experimento_5.txt', n.chains = 2, n.iter = 2000,
                         n.burnin = 0, bugs.directory = '/home/gabriel/Downloads/WinBUGS/',
                         debug = T, n.thin=1)

sample_mcmc <- as.mcmc.list(collected_sample)

traceplot(sample_mcmc)

####Gerando o modelo e coletando amostra (com burn-in)####
collected_sample <- bugs(data_model, inits, c('phi', 'sigma'),
                         'Experimento_5.txt', n.chains = 4, n.iter = 1500,
                         n.burnin = 500, bugs.directory = '/home/gabriel/Downloads/WinBUGS/', n.thin=1)

sample_mcmc <- as.mcmc.list(collected_sample)

#### Análises ####
gelman.diag(sample_mcmc, autoburnin = F)

plot(sample_mcmc[[1]])

summary(sample_mcmc[[1]][,c('phi','sigma')])

lattice::densityplot(sample_mcmc[[1]])
par(mfrow=c(2,2))
traceplot(sample_mcmc[[1]])

autocorr.plot(sample_mcmc[[1]])
autocorr.diag(sample_mcmc[[1]])

effectiveSize(sample_mcmc[[1]])
BayesianTools::correlationPlot(sample_mcmc[[1]])

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
inits = function(){list(phi = rnorm(p), 
                        tau = rgamma(1,1,1))}

collected_sample <- bugs(data_model, inits, c('phi', 'sigma'),
                         'Experimento_5_1.txt', n.chains = 2, n.iter = 2000,
                         n.burnin = 0, bugs.directory = '/home/gabriel/Downloads/WinBUGS/',
                         debug = T, n.thin=1)

sample_mcmc <- as.mcmc.list(collected_sample)

traceplot(sample_mcmc)

collected_sample <- bugs(data_model, inits, c('phi', 'sigma'),
                         'Experimento_5_1.txt', n.chains = 4, n.iter = 1500,
                         n.burnin = 500, bugs.directory = '/home/gabriel/Downloads/WinBUGS/', n.thin=1)

sample_mcmc <- as.mcmc.list(collected_sample)

#### Análises ####
gelman.diag(sample_mcmc, autoburnin = F)

plot(sample_mcmc[[1]])

summary(sample_mcmc[[1]])

lattice::densityplot(sample_mcmc[[1]])
par(mfrow=c(2,2))
traceplot(sample_mcmc[[1]])

autocorr.plot(sample_mcmc[[1]])
autocorr.diag(sample_mcmc[[1]])

effectiveSize(sample_mcmc[[1]])
BayesianTools::correlationPlot(sample_mcmc[[1]])
