#Exemplo Para um modelo de regressão

rm(list=ls())
require(R2WinBUGS)
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
inits = function(){list(beta0 = rnorm(1), beta1 = rnorm(1), 
                        tau = rgamma(1,1,1))}

collected_sample <- bugs(data_model, inits, c('beta0', 'beta1', 'sigma'),
                         'Experimento_1.txt', n.chains = 2, n.iter = 2000,
                         n.burnin = 0, bugs.directory = 'path_winbugs',
                         debug = T, n.thin=1)

sample_mcmc <- as.mcmc.list(collected_sample)

traceplot(sample_mcmc)


collected_sample <- bugs(data_model, inits, c('beta0', 'beta1', 'sigma'),
                         'Experimento_1.txt', n.chains = 4, n.iter = 1500,
                         n.burnin = 500, bugs.directory = 'path_winbugs')

sample_mcmc <- as.mcmc.list(collected_sample)

gelman.diag(sample_mcmc, autoburnin=F)

plot(sample_mcmc[[1]])

summary(sample_mcmc[[1]])

lattice::densityplot(sample_mcmc[[1]])
par(mfrow=c(2,2))
traceplot(sample_mcmc[[1]])

autocorr.plot(sample_mcmc[[1]])
autocorr.diag(sample_mcmc[[1]])

effectiveSize(sample_mcmc[[1]])
BayesianTools::correlationPlot(sample_mcmc[[1]])
