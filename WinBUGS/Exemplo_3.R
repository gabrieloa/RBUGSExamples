#### Exemplo 2 Para um modelo de regressão ####

rm(list=ls())
require(R2WinBUGS)
require(coda)

rm(list=ls())

data("mtcars")

head(mtcars)

#### Definindo os dados ####
df <- mtcars[, c('mpg', 'disp', 'hp', 'wt')]

data_model <- list(y = as.vector(df$mpg),
                   x_1 = as.vector(df$disp),
                   x_2 = as.vector(df$hp),
                   x_3 = as.vector(df$wt),
                   N = nrow(df),
                   mu0 = 0, g0 = 0.001,
                   mu1 = 0, g1 = 0.001,
                   mu2 = 0, g2 = 0.001,
                   mu3 = 0, g3 = 0.001,
                   a = 0.001, b = 0.001)

####Gerando o modelo e coletando amostra (sem descarte de burn-in)####
inits = function(){list(beta0=rnorm(1), beta1=rnorm(1), beta2=rnorm(1), 
                         beta3=rnorm(1), tau=rgamma(1,1,1))}

collected_sample <- bugs(data_model, inits, c('beta0', 'beta1', 'beta2', 
                                              'beta3', 'sigma'),
                         'Experimento_3.txt', n.chains = 2, n.iter = 5000, 
                         n.thin=1, n.burnin = 0, 
                         bugs.directory = '/home/gabriel/Downloads/WinBUGS/',
                         debug = T)

sample_mcmc <- as.mcmc.list(collected_sample)
par(mfrow=c(3,2))
traceplot(sample_mcmc)

####Gerando o modelo e coletando amostra (com burn-in)####
collected_sample <- bugs(data_model, inits, c('beta0', 'beta1', 'beta2', 
                                              'beta3', 'sigma'),
                         'Experimento_3.txt', n.chains = 4, n.iter = 5500,
                         n.burnin = 1500, bugs.directory = '/home/gabriel/Downloads/WinBUGS/',
                         debug = F, n.thin=1)

sample_mcmc <- as.mcmc.list(collected_sample)

#### Análises ####
gelman.diag(collected_sample, autoburnin = F)

plot(sample_mcmc[[1]])

summary(sample_mcmc[[1]])

lattice::densityplot(sample_mcmc[[1]])
par(mfrow=c(3,2))
traceplot(sample_mcmc[[1]])

autocorr.plot(sample_mcmc[[1]])
autocorr.diag(sample_mcmc[[1]])

effectiveSize(sample_mcmc[[1]])
BayesianTools::correlationPlot(sample_mcmc[[1]])


###Testando Lag####
collected_sample2 <- bugs(data_model, inits, c('beta0', 'beta1', 'beta2', 
                                              'beta3', 'sigma'),
                         'Experimento_3.txt', n.chains = 4, n.iter = 5500,
                         n.burnin = 1500, bugs.directory = '/home/gabriel/Downloads/WinBUGS/',
                         debug = F, n.thin=2)

sample_mcmc2 <- as.mcmc.list(collected_sample2)

eff_1 <- sapply(sample_mcmc, effectiveSize)

eff_2 <- sapply(sample_mcmc2, effectiveSize)
rowMeans(eff_2)
rowMeans(eff_1)

#### Comparando modelos ####
inits = function(){list(beta0=rnorm(1), beta1=rnorm(1), beta2=rnorm(1), 
                        beta3=rnorm(1), tau=rgamma(1,1,1))}

collected_sample <- bugs(data_model, inits, c('beta0', 'beta1', 'beta2', 
                                              'beta3', 'sigma'),
                         'Experimento_3.txt', n.chains = 4, n.iter = 5500, 
                         n.thin=1, n.burnin = 1500, 
                         bugs.directory = '/home/gabriel/Downloads/WinBUGS/',
                         debug = F)

data_model <- list('y' = as.vector(df$mpg),
                   'x_1' = as.vector(df$disp),
                   'x_2' = as.vector(df$hp),
                   'N' = nrow(df),
                   'mu0' = 0, 'g0' = 0.001,
                   'mu1' = 0, 'g1' = 0.001,
                   'mu2' = 0, 'g2' = 0.001,
                   'a' = 0.001, 'b' = 0.001)

inits = function(){list(beta0=rnorm(1), beta1=rnorm(1), beta2=rnorm(1), 
                        tau=rgamma(1,1,1))}

collected_sample2 <- bugs(data_model, inits, c('beta0', 'beta1', 'beta2', 
                                              'sigma'),
                         'Experimento_3_1.txt', n.chains = 4, n.iter = 5500, 
                         n.thin=1, n.burnin = 1500, 
                         bugs.directory = '/home/gabriel/Downloads/WinBUGS/',
                         debug = F)
collected_sample$DIC
collected_sample2$DIC
