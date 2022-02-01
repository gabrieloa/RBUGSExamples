### Exemplo 3 Para um modelo de regressão Logistica ####

rm(list=ls())
require(stan)
require(coda)

data('mtcars')
summary(mtcars)

#### Definindo os dados #### 
df <- mtcars[, c('vs', 'wt', 'disp')]

data_model <- list('y' = as.vector(df$vs),
                   'x1' = as.vector(df$wt),
                   'x2' = as.vector(df$disp),
                   'N' = nrow(df),
                   'mu0' = 0, 'g0' = 0.001,
                   'mu1' = 0, 'g1' = 0.001,
                   'mu2' = 0, 'g2' = 0.001)

####Gerando o modelo e coletando amostra (sem descarte de burn-in)####
model <- stan('Experimento_4.stan', data = data_model,
              chains=2, iter=2100, warmup = 100,
              pars=c('beta0', 'beta1', 'beta2'))

collected_sample <- As.mcmc.list(model)

par(mfrow=c(2,2))
traceplot(collected_sample)


####Gerando o modelo e coletando amostra (com burn-in)####
model <- stan('Experimento_4.stan', data = data_model,
              chains=4, iter=6000, warmup = 1000,
              pars=c('beta0', 'beta1', 'beta2'))

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

####Gerando o modelo e coletando amostra (com burn-in de 2000 iterações)####
model <- stan('Experimento_4.stan', data = data_model,
              chains=4, iter=7000, warmup = 2000,
              pars=c('beta0', 'beta1', 'beta2'))

collected_sample <- As.mcmc.list(model)

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

#### Reparametrizado ####
model <- stan('Experimento_4Rep.stan', data = data_model,
              chains=4, iter=7000, warmup = 2000,
              pars=c('beta0', 'beta1', 'beta2'))

collected_sample_new <- As.mcmc.list(model)

gelman.diag(collected_sample_new)

plot(collected_sample_new)
plot(collected_sample_new[[1]])

summary(collected_sample_new[[1]])

lattice::densityplot(collected_sample_new[[1]])
par(mfrow=c(2,2))
traceplot(collected_sample_new[[1]])


autocorr.plot(collected_sample_new[[1]])
autocorr.diag(collected_sample_new[[1]])
autocorr.diag(collected_sample[[1]])

effectiveSize(collected_sample_new[[1]])

eff_1 <- sapply(collected_sample_new, effectiveSize)

eff_2 <- sapply(collected_sample, effectiveSize)
rowMeans(eff_2)
rowMeans(eff_1)

