### Exemplo 4 Para um modelo de regressão Logistica ####
rm(list=ls())
require(stan)
require(coda)

data('mtcars')
summary(mtcars)

#### Definindo os dados #### 
df <- mtcars[, c('vs', 'wt', 'disp', 'am')]

data_model <- list('y' = as.vector(df$vs),
                   'x1' = as.vector(df$wt),
                   'x2' = as.vector(df$disp),
                   'x3' = as.vector(df$am)+1,
                   'K' = 2,
                   'N' = nrow(df),
                   'mu0' = 0, 'g0' = 0.1, #0.001
                   'mu1' = 0, 'g1' = 0.1,
                   'mu2' = 0, 'g2' = 0.1,
                   'mu3' = 0, 'g3' = 0.1)

####Gerando o modelo e coletando amostra (sem descarte de burn-in)####
model <- stan('Experimento_5.stan', data = data_model,
              chains=2, iter=20100, warmup = 100,
              pars=c('beta0', 'beta1', 'beta2', 'beta3'))

collected_sample <- As.mcmc.list(model)

par(mfrow=c(2,2))
traceplot(collected_sample)

####Gerando o modelo e coletando amostra (com burn-in)####
model <- stan('Experimento_5.stan', data = data_model,
              chains=4, iter=30000, warmup = 10000,
              pars=c('beta0', 'beta1', 'beta2', 'beta3'))
collected_sample <- As.mcmc.list(model)

#### Análises ####
gelman.diag(collected_sample, autoburnin = F)

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

#### Reparametrização ####
model_new <- stan('Experimento_5_1.stan', data = data_model,
                  chains=2, iter=20100, warmup = 100,
                  pars=c('beta0', 'beta1', 'beta2', 'beta3'))
collected_sample_new <- As.mcmc.list(model)

par(mfrow=c(2,2))
traceplot(collected_sample_new)

model_new <- stan('Experimento_5_1.stan', data = data_model,
                  chains=4, iter=30000, warmup = 10000,
                  pars=c('beta0', 'beta1', 'beta2', 'beta3'))
collected_sample_new <- As.mcmc.list(model_new)

gelman.diag(collected_sample_new, autoburnin = F)

plot(collected_sample_new)
plot(collected_sample_new[[1]])

summary(collected_sample_new[[1]])

lattice::densityplot(collected_sample_new[[1]])
par(mfrow=c(2,2))
traceplot(collected_sample_new[[1]])


autocorr.plot(collected_sample_new[[1]])
autocorr.diag(collected_sample_new[[1]])

effectiveSize(collected_sample_new[[1]])
BayesianTools::correlationPlot(collected_sample_new[[1]])


eff_1 <- sapply(collected_sample_new, effectiveSize)

eff_2 <- sapply(collected_sample, effectiveSize)
rowMeans(eff_2)
rowMeans(eff_1)
