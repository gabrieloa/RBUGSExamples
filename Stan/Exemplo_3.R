### Exemplo 2 Para um modelo de regressão ####

rm(list=ls())
require(rjags)
require(coda)

rm(list=ls())

data("mtcars")

head(mtcars)

#### Definindo os dados #### 
df <- mtcars[, c('mpg', 'disp', 'hp', 'wt')]

data_model <- list('y' = as.vector(df$mpg),
                   'x1' = as.vector(df$disp),
                   'x2' = as.vector(df$hp),
                   'x3' = as.vector(df$wt),
                   'N' = nrow(df),
                   'mu0' = 0, 'g0' = 0.001,
                   'mu1' = 0, 'g1' = 0.001,
                   'mu2' = 0, 'g2' = 0.001,
                   'mu3' = 0, 'g3' = 0.001,
                   'a' = 0.001, 'b' = 0.001)

#### Gerando o modelo e coletando amostra (sem descarte de burn-in) ####

model <- stan('Experimento_3.stan', data = data_model,
              chains=2, iter=5100, warmup = 100, 
              pars=c('beta0', 'beta1', 'beta2', 'beta3', 'sigma'))

collected_sample <- As.mcmc.list(model)

par(mfrow=c(2,2))
traceplot(collected_sample)

#### Gerando o modelo e coletando amostra (com burn-in) ####

model <- stan('Experimento_3.stan', data = data_model,
              chains=4, iter=5500, warmup = 1500,
              pars=c('beta0', 'beta1', 'beta2', 'beta3', 'sigma'))

collected_sample <- As.mcmc.list(model)
#### Análises ####
gelman.diag(collected_sample, autoburnin = F)

plot(collected_sample)
plot(collected_sample[[1]])

summary(collected_sample[[1]])

lattice::densityplot(collected_sample[[1]])
par(mfrow=c(3,2))
traceplot(collected_sample[[1]])


autocorr.plot(collected_sample[[1]])
autocorr.diag(collected_sample[[1]])

effectiveSize(collected_sample[[1]])
BayesianTools::correlationPlot(collected_sample[[1]])


###Testando Lag####
model <- stan('Experimento_3.stan', data = data_model,
              chains=4, iter=5500, warmup = 1500, thin = 2,
              pars=c('beta0', 'beta1', 'beta2', 'beta3', 'sigma'))

collected_sample2 <- As.mcmc.list(model)


eff_1 <- sapply(collected_sample, effectiveSize)

eff_2 <- sapply(collected_sample2, effectiveSize)
rowMeans(eff_2)
rowMeans(eff_1)

model <- stan('Experimento_3.stan', data = data_model,
              chains=4, iter=5500, warmup = 1500, thin = 3,
              pars=c('beta0', 'beta1', 'beta2', 'beta3', 'sigma'))

collected_sample3 <- As.mcmc.list(model)

eff_3 <- sapply(collected_sample3, effectiveSize)
rowMeans(eff_3)
rowMeans(eff_2)
rowMeans(eff_1)
