#Exemplo Para um modelo de regressão logistica

rm(list=ls())
require(rjags)
require(coda)

data('mtcars')
summary(mtcars)

df <- mtcars[, c('vs', 'wt', 'disp')]

data_model <- list('y' = as.vector(df$vs),
                   'x_1' = as.vector(df$wt),
                   'x_2' = as.vector(df$disp),
                   'N' = nrow(df),
                   'mu0' = 0, 'g0' = 0.001,
                   'mu1' = 0, 'g1' = 0.001,
                   'mu2' = 0, 'g2' = 0.001)

####Gerando o modelo e coletando amostra (sem descarte de burn-in)####
model <- jags.model(file = 'Experimento_3.txt',
                    data = data_model, 
                    n.chains = 2)

#Coletando a amostra
collected_sample <- coda.samples(model, 
                                 variable.names = c('b0', 'b1', 'b2'),
                                 n.iter = 2000)

par(mfrow=c(2,2))
traceplot(collected_sample)

model <- jags.model(file = 'Experimento_3.txt',
                    data = data_model, 
                    n.chains = 2,
                    n.adapt = 0)

#Coletando a amostra
collected_sample <- coda.samples(model, 
                                 variable.names = c('b0', 'b1', 'b2'),
                                 n.iter = 5000)

par(mfrow=c(2,2))
traceplot(collected_sample)

####Gerando o modelo e coletando amostra (com burn-in de 1000 iterações)####
model <- jags.model(file = 'Experimento_3.txt', data = data_model, 
                    n.chains = 4, n.adapt = 0)


update(model, 1000)
#Coletando a amostra
collected_sample <- coda.samples(model, 
                                 variable.names = c('b0', 'b1', 'b2'),
                                 n.iter = 5000)

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

####Gerando o modelo e coletando amostra (com burn-in de 1000 iterações)####
model <- jags.model(file = 'Experimento_3.txt', data = data_model, 
                    n.chains = 4, n.adapt = 0)


update(model, 2000)
#Coletando a amostra
collected_sample <- coda.samples(model, 
                                 variable.names = c('b0', 'b1', 'b2'),
                                 n.iter = 5000)

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

###Reparametrizado
model_new <- jags.model(file = 'Experimento_3Rep.txt', data = data_model, 
                        n.chains = 4, n.adapt = 0)
update(model_new, 2000)

collected_sample_new <- coda.samples(model_new, 
                                     variable.names = c('b0', 'b1', 'b2'),
                                     n.iter = 5000)

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

