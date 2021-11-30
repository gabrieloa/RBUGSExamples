rm(list=ls())
require(rjags)
require(coda)

data('mtcars')
summary(mtcars)

df <- mtcars[, c('vs', 'wt', 'disp', 'am')]

data_model <- list('y' = as.vector(df$vs),
                   'x_1' = as.vector(df$wt),
                   'x_2' = as.vector(df$disp),
                   'x_3' = as.vector(df$am),
                   'K' = 2,
                   'N' = nrow(df),
                   'mu0' = 0, 'g0' = 0.1, #0.001
                   'mu1' = 0, 'g1' = 0.1,
                   'mu2' = 0, 'g2' = 0.1,
                   'mu3' = 0, 'g3' = 0.1)

####Gerando o modelo e coletando amostra (sem descarte de burn-in)####
model <- jags.model(file = 'Experimento_4.txt',
                    data = data_model, 
                    n.chains = 2,
                    n.adapt = 0)

#Coletando a amostra
collected_sample <- coda.samples(model, 
                                 variable.names = c('b0', 'b1', 'b2', 'b3[2]'),
                                 n.iter = 20000)

par(mfrow=c(2,2))
traceplot(collected_sample)

model <- jags.model(file = 'Experimento_4.txt',
                    data = data_model, 
                    n.chains = 4,
                    n.adapt = 0)

update(model, 10000)

#Coletando a amostra
collected_sample <- coda.samples(model, 
                                 variable.names = c('b0', 'b1', 'b2', 'b3[2]'),
                                 n.iter = 20000)

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

####Reparametrização
model_new <- jags.model(file = 'Experimento_4_1.txt',
                    data = data_model, 
                    n.chains = 2,
                    n.adapt = 0)

#Coletando a amostra
collected_sample_new <- coda.samples(model_new, 
                                 variable.names = c('b0', 'b1', 'b2', 'b3[2]'),
                                 n.iter = 20000)

par(mfrow=c(2,2))
traceplot(collected_sample_new)

model_new <- jags.model(file = 'Experimento_4_1.txt',
                        data = data_model, 
                        n.chains = 4,
                        n.adapt = 0)
update(model_new, 10000)
#Coletando a amostra
collected_sample_new <- coda.samples(model_new, 
                                     variable.names = c('b0', 'b1', 'b2', 'b3[2]'),
                                     n.iter = 20000)


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
