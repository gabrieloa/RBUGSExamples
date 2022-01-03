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
                   'x_1' = as.vector(df$disp),
                   'x_2' = as.vector(df$hp),
                   'x_3' = as.vector(df$wt),
                   'N' = nrow(df),
                   'mu0' = 0, 'g0' = 0.001,
                   'mu1' = 0, 'g1' = 0.001,
                   'mu2' = 0, 'g2' = 0.001,
                   'mu3' = 0, 'g3' = 0.001,
                   'a' = 0.001, 'b' = 0.001)

#### Gerando o modelo e coletando amostra (sem descarte de burn-in) ####

model <- jags.model(file = 'Experimento_2.txt',
                    data = data_model,
                    n.chains = 2)

collected_sample <- coda.samples(model, 
                          variable.names = c('beta0', 'beta1', 'beta2', 'beta3', 'sigma'),
                          n.iter = 5000) 

par(mfrow=c(2,2))
traceplot(collected_sample)

#### Gerando o modelo e coletando amostra (com burn-in) ####

model <- jags.model(file = 'Experimento_2.txt',
                    data = data_model,
                    n.chains = 4)

update(model, 1500)
collected_sample <- coda.samples(model, 
                                 variable.names = c('beta0', 'beta1', 'beta2', 'beta3', 'sigma'),
                                 n.iter = 4000) 
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
model <- jags.model(file = 'Experimento_2.txt',
                    data = data_model,
                    n.chains = 4)

update(model, 1500)
collected_sample2 <- coda.samples(model, 
                                 variable.names = c('beta0', 'beta1', 'beta2', 'beta3', 'sigma'),
                                 n.iter = 4000,
                                 thin=2) 


eff_1 <- sapply(collected_sample, effectiveSize)

eff_2 <- sapply(collected_sample2, effectiveSize)
rowMeans(eff_2)
rowMeans(eff_1)

model <- jags.model(file = 'Experimento_2.txt',
                    data = data_model,
                    n.chains = 4)

update(model, 1500)
collected_sample3 <- coda.samples(model, 
                                  variable.names = c('beta0', 'beta1', 'beta2', 'beta3', 'sigma'),
                                  n.iter = 4000,
                                  thin=3) 

eff_3 <- sapply(collected_sample3, effectiveSize)
rowMeans(eff_3)
rowMeans(eff_2)
rowMeans(eff_1)

#### Comparando modelos ####
model1 <- jags.model(file = 'Experimento_2.txt',
                    data = data_model,
                    n.chains = 4)

update(model1, 1500)

dic_model1 <- dic.samples(model1, 4000)

data_model <- list('y' = as.vector(df$mpg),
                   'x_1' = as.vector(df$disp),
                   'x_2' = as.vector(df$hp),
                   'N' = nrow(df),
                   'mu0' = 0, 'g0' = 0.001,
                   'mu1' = 0, 'g1' = 0.001,
                   'mu2' = 0, 'g2' = 0.001,
                   'a' = 0.001, 'b' = 0.001)

model2 <- jags.model(file = 'Experimento_2_1.txt',
                     data = data_model,
                     n.chains = 4)
update(model2, 1500)

dic_model2 <- dic.samples(model2, 4000)
