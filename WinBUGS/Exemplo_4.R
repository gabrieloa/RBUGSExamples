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
                   'mu0' = 0, 'g0' = 0.1,
                   'mu1' = 0, 'g1' = 0.1,
                   'mu2' = 0, 'g2' = 0.1,
                   'mu3' = 0, 'g3' = 0.1)


inits = function(){list(b0 = rnorm(1, 0, 1), 
                        b1 = rnorm(1, 0, 1), 
                        b2 = rnorm(1, 0, 1), 
                        b3 = c(NA, rnorm(1, 0, 1)))}

collected_sample <- bugs(data_model, inits, c('b0', 'b1', 'b2', 'b3'),
                         'Experimento_4.txt', n.chains = 2, n.iter = 20000,
                         n.burnin = 0, bugs.directory = '/path_winbugs',
                         debug = T, n.thin=1)

collected_sample <- bugs(data_model, inits, c('b0', 'b1', 'b2', 'b3'),
                         'Experimento_4_1.txt', n.chains = 2, n.iter = 20000,
                         n.burnin = 0, bugs.directory = '/path_winbugs',
                         debug = T, n.thin=1)

sample_mcmc <- as.mcmc.list(collected_sample)
plot(sample_mcmc)


collected_sample <- bugs(data_model, inits, c('b0', 'b1', 'b2', 'b3'),
                         'Experimento_4_1.txt', n.chains = 4, n.iter = 30000,
                         n.burnin = 10000, bugs.directory = '/path_winbugs',
                         debug = T, n.thin=1)

sample_mcmc <- as.mcmc.list(collected_sample)
plot(sample_mcmc)

gelman.diag(sample_mcmc)

plot(sample_mcmc[[1]])

summary(sample_mcmc[[1]])

lattice::densityplot(sample_mcmc[[1]])
par(mfrow=c(2,2))
traceplot(sample_mcmc[[1]])

autocorr.plot(sample_mcmc[[1]])
autocorr.diag(sample_mcmc[[1]])

effectiveSize(sample_mcmc[[1]])
BayesianTools::correlationPlot(sample_mcmc[[1]])

