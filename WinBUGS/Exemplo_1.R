rm(list=ls())
require(R2WinBUGS)
require(coda)

set.seed(42)

##Gerando os dados
sample_size <- 200

mu <- 25
sigma <- 10
tau <- 1/sigma^2
x <- rnorm(sample_size, mu, sigma)

data_model <- list('n'= sample_size, 'x' = x)
inits = function(){list(mu = rnorm(1), 
                        tau = rgamma(1,1,1))}

####Gerando o modelo e coletando amostra (sem descarte de burn-in)####
collected_sample <- bugs(data_model, inits, c('mu', 'sigma'),
                         'Experimento_1.txt', n.chains = 2, n.iter = 2000,
                         n.burnin = 0, bugs.directory = '/home/gabriel/Downloads/WinBUGS/',
                         debug = T, n.thin=1)
sample_mcmc <- as.mcmc.list(collected_sample)

par(mfrow=c(2,2))
traceplot(sample_mcmc)

collected_sample <- bugs(data_model, inits, c('mu', 'sigma', 'tau'),
                         'Experimento_1.txt', n.chains = 4, n.iter = 2500,
                         n.burnin = 500, bugs.directory = '/home/gabriel/Downloads/WinBUGS/',
                         debug = F, n.thin=1)

sample_mcmc <- as.mcmc.list(collected_sample)

gelman.diag(sample_mcmc)

plot(sample_mcmc)
plot(sample_mcmc[[1]])

summary(sample_mcmc[[1]])

lattice::densityplot(sample_mcmc[[1]])
par(mfrow=c(2,2))
traceplot(sample_mcmc[[1]])


autocorr.plot(sample_mcmc[[1]])
autocorr.diag(sample_mcmc[[1]])

effectiveSize(sample_mcmc[[1]])
BayesianTools::correlationPlot(sample_mcmc[[1]])

#priori normal-gamma (mu, lambda, alpha, beta) (0,0.001, 0.001, 0.001)
#posterior
# (lambda*mu + nx_bar)/(lamba + n)
# lambda + n
# alpha+n/2
# beta + 0.5*(n*s+(lambda*n*(x_bar-mu)^2)/(lambda+n))
# s= 1/n(var)

alpha_prior = 0.001
beta_prior = 0.001
lambda_prior = 0.001
mu_prior = 0.001
s = var(x)
x_bar = mean(x)

##Distribuição de tau
alpha_posterior = alpha_prior + sample_size/2
beta_posterior = beta_prior + 0.5*(sample_size*s + (lambda_prior*sample_size*(x_bar-mu_prior)^2)/(lambda_prior+sample_size))

alpha_posterior/beta_posterior
tau_sample <- sample_mcmc[[1]][,'tau']

x_gamma <- rgamma(1000, shape=alpha_posterior, rate = beta_posterior)
par(mfrow=c(1,1))
hist(tau_sample, prob=T)
lines(density(x_gamma),lwd=2)

###Distribuição de mu
mu_posterior = (lambda_prior*mu_prior + sample_size*x_bar)/(lambda_prior + sample_size)
lambda_posterior = (lambda_prior+sample_size)
df = 2*alpha_posterior
mu_t = mu_posterior
sigma_t = beta_posterior/(lambda_posterior*alpha_posterior)

mu_t
sigma_t*(df/(df-2))
mu_sample <- sample_mcmc[[1]][,'mu']

x_t <- rt(1000, df=df)*sqrt(sigma_t) + mu_t

par(mfrow=c(1,1))
d_posterior <- density(x_t)

hist(mu_sample, prob=T)
lines(d_posterior, lwd=2)
