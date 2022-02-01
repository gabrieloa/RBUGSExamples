rm(list=ls())
require(rstan)
require(coda)

set.seed(42)

sample_size <- 200

mu <- 25
sigma <- 10
tau <- 1/sigma^2
x <- rnorm(sample_size, mu, sigma)


data_model <- list('n'= sample_size, 'x' = x)
####Gerando o modelo e coletando amostra (sem descarte de burn-in)####
model <- stan('Experimento_1.stan', data = data_model,
                         chains=2, iter=2100, warmup = 100)

collected_sample <- As.mcmc.list(model)
par(mfrow=c(2,2))
traceplot(collected_sample)

###Analisado a cadeia
model <- stan('Experimento_1.stan', data = data_model,
              chains=4, iter=2500, warmup = 500)

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
tau_sample <- 1/collected_sample[[1]][,'sigma']^2

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
mu_sample <- collected_sample[[1]][,'mu']

x_t <- rt(1000, df=df)*sqrt(sigma_t) + mu_t

par(mfrow=c(1,1))
d_posterior <- density(x_t)

hist(mu_sample, prob=T)
lines(d_posterior, lwd=2)

