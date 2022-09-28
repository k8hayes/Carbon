# model using simulated data
# https://rpubs.com/jwesner/gamma_glm

library(R2jags)
library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())

library(bayesplot)

# simple example ############################################################
#fake_data_a - simulate from https://seananderson.ca/2014/04/08/gamma-glms/
set.seed(5555)
  N <- 100
  x <- runif(N, 0, 1)
  a <- 0.5 # intercept
  b1 <- 1.1 # beta 1
  y_true <- exp(a + b1 * x)
  shape <- 10

y <- rgamma(N, rate = shape / y_true, shape = shape)

fake_data_a <- data.frame(x = x, y = y)

ggplot(fake_data_a, aes(x = x, y = y))+
  geom_point()+
  ggtitle("regression")+
  coord_cartesian(ylim=c(0,7))

# jags version of model
jagsdata_1 <- with(fake_data_a, # creates a list of three elements
                    list(y = y,
                         x = x,
                         N = length(fake_data_a$y)))

lm1_jags <- function(){
  # Likelihood:
  for (i in 1:N){
    y[i] ~ dgamma(shape, shape / exp(mu[i]))
    mu[i] <- intercept + beta1 * x[i]
  }
  # Priors:
  intercept ~ dnorm(0, 2) # intercept
  beta1 ~ dnorm(0, 2) # slope
  shape ~ dgamma(0.01, 0.01) # shape
}

init_values <- function(){ # specifies initial parameter values
  list(intercept = runif(1), beta1 = runif(1),
       shape = rgamma(1,1))
}

params <- c("intercept", "beta1", "shape")

fit_lm1 <- jags(data = jagsdata_1, # specifies data
                inits = init_values,
                parameters.to.save = params,
                model.file = lm1_jags,
                n.chains = 3, # number of chains
                n.iter = 2000,
                n.burnin = 500) # only keeps every 10th iteration)
fit_lm1

plot(fit_lm1)

traceplot(fit_lm1)

lm_mcmc <- as.mcmc(fit_lm1)


## predictions #############################
nvalues <- 100
x_new <- seq(min(fake_data_a$x), max(fake_data_a$x),
             length.out = nvalues)

# combine 2 chains into one mcmc object
lm1_mcmc_combi <- as.mcmc(rbind(lm_mcmc[[1]], lm_mcmc[[2]]))

  # calculate predicted son heights for new father heights
  pred_mean_mean <- mean(lm1_mcmc_combi[, "intercept"]) + x_new * mean(lm1_mcmc_combi[, "beta1"])

  # also want to show uncertainty around mean predicted value
  # 2 forms of uncertainty
  # uncertainty about true parameter values (1)
  # uncertainty caused through stochastic relationship between father/son heights (2)

  # 1) credible uncertainty = credible interval around predicted sheight for given fheight
  pred_mean_dist <- matrix(NA, nrow = nrow(lm1_mcmc_combi), ncol = nvalues)
  # calculates 95% credible interval for each individual (n = 1078)
  for (i in 1:nrow(pred_mean_dist)){
    pred_mean_dist[i,] <- lm1_mcmc_combi[i,"intercept"] + x_new * lm1_mcmc_combi[i,"beta1"]
  }
  credible_lower <- apply(pred_mean_dist, MARGIN = 2, quantile, prob = 0.025)
  credible_upper <- apply(pred_mean_dist, MARGIN = 2, quantile, prob = 0.975)

  # 2) drawing random values for normal distribution w/ mean defined by fheight, alpha/beta + variance
  lm1_mcmc_combi_rep <- do.call(rbind, rep(list(lm1_mcmc_combi), 50)) # replication

  # Draw random values for all parameter combinations (rows) and body length values (columns):
  pred_data_dist <- matrix(NA, nrow = nrow(lm1_mcmc_combi_rep), ncol = nvalues)
  for (i in 1:nrow(pred_data_dist)){
    pred_data_dist[i,] <- lm1_mcmc_combi_rep[i,"intercept"] + x_new * lm1_mcmc_combi_rep[i,"beta1"]
      rnorm(nvalues, mean = 0, sd = lm1_mcmc_combi_rep[i, "shape"])
  }

  # Calculate quantiles:
  uncertain_lower <- apply(pred_data_dist, MARGIN = 2, quantile, prob = 0.025)
  uncertain_upper <- apply(pred_data_dist, MARGIN = 2, quantile, prob = 0.975)


  ## Plot of mean model prediction + 2 uncertainty measures #####

  dev.off()

  lm <- glm(y ~ x, data = fake_data_a, family = Gamma(link = "log"))
  summary(lm)
  # estimates - beta0  0.31, x = 1.1, z = 0.02

  plot(y ~ x, data = fake_data_a)
  lines(x_new, pred_mean_mean)
  lines(x_new, credible_lower, lty = 2)
  lines(x_new, credible_upper, lty = 2)
  lines(x_new, uncertain_lower, lty = 2, col = "red")
  lines(x_new, uncertain_upper, lty = 2, col = "red")

## testing bayesplot ###############################

  plot_title <- ggtitle("Posterior distributions",
                        "with medians and 80% intervals")
  mcmc_areas(lm_mcmc, pars = c("intercept", "beta1", "shape")) + plot_title

  fit_lm1$BUGSoutput$sims.list
  ppc_dens_overlay(y = lm_mcmc["y"],
                   yrep = posterior_predict(lm_mcmc, draws = 50))

  # adding variable to model ################################
  #fake_data_a - simulate from https://seananderson.ca/2014/04/08/gamma-glms/
  set.seed(5555)
  N <- 100
  x <- runif(N, 0, 1)
  z <- runif(N, 0, 3)
  q <- runif(N, 0, 5)

  a <- 0.5 # intercept
  b1 <- 1.1 # beta1
  b2 <- 0.7 # beta2
  b3 <- 2.4 # beta3
  y_true <- exp(a + b1 * x + b2 * z + b3 * q)
  shape <- 10

  y <- rgamma(N, rate = shape / y_true, shape = shape)

  fake_data_a <- data.frame(x = x, y = y, z = z, q = q)

  # jags version of model
  jagsdata_1 <- with(fake_data_a, # creates a list of three elements
                     list(y = y,
                          x = x,
                          z = z,
                          q = q,
                          N = length(fake_data_a$y)))

  lm1_jags <- function(){
    # Likelihood:
    for (i in 1:N){
      y[i] ~ dgamma(shape, shape / exp(mu[i]))
      mu[i] <- intercept + beta1 * x[i] + beta2 * z[i] + beta3 * q[i]
    }
    # Priors:
    intercept ~ dnorm(0, 2) # intercept
    beta1 ~ dnorm(0, 2)
    beta2 ~ dnorm(0, 2)
    beta3 ~ dnorm(0,2)
    shape ~ dgamma(0.01, 0.01) # shape
  }

  init_values <- function(){ # specifies initial parameter values
    list(intercept = runif(1),
         beta1 = runif(1), beta2 = runif(1), beta3 = runif(1),
         shape = rgamma(1,1))
  }

  params <- c("intercept",
              "beta1", "beta2","beta3",
              "shape")

  fit_lm1 <- jags(data = jagsdata_1, # specifies data
                  inits = init_values,
                  parameters.to.save = params,
                  model.file = lm1_jags,
                  n.chains = 3, # number of chains
                  n.iter = 2000,
                  n.burnin = 500) # only keeps every 10th iteration)
  fit_lm1

  plot(fit_lm1)

  traceplot(fit_lm1)

  lm_mcmc <- as.mcmc(fit_lm1)



  # adjusting variables to actual values ####################
  #fake_data_a - simulate from https://seananderson.ca/2014/04/08/gamma-glms/
  set.seed(5555)
  N <- 42
  x <- runif(N, 0, 2)
  z <- runif(N, 0, 2)
  q <- runif(N, 0, 15)

  a <- 0.5 # intercept
  b1 <- 1.1 # beta1
  b2 <- 0.7 # beta2
  b3 <- 2.4 # beta3
  y_true <- exp(a + b1 * x + b2 * z + b3 * q)
  shape <- 10

  y <- rgamma(N, rate = shape / y_true, shape = shape)

  fake_data_a <- data.frame(x = x, y = y, z = z, q = q)

  # jags version of model
  jagsdata_1 <- with(fake_data_a, # creates a list of three elements
                     list(y = y,
                          x = x,
                          z = z,
                          q = q,
                          N = length(fake_data_a$y)))

  lm1_jags <- function(){
    # Likelihood:
    for (i in 1:N){
      y[i] ~ dgamma(shape, shape / exp(mu[i]))
      mu[i] <- intercept + beta1 * x[i] + beta2 * z[i] + beta3 * q[i]
    }
    # Priors:
    intercept ~ dnorm(0, 2) # intercept
    beta1 ~ dnorm(0, 2)
    beta2 ~ dnorm(0, 2)
    beta3 ~ dnorm(0,2)
    shape ~ dgamma(0.01, 0.01) # shape
  }

  init_values <- function(){ # specifies initial parameter values
    list(intercept = runif(1),
         beta1 = runif(1), beta2 = runif(1), beta3 = runif(1),
         shape = rgamma(1,1))
  }

# scaled example ############################################################
  #fake_data_a - simulate from https://seananderson.ca/2014/04/08/gamma-glms/
  set.seed(5555)
  N <- 100
  x <- runif(N, 0, 1)
  a <- 0.5 # intercept
  b1 <- 1.1 # beta 1
  y_true <- exp(a + b1 * x)
  shape <- 10

  y <- rgamma(N, rate = shape / y_true, shape = shape)

  fake_data_a <- data.frame(x = scale(x, scale = TRUE),
                            y = y)

  ggplot(fake_data_a, aes(x = x, y = y))+
    geom_point()+
    ggtitle("regression")+
    coord_cartesian(ylim=c(0,7))

  # jags version of model
  jagsdata_1 <- with(fake_data_a, # creates a list of three elements
                     list(y = y,
                          x = x,
                          N = length(fake_data_a$y)))

  lm1_jags <- function(){
    # Likelihood:
    for (i in 1:N){
      y[i] ~ dgamma(shape, shape / exp(mu[i]))
      mu[i] <- intercept + beta1 * x[i]
    }
    # Priors:
    intercept ~ dnorm(0, 1) # intercept
    beta1 ~ dnorm(0, 1) # slope
    shape ~ dgamma(0.01, 0.01) # shape
  }

  init_values <- function(){ # specifies initial parameter values
    list(intercept = rnorm(1, mean = 0), beta1 = rnorm(1, mean = 0),
         shape = runif(1,1))
  }

  params <- c("intercept", "beta1", "shape")

  fit_lm1 <- jags(data = jagsdata_1, # specifies data
                  inits = init_values,
                  parameters.to.save = params,
                  model.file = lm1_jags,
                  n.chains = 3, # number of chains
                  n.iter = 2000,
                  n.burnin = 500) # only keeps every 10th iteration)
  fit_lm1

  plot(fit_lm1)

  traceplot(fit_lm1)

  lm_mcmc <- as.mcmc(fit_lm1)

  params <- c("intercept",
              "beta1", "beta2","beta3",
              "shape")

  fit_lm1 <- jags(data = jagsdata_1, # specifies data
                  inits = init_values,
                  parameters.to.save = params,
                  model.file = lm1_jags,
                  n.chains = 3, # number of chains
                  n.iter = 2000,
                  n.burnin = 500) # only keeps every 10th iteration)
  fit_lm1

  plot(fit_lm1)

  traceplot(fit_lm1)

  # dummy variable ############################################################
  #fake_data_a - simulate from https://seananderson.ca/2014/04/08/gamma-glms/
  set.seed(5555)
  N <- 100
  x <- runif(N, 0, 1)
  a <- 0.5 # intercept
  b1 <- 1.1 # beta 1
  site <- rbinom(N,1, 0.5)
  y_true <- exp(a + b1 * x)
  shape <- 10

  y <- rgamma(N, rate = shape / y_true, shape = shape)

  fake_data_a <- data.frame(x = x, y = y, site = site)

  ggplot(fake_data_a, aes(x = x, y = y))+
    geom_point()+
    ggtitle("regression")+
    coord_cartesian(ylim=c(0,7))

  # jags version of model
  jagsdata_1 <- with(fake_data_a, # creates a list of three elements
                     list(y = y,
                          x = x,
                          site = site,
                          N = length(fake_data_a$y)))

  lm1_jags <- function(){
    # Likelihood:
    for (i in 1:N){
      y[i] ~ dgamma(shape, shape / exp(mu[i]))
      mu[i] <- intercept + beta1 * x[i] + site
    }
    # Priors:
    intercept ~ dnorm(0, 2) # intercept
    beta1 ~ dnorm(0, 2) # slope
    shape ~ dgamma(0.01, 0.01) # shape
  }

  init_values <- function(){ # specifies initial parameter values
    list(intercept = runif(1), beta1 = runif(1),
         shape = rgamma(1,1))
  }

  params <- c("intercept", "beta1", "shape")

  fit_lm1 <- jags(data = jagsdata_1, # specifies data
                  inits = init_values,
                  parameters.to.save = params,
                  model.file = lm1_jags,
                  n.chains = 3, # number of chains
                  n.iter = 12000,
                  n.burnin = 0) # only keeps every 10th iteration)
  fit_lm1

  plot(fit_lm1)

  traceplot(fit_lm1)

  lm_mcmc <- as.mcmc(fit_lm1)

