
library(R2jags)
library(tidyverse)
library(cowplot)
library(here)
theme_set(theme_cowplot())

library(bayesplot)

data <- read.csv(here("data/output/biomass_variables.csv"))

data <- data %>%
  filter(TREAT != 0)

biomass_y <- as.numeric(data$Biomass_gm2)

treat_x1 <- as.vector(data$TREAT)

jagsdata_1 <- with(data, # creates a list of elements
                   list(y = biomass_y,
                        x1 = treat_x1,
                        N = length(data$Biomass_gm2)))

mod1_jags <- function(){
  # Likelihood:
  for (i in 1:N){
    y[i] ~ dnorm(mu[i], tau) # tau is precision (1 / variance)
    mu[i] <- intercept + beta1 * x1[i] + error

  }
  # Priors:
  intercept ~ dnorm(0, 0.01) # intercept
  beta1 ~ dnorm(0, 0.01) # slope
  sigma ~ dunif(0, 100) # standard deviation
  tau <- 1 / (sigma * sigma) # sigma^2 doesn't work in JAGS
  error ~ dgamma(0.01, 0.01)
}


# initial values ############################
init_values <- function(){ # specifies initial parameter values
  list(intercept = rnorm(1), beta1 = rnorm(1),
       sigma = runif(1))
}

# parameters to track
params <- c("intercept", "beta1", "sigma")

# Run the model in JAGS ###########################################3
fit_mod1 <- jags(data = jagsdata_1, # specifies data
                 inits = init_values,
                 parameters.to.save = params,
                 model.file = mod1_jags,
                 n.chains = 3, # number of chains
                 n.iter = 2000,
                 n.burnin = 0) # only keeps every 10th iteration)
fit_mod1

plot(fit_mod1)

traceplot(fit_mod1)

mod1_mcmc <- as.mcmc(fit_mod1)

mcmc_areas(mod1_mcmc, pars = c("beta1", "intercept"), prob = 0.8) +
  ggtitle("Posterior distributions",
          "with medians and 80% intervals")


###
mod1_jags <- function(){
  # Likelihood:

  for (i in 1:N){
    y[i] ~ dgamma(shape, shape / exp(mu[i]))
    mu[i] <- intercept + beta1 * x1[i]
  }
  # Priors:
  intercept ~ dnorm(0, 2)
  beta1 ~ dnorm(0, 2) # fire
  shape ~ dgamma(0.01, 0.01) # shape

}

# initial values ############################
init_values <- function(){ # specifies initial parameter values
  list(intercept = runif(1), beta1 = runif(1),
       shape = rgamma(1,1))
}

# parameters to track
params <- c("intercept", "beta1", "shape")

# Run the model in JAGS ###########################################3
fit_mod1 <- jags(data = jagsdata_1, # specifies data
                 inits = init_values,
                 parameters.to.save = params,
                 model.file = mod1_jags,
                 n.chains = 3, # number of chains
                 n.iter = 2000,
                 n.burnin = 0) # only keeps every 10th iteration)
fit_mod1

plot(fit_mod1)

traceplot(fit_mod1)

mod1_mcmc <- as.mcmc(fit_mod1)

mcmc_areas(mod1_mcmc, pars = c("beta1", "intercept"), prob = 0.8) +
  ggtitle("Posterior distributions",
          "with medians and 80% intervals")

