# soil model

# input: soil_variables.csv
# output:

# start #################################
library(tidyverse)
library(R2jags)
library(here)
library(bayesplot)

data <- read.csv(here("data/output/soil_variables.csv"))

data <- data %>%
  filter(TREAT != 0)

# model 1 #########################################
# jags data ######################################

# defining variables
soil_y <- as.vector(data$Soil_gm2)

# independent variables
density_x1 <- as.vector(data$dens_gm2)
ba_x2 <- as.vector(data$decidba_gm2)
SOL_x3 <- as.vector(data$avSOLdepth_CM)

jagsdata_1 <- with(data, # creates a list of elements
                   list(y = soil_y,
                        x1 = density_x1,
                        x2 = ba_x2,
                        x3 = SOL_x3,
                        N = length(soil_y)))

mod1_jags <- function(){
  # Likelihood:

  for (i in 1:N){
    y[i] ~ dgamma(shape, shape / exp(mu[i]))
    mu[i] <- intercept + beta1 * x1[i] + beta2 * x2[i] + beta3 * x3[i]
  }
  # Priors:
  intercept ~ dnorm(0, 2)
  beta1 ~ dnorm(0, 2) # density coeff (x1)
  beta2 ~ dnorm(0, 2) # BA coeff (x2)
  beta3 ~ dnorm(0, 2) # SOL coeff (x3)
  shape ~ dgamma(0.01, 0.01) # shape

}

# initial values ############################
init_values <- function(){ # specifies initial parameter values
  list(intercept = runif(1), beta1 = runif(1), beta2 = runif(1), beta3 = runif(1),
       shape = rgamma(1,1))
}

# parameters to track
params <- c("intercept", "beta1","beta2","beta3", "shape")

# Run the model in JAGS ###########################################3
fit_mod1 <- jags(data = jagsdata_1, # specifies data
                 inits = init_values,
                 parameters.to.save = params,
                 model.file = mod1_jags,
                 n.chains = 3, # number of chains
                 n.iter = 2000,
                 n.burnin = 500) # only keeps every 10th iteration)
fit_mod1

plot(fit_mod1)

traceplot(fit_mod1)

mod1_mcmc <- as.mcmc(fit_mod1)

mcmc_areas(mod1_mcmc, pars = c("beta1", "beta2", "beta3"), prob = 0.8) +
  ggtitle("Posterior distributions",
          "with medians and 80% intervals")

# model 3 - scaled #########################################
## jags data ######################################

# defining variables
data <- na.omit(data)
soil_y <- as.vector(data$Soil_gm2)

# independent variables
density_x1 <- as.vector(scale(data$dens_gm2))
ba_x2 <- as.vector(scale(data$decidba_gm2))
SOL_x3 <- as.vector(scale(data$avSOLdepth_CM))

jagsdata_1 <- with(data, # creates a list of elements
                   list(y = biomass_y,
                        x1 = density_x1,
                        x2 = ba_x2,
                        x3 = SOL_x3,
                        N = length(data$Biomass_gm2)))


mod1_jags <- function(){
  # Likelihood:

  for (i in 1:N){
    y[i] ~ dgamma(shape, shape / exp(mu[i]))
    mu[i] <- intercept + beta1 * x1[i] + beta2 * x2[i] + beta3 * x3[i]
  }
  # Priors:
  intercept ~ dnorm(0, 1)
  beta1 ~ dnorm(0, 1) # density coeff (x1)
  beta2 ~ dnorm(0, 1) # BA coeff (x2)
  beta3 ~ dnorm(0, 1) # SOL coeff (x3)
  shape ~ dgamma(0.01, 0.01) # shape

}

# initial values ############################
init_values <- function(){ # specifies initial parameter values
  list(intercept = rnorm(1, mean = 0), beta1 = rnorm(1, mean = 0), beta2 = rnorm(1, mean = 0), beta3 = rnorm(1, mean = 0),
       shape = rgamma(1,1))
}

# parameters to track
params <- c("intercept", "beta1","beta2","beta3", "shape")

# Run the model in JAGS ###########################################3
fit_mod1 <- jags(data = jagsdata_1, # specifies data
                 inits = init_values,
                 parameters.to.save = params,
                 model.file = mod1_jags,
                 n.chains = 3, # number of chains
                 n.iter = 2000,
                 n.burnin = 500) # only keeps every 10th iteration)
fit_mod1

plot(fit_mod1)

traceplot(fit_mod1)

mod1_mcmc <- as.mcmc(fit_mod1)

mcmc_areas(mod1_mcmc, pars = c("intercept", "beta1", "beta2", "beta3"), prob = 0.8) +
  ggtitle("Posterior distributions",
          "with medians and 80% intervals")

mcmc_areas(mod1_mcmc, pars = c("beta1", "beta2", "beta3"), prob = 0.8) +
  ggtitle("Posterior distributions - Soil Model",
          "with medians and 80% intervals")
soilplot <- mcmc_areas(mod1_mcmc, pars = c("beta1", "beta2", "beta3"), prob = 0.8) +
  ggtitle("Posterior distributions - Soil Model",
          "with medians and 80% intervals")

ppc_dens_overlay(y = mod1_mcmc[[1, y]],
                 yrep = posterior_predict(mod1_mcmc, draws = 50))
