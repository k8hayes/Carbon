# Biomass model

# input: output/biomass_variables.csv
# output:

# start #################################
library(tidyverse)
library(R2jags)
library(here)
library(bayesplot)

data <- read.csv(here("data/output/biomass_variables.csv"))

data <- data %>%
  filter(TREAT != 0)
dalton <- data %>%
  filter(SITE == "DALTON")

# model 1 - DALTON #########################################
## jags data ######################################

# defining variables
dalton <- na.omit(dalton)
biomass_y <- as.vector(dalton$Biomass_gm2)

# independent variables
density_x1 <- as.vector(scale(dalton$dens_gm2))
ba_x2 <- as.vector(scale(dalton$decidba_gm2))
SOL_x3 <- as.vector(scale(dalton$avSOLdepth_CM))


jagsdalton_1 <- with(data, # creates a list of elements
                   list(y = biomass_y,
                        x1 = density_x1,
                        x2 = ba_x2,
                        x3 = SOL_x3,
                        N = length(dalton$Biomass_gm2)))

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

dalton <- mcmc_areas(mod1_mcmc, pars = c("beta1", "beta2", "beta3"), prob = 0.8) +
  ggtitle("Posterior distributions",
          "with medians and 80% intervals")


# model 1 - gamma steese #########################################

## jags data ######################################

steese <- data %>%
  filter(SITE == "STEESE")

# defining variables
steese <- na.omit(steese)
biomass_y <- as.vector(steese$Biomass_gm2)

# independent variables
density_x1 <- as.vector(scale(steese$dens_gm2))
ba_x2 <- as.vector(scale(steese$decidba_gm2))
SOL_x3 <- as.vector(scale(steese$avSOLdepth_CM))


jagsdata_1 <- with(steese, # creates a list of elements
                   list(y = biomass_y,
                        x1 = density_x1,
                        x2 = ba_x2,
                        x3 = SOL_x3,
                        N = length(steese$Biomass_gm2)))


mod2_jags <- function(){
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
  list(intercept = rnorm(1), beta1 = rnorm(1), beta2 = rnorm(1), beta3 = rnorm(1),
       shape = runif(1))
}

# parameters to track
params <- c("intercept", "beta1","beta2","beta3", "shape")

# Run the model in JAGS ###########################################3
fit_mod1 <- jags(data = jagsdata_1, # specifies data
                 inits = init_values,
                 parameters.to.save = params,
                 model.file = mod2_jags,
                 n.chains = 3, # number of chains
                 n.iter = 2000,
                 n.burnin = 500) # only keeps every 10th iteration)
fit_mod1

plot(fit_mod1)

traceplot(fit_mod1)

mod1_mcmc <- as.mcmc(fit_mod1)

steese <- mcmc_areas(mod1_mcmc, pars = c( "beta1", "beta2", "beta3"), prob = 0.8) +
  ggtitle("Posterior distributions",
          "with medians and 80% intervals")
