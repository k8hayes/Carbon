# draft of lognormal model
library(R2jags)
library(tidyverse)
library(cowplot)
library(here)
theme_set(theme_cowplot())

library(bayesplot)

data <- read.csv(here("data/output/biomass_variables.csv"))

data <- data %>%
  filter(TREAT != 0)

treat_x1 <- as.vector(data$TREAT)

# setting up fake biomass data
biomass_y <- as.numeric(data$Biomass_gm2)
summary(biomass_y)
fake_biomass <- runif(N, 200, 3200)

test <- cbind(fake_biomass, treat_x1)
summary(lm(fake_biomass ~ treat_x1, data))
# y = 1975 + -129.9 x

summary(lm(log(fake_biomass) ~ treat_x1, data)) # logged
# y = 7.4 + -0.06x


jagsdata_1 <- with(data, # creates a list of elements
                   list(y = fake_biomass,
                        x1 = treat_x1,
                        N = length(fake_biomass)))

mod1_jags <- function(){
  # Likelihood:
  for (i in 1:N){
   y[i] ~ dlnorm(mu[i], tau) # tau is precision (1 / variance)
    mu[i] <- intercept + beta1 * x1[i]

  }
  # Priors:
  intercept ~ dnorm(7, 0.25) # intercept
  beta1 ~ dnorm(0, 0.11) # slope
  sigma ~ dunif(0, 100) # standard deviation
  tau <- 1 / (sigma * sigma) # sigma^2 doesn't work in JAGS
}


# initial values ############################
init_values <- function(){ # specifies initial parameter values
  list(intercept = rnorm(1), beta1 = rnorm(1),
       sigma = runif(1))
}

# parameters to track
params <- c("intercept", "beta1", "tau")

# Run the model in JAGS ###########################################3
fit_mod1 <- jags(data = jagsdata_1, # specifies data
                 inits = init_values,
                 parameters.to.save = params,
                 model.file = mod1_jags,
                 n.chains = 3, # number of chains
                 n.iter = 2000)
fit_mod1

traceplot(fit_mod1, mfrow = c(2, 2), ask = F)
