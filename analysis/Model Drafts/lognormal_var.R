# log normal with variables
#https://www.y1zhou.com/series/bayesian-stat/bayesian-stat-bayesian-linear-regression/
library(R2jags)
library(tidyverse)
library(cowplot)
library(here)
theme_set(theme_cowplot())

library(bayesplot)

# START #######################
data <- read.csv(here("data/output/biomass_variables.csv"))

data <- data %>%
  filter(TREAT != 0) %>%
  filter(SITE == "DALTON")

data <- na.omit(data)

# setting up fake biomass data
biomass_y <- as.numeric(data$Biomass_gm2)
nPlot <- as.numeric(length(unique(data$PLOT)))



summary(biomass_y)

# data$site <- ifelse(data$SITE == "DALTON", 1,2)

J <- 4 # number of regression coefficients
#site <- data$site

# independent variables
density_x1 <- as.vector(scale(data$dens_gm2))
ba_x2 <- as.vector(scale(data$decidba_gm2))
SOL_x3 <- as.vector(scale(data$avSOLdepth_CM))

log.glm <- glm(log(Biomass_gm2) ~ scale(dens_gm2) + scale(decidba_gm2) + scale(avSOLdepth_CM), family = gaussian, data = data)

summary(log.glm)
# y = 7.06 + -0.04x1 + 0.16x2 + 0.37x3

# LOGNORMAL - no random effects #####################################################

# DALTON ###################################
jagsdata_1 <- with(data, # creates a list of elements
                   list(y = biomass_y,
                        J = J,
                        x1 = density_x1,
                        x2 = ba_x2,
                        x3 = SOL_x3,
                        N = length(data$Biomass_gm2)))

mod1_jags <- function(){
  # Likelihood:
  for (i in 1:N){
    y[i] ~ dlnorm(mu[i], tau) # tau is precision (1 / variance)
    mu[i] <- beta[1] + beta[2] * x1[i] + beta[3] * x2[i] + beta[4] * x3[i]

  }
  # Priors:
  for(i in 1:J){
    beta[i] ~ dnorm(0, 0.01)
  }

  sigma ~ dgamma(0.01, 0.01) # sigma^2 doesn't work in JAGS
  # Need to have model calculate sigma = 1/tau
  tau <- 1/(sigma * sigma)
}

# initial values ############################
init_values <- function(){ # specifies initial parameter values
  list(beta = rnorm(J),
       sigma = runif(1))
}

# parameters to track
params <- c( "beta","tau")

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
mod1_mcmc <- as.mcmc(fit_mod1)

mod1_mcmc_combi <- as.mcmc(rbind(mod1_mcmc[[1]], mod1_mcmc[[2]], mod1_mcmc[[3]]))

mcmc_areas(mod1_mcmc_combi, pars = c("beta[2]", "beta[3]", "beta[4]"), prob = 0.8) +
  ggtitle("Posterior distributions",
          "with medians and 80% intervals")

mcmc_gg <- ggs(as.mcmc(mod1_mcmc_combi))
ggs_caterpillar(mcmc_gg, family = "beta")

# STEESE ###################################
data <- read.csv(here("data/output/biomass_variables.csv"))

data <- data %>%
  filter(TREAT != 0) %>%
  filter(SITE == "STEESE")

data <- na.omit(data)

# setting up fake biomass data
biomass_y <- as.numeric(data$Biomass_gm2)
nPlot <- as.numeric(length(unique(data$PLOT)))



summary(biomass_y)

# data$site <- ifelse(data$SITE == "DALTON", 1,2)

J <- 4 # number of regression coefficients
#site <- data$site

# independent variables
density_x1 <- as.vector(scale(data$dens_gm2))
ba_x2 <- as.vector(scale(data$decidba_gm2))
SOL_x3 <- as.vector(scale(data$avSOLdepth_CM))
jagsdata_1 <- with(data, # creates a list of elements
                   list(y = biomass_y,
                        J = J,
                        x1 = density_x1,
                        x2 = ba_x2,
                        x3 = SOL_x3,
                        N = length(data$Biomass_gm2)))

mod1_jags <- function(){
  # Likelihood:
  for (i in 1:N){
    y[i] ~ dlnorm(mu[i], tau) # tau is precision (1 / variance)
    mu[i] <- beta[1] + beta[2] * x1[i] + beta[3] * x2[i] + beta[4] * x3[i]

  }
  # Priors:
  for(i in 1:J){
    beta[i] ~ dnorm(0, 0.01)
  }

  sigma ~ dgamma(0.01, 0.01) # sigma^2 doesn't work in JAGS
  # Need to have model calculate sigma = 1/tau
  tau <- 1/(sigma * sigma)
}

# initial values ############################
init_values <- function(){ # specifies initial parameter values
  list(beta = rnorm(J),
       sigma = runif(1))
}

# parameters to track
params <- c( "beta","tau")

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
mod1_mcmc <- as.mcmc(fit_mod1)


mcmc_areas(mod1_mcmc, pars = c("beta[2]", "beta[3]", "beta[4]"), prob = 0.8) +
  ggtitle("Posterior distributions",
          "with medians and 80% intervals")
# RANDOM INTERCEPT #####################################################

site <- data$site
nSite <- 2
J <- 3 # number of regression coefficients

jagsdata_2 <- with(data, # creates a list of elements
                   list(y = fake_biomass,
                        J = J,
                        nSite = nSite,
                        site = site,
                        x1 = density_x1,
                        x2 = ba_x2,
                        x3 = SOL_x3,
                        N = length(data$Biomass_gm2)))

mod2_jags <- function(){
  # Likelihood:
  for (i in 1:N){
    y[i] ~ dlnorm(mu[i], tau) # tau is precision (1 / variance)
    mu[i] = intercept[site[i]]  + beta[1] * x1[i] + beta[2] * x2[i] + beta[3] * x3[i]

  }
  # Priors:
  for(i in 1:J){
    beta[i] ~ dnorm(0, 0.01)
  }

  for(j in 1:nSite) {
    intercept[j] ~ dnorm(0, tau.intercept)
  }

tau <- 1/(sigma * sigma)
tau.intercept <- 1/(sigma.intercept * sigma.intercept)

sigma.intercept ~ dgamma(0.01, 0.01)
sigma ~ dgamma(0.01, 0.01)

  # Perhaps we also want the intraclass correlation
  icc = sigma.intercept/(sigma+sigma.intercept)

}

# initial values ############################
init_values2 <- function(){ # specifies initial parameter values
  list(beta = rnorm(J), intercept = rbinom(2,1,0.5),
       sigma = runif(1), sigma.intercept = runif(1))
}

# parameters to track
params2 <- c("intercept", "beta","tau", "icc")

# Run the model in JAGS ###########################################3
fit_mod2 <- jags(data = jagsdata_2, # specifies data
                 inits = init_values2,
                 parameters.to.save = params2,
                 model.file = mod2_jags,
                 n.chains = 3, # number of chains
                 n.iter = 2000,
                 n.burnin = 0) # only keeps every 10th iteration)
fit_mod2

plot(fit_mod1)

mod2_mcmc <- as.mcmc(fit_mod2)

head(mod2_mcmc[[1]])

lm1_mcmc_combi <- as.mcmc(rbind(mod2_mcmc[[1]], mod2_mcmc[[2]], mod2_mcmc[[3]]))

plot(lm1_mcmc_combi[, "icc"])

chainsPlot(mod2_mcmc)
samplesPlot(mod2_mcmc, var = c("beta", "intercept"))


# RANDOM SLOPE ################################################
# haven't figured out yet

nSite <- 2

jagsdata_3 <- with(data, # creates a list of elements
                   list(y = biomass_y, # y variable
                        J = J, # number of regression coeff
                        nSite = nSite, # number of sites
                        site = site, # 0/1 designations
                        x1 = density_x1,
                        x2 = ba_x2,
                        x3 = SOL_x3,
                        N = length(data$Biomass_gm2)))

mod3_jags <- function(){
  # Likelihood:
  for (i in 1:N){
    y[i] ~ dlnorm(mu[i], tau) # tau is precision (1 / variance)
    mu[i] = intercept + beta[site[1]] * x1[i] + beta[site[2]] * x2[i] + beta[site[3]] * x3[i]

  }

  for(j in 1:nSite){
    beta[j] ~ dnorm(muSite[j], tau.slope)
    muSite[j] = betaSite[1]*x1[j]+betaSite[2]*x2[j]+betaSite[3]*x3[j]
  }

  # Priors:
  for(i in 1:J){
    beta[i] ~ dnorm(0, 0.01)
  }

  for(i in 1:nSite){
    betaSite ~ dnorm(0,0.01)
  }

  intercept ~ dnorm(0, 0.01)
  tau <- 1/(sigma * sigma)
  tau.slope <- 1/(sigma.slope * sigma.slope)

  sigma.slope ~ dgamma(0.01, 0.01)
  sigma ~ dgamma(0.01, 0.01)
}

# initial values ############################
init_values3 <- function(){ # specifies initial parameter values
  list(beta = rbinom(J, 1, 0.5), intercept = runif(1),
       sigma = runif(1), sigma.slope = runif(1))
}

# parameters to track
params3 <- c("intercept", "beta","tau")

# Run the model in JAGS ###########################################3
fit_mod3 <- jags(data = jagsdata_3, # specifies data
                 inits = init_values3,
                 parameters.to.save = params3,
                 model.file = mod3_jags,
                 n.chains = 3, # number of chains
                 n.iter = 2000,
                 n.burnin = 0) # only keeps every 10th iteration)
fit_mod3

plot(fit_mod1)

mod2_mcmc <- as.mcmc(fit_mod2)

head(mod2_mcmc[[1]])

lm1_mcmc_combi <- as.mcmc(rbind(mod2_mcmc[[1]], mod2_mcmc[[2]], mod2_mcmc[[3]]))

plot(lm1_mcmc_combi[, "icc"])

chainsPlot(mod2_mcmc)
samplesPlot(mod2_mcmc, var = c("beta", "intercept"))
