# lognormal model with fire as primary variable
library(R2jags)
library(tidyverse)
library(cowplot)
library(here)
theme_set(theme_cowplot())

library(bayesplot)
#library(basicMCMCplots)

# START #####################################
data <- read.csv(here("data/output/biomass_variables.csv"))

data <- data %>%
  filter(TREAT != 0)

data <- na.omit(data)

# setting up biomass data
biomass_y <- as.numeric(data$Biomass_gm2)
nPlot <- 41

summary(biomass_y)

data$site <- ifelse(data$SITE == "DALTON", 1,2)
site <- data$site

# independent variables
fire_x1 <- as.factor(data$TREAT)

log.glm <- glm(log(Biomass_gm2) ~ data$TREAT, family = gaussian, data = data)

summary(log.glm)
# y = 7.7 + -0.35x


# LOGNORMAL - no random effects #####################################################
nCoef <- 2 # number of regression coefficients

jagsdata_1 <- with(data, # creates a list of elements
                   list(y = biomass_y, # y variable
                        nPlot = nPlot,
                        nCoef = nCoef,# number of plots
                        x1 = fire_x1))


mod1_jags <- function(){
  # Likelihood:
  for (i in 1:nPlot){
    y[i] ~ dlnorm(mu[i], tau) # tau is precision (1 / variance)
    mu[i] <- beta[1] + beta[2] * x1[i]

  }
  # Priors:
  for(i in 1:nCoef){
    beta[i] ~ dnorm(0, 0.01)
  }

  sigma ~ dgamma(0.01, 0.01) # sigma^2 doesn't work in JAGS
  # Need to have model calculate sigma = 1/tau
  tau <- 1/(sigma * sigma)
}

# initial values ############################
init_values1 <- function(){ # specifies initial parameter values
  list(beta = rnorm(nCoef),
       sigma = runif(1))
}

# parameters to track
params1 <- c( "beta","tau")

# Run the model in JAGS ###########################################3
fit_mod1 <- jags(data = jagsdata_1, # specifies data
                 inits = init_values1,
                 parameters.to.save = params1,
                 model.file = mod1_jags,
                 n.chains = 3, # number of chains
                 n.iter = 12000,
                 n.thin = 5, # only keeps every 5th iteration
                n.burnin = 0) # by default discards first 100
fit_mod1

plot(fit_mod1)

mod1_mcmc <- as.mcmc(fit_mod1)

chainsPlot(mod1_mcmc)

samplesPlot(mod1_mcmc, var = c("beta", "intercept"))

# RANDOM INTERCEPT #####################################################

site <- data$site
nSite <- 2

jagsdata_2 <- with(data, # creates a list of elements
                   list(y = biomass_y,
                        nCoef = nCoef,
                        nSite = nSite,
                        nPlot = nPlot,
                        site = site,
                        x1 = fire_x1
                       ))

mod2_jags <- function(){
  # Likelihood:
  for (i in 1:nPlot){
    y[i] ~ dlnorm(mu[i], tau) # tau is precision (1 / variance)
    mu[i] = intercept[site[i]] + beta[1] * x1[i]

  }
  # Priors:
  for(i in 1:nCoef){
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
  list(beta = rnorm(nCoef), intercept = rbinom(2,1,0.5),
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
                 n.iter = 12000,
                 n.burnin = 0) # only keeps every 10th iteration)
fit_mod2

plot(fit_mod1)

mod2_mcmc <- as.mcmc(fit_mod2)

samplesPlot(mod2_mcmc, var = c("beta", "intercept"))

chainsPlot(mod2_mcmc)

head(mod2_mcmc[[1]])

lm1_mcmc_combi <- as.mcmc(rbind(mod2_mcmc[[1]], mod2_mcmc[[2]], mod2_mcmc[[3]]))

plot(lm1_mcmc_combi[, "icc"])

# RANDOM SLOPE + INTERCEPT #####################################################

site <- data$site
nSite <- 2
nCoef <- 2 # number of regression coefficients

jagsdata_3 <- with(data, # creates a list of elements
                   list(y = biomass_y,
                        nSite = nSite,
                        site = site,
                        x1 = fire_x1,
                        N = length(data$Biomass_gm2)))

mod3_jags <- function(){
  # Likelihood:
  for (i in 1:N){
    y[i] ~ dlnorm(mu[i], tau) # tau is precision (1 / variance)
    mu[i] = intercept[1] + intercept[2]*site[i] + (beta[1] + beta[2]*site[i]) * x1[i]

  }
  # Priors:
  for(i in 1:nSite){
    intercept[i] ~ dnorm(0,0.01)
    beta[i] ~ dnorm(0, 0.01)
  }

  tau <- 1/(sigma * sigma)

  sigma ~ dgamma(0.01, 0.01)
}

# initial values ############################
init_values3 <- function(){ # specifies initial parameter values
  list(beta = rnorm(nCoef), intercept = rbinom(2,1,0.5),
       sigma = runif(1))
}

# parameters to track
params3 <- c("intercept", "beta","tau")

# Run the model in JAGS ###########################################3
fit_mod3 <- jags(data = jagsdata_3, # specifies data
                 inits = init_values3,
                 parameters.to.save = params3,
                 model.file = mod3_jags,
                 n.chains = 3, # number of chains
                 n.iter = 12000,

                 n.burnin = 0) # only keeps every 10th iteration)
fit_mod3

plot(fit_mod3)

mod3_mcmc <- as.mcmc(fit_mod3)
posterior <- as.matrix(fit_mod3)

#samplesPlot(mod3_mcmc, var = c("intercept"), traceplot = FALSE)

mcmc_areas(posterior,
           pars = c("beta", "intercept"),
           prob = 0.8)

#chainsPlot(mod3_mcmc)
mcmc_areas(mod3_mcmc, pars = c("beta[1]", "beta[2]"), prob = 0.8) +
  ggtitle("Posterior distributions",
          "with medians and 80% intervals")
mcmc_areas(mod3_mcmc, pars = c("intercept[1]", "intercept[2]"), prob = 0.8) +
  ggtitle("Posterior distributions",
          "with medians and 80% intervals")

# RANDOM EFFECTS + INTERCEPT #############
siteDens <- as.vector(scale(data$dens_gm2))
siteBA <- as.vector(scale(data$decidba_gm2))
siteSOL <- as.vector(scale(data$avSOLdepth_CM))
nCoef <- 2
nSiteCoef <- 3
site <- as.factor(site)
nSite <- 2

jagsdata_4 <- with(data, # creates a list of elements
                   list(y = biomass_y,
                        nSite = nSite,
                        nSiteCoef = nSiteCoef,
                        #nCoef = nCoef,
                        site = site,
                        x1 = fire_x1,
                        siteDens = siteDens,
                        siteBA = siteBA,
                        siteSOL = siteSOL,
                        nPlot = nPlot))

mod4_jags <- function(){
  # Likelihood - in JAGS, normal distribution is parameterized by
  # mean theta and precision = tau2 = 1/sig2
  for (i in 1:nPlot) {
    y[i] ~ dlnorm(muPlot[i], tau)
    muPlot[i] <- intercept[1] + intercept[2]*site[i] + (betaPlot[1] + betaPlot[2]*site[i]) * x1[i]
}
    for(j in 1:nSite){
    betaPlot[j] ~ dlnorm(muSite[j], tau.intercept )
    muSite[j] <- betaSite[1]*siteDens[j] + betaSite[2]*siteBA[j] + betaSite[3]*siteSOL[j]
  }

  # Priors
  for(i in 1:nSite) {
    intercept[i] ~ dnorm(0.01, 0.01)
    betaPlot[i] ~ dnorm(0, 0.01)
  }

  for(j in 1:nSiteCoef){
    betaSite[j] ~ dnorm(0, 0.01)
  }


  tau <- 1/(sigma * sigma)
  tau.intercept <- 1/(sigma.intercept * sigma.intercept)

  sigma.intercept ~ dgamma(0.01, 0.01)
  sigma ~ dgamma(0.01, 0.01)
}


# initial values ############################
init_values4 <- function(){ # specifies initial parameter values
  list(betaSite = rnorm(nSiteCoef),
       intercept = rbinom(nSite, 1, 0.5),
       sigma = runif(1),
       sigma.intercept = runif(1))
}

# parameters to track
params4 <- c("intercept", "betaSite","betaPlot", "tau")

# Run the model in JAGS ###########################################
fit_mod4 <- jags(data = jagsdata_4, # specifies data
                 inits = init_values4,
                 parameters.to.save = params4,
                 model.file = mod4_jags,
                 n.chains = 3, # number of chains
                 n.iter = 12000,
                 n.burnin = 0) # only keeps every 10th iteration)
fit_mod4

plot(fit_mod1)

mod2_mcmc <- as.mcmc(fit_mod2)

samplesPlot(mod2_mcmc, var = c("beta", "intercept"))

chainsPlot(mod2_mcmc)

head(mod2_mcmc[[1]])

lm1_mcmc_combi <- as.mcmc(rbind(mod2_mcmc[[1]], mod2_mcmc[[2]], mod2_mcmc[[3]]))

plot(lm1_mcmc_combi[, "icc"])
