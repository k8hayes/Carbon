# Biomass model - soil organic layer
library(tidyverse)
library(R2jags)
library(UsingR)
library(here)

data <- read.csv(here("data/output/biomass_variables.csv"))

data <- data %>%
  filter(TREAT != 0)

plot(log(Biomass_Kg_ha) ~ AV_DEPTH, data = data)


# Checking Distributions ########################

DensityPlot(data$Biomass_Kg_ha)
DensityPlot(data$AV_DEPTH)

qqPlot(data$Biomass_Kg_ha)
qqPlot(data$AV_DEPTH)

qqPlot(log(data$Biomass_Kg_ha))


# SOIL ORGANIC LAYER ###################################
## Linear model #######

glm_density <- lm(Biomass_Kg_ha ~ Count_HA + SITE, data = data)

summary(glm_density)
plot(Biomass_Kg_ha ~ Count_HA, data = data)
abline(glm_density, col = "red")

### Setting up JAGS ########
# defining data

y_biomass <- as.vector(data$Biomass_Kg_ha)
x_density <- as.vector(data$Count_HA)
x_site <- as.vector(data$SITE)
x_site <- as.factor(x_site)

jagsdata_density1 <- with(data, # creates a list of three elements
                          list(y_biomass = y_biomass,
                               x_density = x_density,
                               x_site = x_site,
                               N = length(data$Biomass_Kg_ha),
                               mu0 = 0, g0 = 0.0025,
                               mu1 = 0, g1 = 0.0025,
                               mu2 = 0, g2 = 0.0025,
                               a = 0.001, b = 0.001))

### model 1 - uninformative priors ################
jagsmod_density1 <- function(){
  # Likelihood:
  for (i in 1:N){
    y_biomass[i] ~ dnorm(beta0 + beta1*x_density[i] + beta2*x_site[i], invsigma2)
  }
  # Priors:
  beta0 ~ dnorm(mu0, g0) # intercept
  beta1 ~ dnorm(mu1, g1) #
  beta2 ~ dnorm(mu2, g2) #
  invsigma2 ~ dgamma(a, b)
}

# specifies initial parameter values
init_values_dens1 <- function(){
  list(beta0 = rnorm(1), beta1 = rnorm(1), beta2 = rnorm(1), invsigma2 = runif(1))
}

params_dens1 <- c("beta0", "beta1", "beta2", "invsigma2") # chooses the parameters for which to report posteriors

#### Run the model in JAGS#########################
fit_density1 <- jags(data = jagsdata_density1, # specifies data
                     inits = init_values_dens1, # specifies initial values
                     parameters.to.save = params_dens1,
                     model.file = jagsmod_density1,
                     n.chains = 3, # number of chains
                     n.iter = 12000, # number of iterations
                     n.burnin = 6000, # discards the first 2k values
                     n.thin = 10) # only keeps every 10th iteration

#### Diagnostics ############################

fit_density1 # model output
# DIC = 904.8
# biomass = 25.161 + 0.13(density) + -3.006(site)

traceplot(fit_density1, mfrow = c(2, 2), ask = F) # traceplots of all three chains

plot(fit_density1)

#### Prediction ###########

#   # create new sequence of predictor values
#   nvalues <- 42
#   data_new <- seq(min(data$Biomass_Kg_ha),
#                      max(data$Count_HA),
#                      length.out = nvalues)
#
#   # combine 3 chains into one mcmc object
#   lm1_mcmc_combi <- as.mcmc(rbind(lm1_mcmc[[1]], lm1_mcmc[[2]], lm1_mcmc[[3]]))
#
#   # calculate predicted biomass for new densities
#   pred_mean_mean <- mean(lm1_mcmc_combi[, "beta0"]) + data_new * mean(lm1_mcmc_combi[, "beta1"] +
#                                                                         data_new * mean(lm1_mcmc_combi[, "beta2"]))
#
#   # also want to show uncertainty around mean predicted value
#   # 2 forms of uncertainty
#   # uncertainty about true parameter values (1)
#   # uncertainty caused through stochastic relationship between biomass/density (2)
#
# # 1) credible uncertainty = credible interval around predicted biomass for given density
# pred_mean_dist <- matrix(NA, nrow = nrow(lm1_mcmc_combi), ncol = nvalues)
# # calculates 95% credible interval for each individual
# for (i in 1:nrow(pred_mean_dist)){
#   pred_mean_dist[i,] <- lm1_mcmc_combi[i,"beta0"] + data_new * lm1_mcmc_combi[i,"beta1"] + data_new * lm1_mcmc_combi[i, "beta2"]
# }
# credible_lower <- apply(pred_mean_dist, MARGIN = 2, quantile, prob = 0.025)
# credible_upper <- apply(pred_mean_dist, MARGIN = 2, quantile, prob = 0.975)
#
# # 2) drawing random values for normal distribution w/ mean defined by density, beta values + variance
# lm1_mcmc_combi_rep <- do.call(rbind, rep(list(lm1_mcmc_combi), 50)) # replication
#
# # Draw random values for all parameter combinations (rows) and density values (columns):
# pred_data_dist <- matrix(NA, nrow = nrow(lm1_mcmc_combi_rep), ncol = nvalues)
# for (i in 1:nrow(pred_data_dist)){
#   pred_data_dist[i,] <- lm1_mcmc_combi_rep[i,"beta0"] + data_new * lm1_mcmc_combi_rep[i,"beta1"] +
#     data_new * lm1_mcmc_combi_rep[i,"beta2"]
#     rnorm(nvalues, mean = 0, sd = lm1_mcmc_combi_rep[i, "invsigma2"])
# }
#
# # Calculate quantiles:
# uncertain_lower <- apply(pred_data_dist, MARGIN = 2, quantile, prob = 0.025)
# uncertain_upper <- apply(pred_data_dist, MARGIN = 2, quantile, prob = 0.975)
#
# ##### Plot Prediction #####
#
# dev.off()
#
# plot(Biomass_Kg_ha ~ Count_HA, data = data)
# points(data_new, col = "red")
# lines(data_new, credible_lower, lty = 2)
# lines(data_new, credible_upper, lty = 2)
# lines(data_new, uncertain_lower, lty = 2, col = "red")
# lines(data_new, uncertain_upper, lty = 2, col = "red")

##  Logged Linear model ##########################


y_biomass_log <- log(y_biomass)
x_density_log <- log(x_density)

glm_density2 <- lm(log(Biomass_Kg_ha) ~ log(Count_HA) + SITE, data = data)

summary(glm_density2)
jagsdata_density2 <- with(data, # creates a list of three elements
                          list(y_biomass_log = y_biomass_log,
                               x_density_log = x_density_log,
                               x_site = x_site,
                               N = length(data$Biomass_Kg_ha),
                               mu0 = 0, g0 = 0.0025,
                               mu1 = 0, g1 = 0.0025,
                               mu2 = 0, g2 = 0.0025,
                               a = 0.001, b = 0.001))


### model 1 - uninformative priors ################
jagsmod_density2 <- function(){
  # Likelihood:
  for (i in 1:N){
    y_biomass_log[i] ~ dnorm(beta0 + beta1*x_density_log[i] + beta2*x_site[i], invsigma2)
  }
  # Priors:
  beta0 ~ dnorm(mu0, g0) # intercept
  beta1 ~ dnorm(mu1, g1) #
  beta2 ~ dnorm(mu2, g2) #
  invsigma2 ~ dgamma(a, b)
}

# specifies initial parameter values
init_values_dens2 <- function(){
  list(beta0 = rnorm(1), beta1 = rnorm(1), beta2 = rnorm(1), invsigma2 = runif(1))
}

params_dens2 <- c("beta0", "beta1", "beta2", "invsigma2") # chooses the parameters for which to report posteriors

#### Run the model in JAGS#########################
fit_density2 <- jags(data = jagsdata_density2, # specifies data
                     inits = init_values_dens2, # specifies initial values
                     parameters.to.save = params_dens2,
                     model.file = jagsmod_density2,
                     n.chains = 3, # number of chains
                     n.iter = 12000, # number of iterations
                     n.burnin = 6000, # discards the first 2k values
                     n.thin = 10) # only keeps every 10th iteration


## ## Diagnostics ############################
#
fit_density2 # model output
# # DIC = 105.9
# log(biomass) = 10.513 + -0.129*log(density) + -0.078(site)

traceplot(fit_density2, mfrow = c(2, 2), ask = F) # traceplots of all three chains

plot(fit_density2)

#### Prediction ###########

# # create new sequence of predictor values
# nvalues <- 100
# data_new <- seq(min(log(data$Biomass_Kg_ha)),
#                 max(log(data$Count_HA)),
#                 length.out = nvalues)
#
# # combine 3 chains into one mcmc object
# lm1_mcmc_combi <- as.mcmc(rbind(lm1_mcmc[[1]], lm1_mcmc[[2]], lm1_mcmc[[3]]))
#
# # calculate predicted biomass for new densities
# pred_mean_mean <- mean(lm1_mcmc_combi[, "beta0"]) + data_new * mean(lm1_mcmc_combi[, "beta1"] +
#                                                                       data_new * mean(lm1_mcmc_combi[, "beta2"]))
# # also want to show uncertainty around mean predicted value
# # 2 forms of uncertainty
# # uncertainty about true parameter values (1)
# # uncertainty caused through stochastic relationship between biomass/density (2)
#
# # 1) credible uncertainty = credible interval around predicted biomass for given density
# pred_mean_dist <- matrix(NA, nrow = nrow(lm1_mcmc_combi), ncol = nvalues)
# # calculates 95% credible interval for each individual
# for (i in 1:nrow(pred_mean_dist)){
#   pred_mean_dist[i,] <- lm1_mcmc_combi[i,"beta0"] + data_new * lm1_mcmc_combi[i,"beta1"] + data_new * lm1_mcmc_combi[i, "beta2"]
# }
# credible_lower <- apply(pred_mean_dist, MARGIN = 2, quantile, prob = 0.025)
# credible_upper <- apply(pred_mean_dist, MARGIN = 2, quantile, prob = 0.975)
#
# # 2) drawing random values for normal distribution w/ mean defined by density, beta values + variance
# lm1_mcmc_combi_rep <- do.call(rbind, rep(list(lm1_mcmc_combi), 50)) # replication
#
# # Draw random values for all parameter combinations (rows) and density values (columns):
# pred_data_dist <- matrix(NA, nrow = nrow(lm1_mcmc_combi_rep), ncol = nvalues)
# for (i in 1:nrow(pred_data_dist)){
#   pred_data_dist[i,] <- lm1_mcmc_combi_rep[i,"beta0"] + data_new * lm1_mcmc_combi_rep[i,"beta1"] +
#     data_new * lm1_mcmc_combi_rep[i,"beta2"]
#   rnorm(nvalues, mean = 0, sd = lm1_mcmc_combi_rep[i, "invsigma2"])
# }
#
# # Calculate quantiles:
# uncertain_lower <- apply(pred_data_dist, MARGIN = 2, quantile, prob = 0.025)
# uncertain_upper <- apply(pred_data_dist, MARGIN = 2, quantile, prob = 0.975)
#
# ##### Plot Prediction ####################################
#
# dev.off()
#
# plot(log(Biomass_Kg_ha) ~ log(Count_HA), data = data)
# points(data_new, col = "red")
# lines(data_new, credible_lower, lty = 2)
# lines(data_new, credible_upper, lty = 2)
# lines(data_new, uncertain_lower, lty = 2, col = "red")
# lines(data_new, uncertain_upper, lty = 2, col = "red")

### model 2 - informative priors ################
jagsdata_density3 <- with(data, # creates a list of three elements
                          list(y_biomass_log = y_biomass_log,
                               x_density_log = x_density_log,
                               x_site = x_site,
                               N = length(data$Biomass_Kg_ha),
                               mu0 = 10.42, g0 = 1.17,
                               mu1 = -0.129, g1 = 0.10,
                               mu2 = -0.074, g2 = 0.258,
                               a = 0.001, b = 0.001))

#### Run the model in JAGS#########################
fit_density3 <- jags(data = jagsdata_density3, # specifies data
                     inits = init_values_dens2, # specifies initial values
                     parameters.to.save = params_dens2,
                     model.file = jagsmod_density2,
                     n.chains = 3, # number of chains
                     n.iter = 12000, # number of iterations
                     n.burnin = 6000, # discards the first 2k values
                     n.thin = 10) # only keeps every 10th iteration


## ## Diagnostics ############################
#
fit_density3 # model output
# # DIC = 104.4
# log(biomass) = 10.457 + -0.125*log(density) + -0.072(site)

traceplot(fit_density3, mfrow = c(2, 2), ask = F) # traceplots of all three chains

plot(fit_density3)
