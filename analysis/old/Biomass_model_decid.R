# Biomass model - deciduous
library(tidyverse)
library(R2jags)
library(UsingR)
library(here)
library(car)

data <- read.csv(here("data/output/biomass_variables.csv"))

data <- data %>%
  filter(TREAT != 0)

plot(Biomass_Kg_ha ~ dBA_HA, data = data)

# Checking Distributions ########################

DensityPlot(data$Biomass_Kg_ha)
DensityPlot(data$dBA_HA)

car::qqPlot(data$Biomass_Kg_ha)

car::qqPlot(log(data$Biomass_Kg_ha))

car::qqPlot(data$dBA_HA)

# BIOMASS ######################
##### Linear model #######

glm_decid <- lm(Biomass_Kg_ha ~ dBA_HA, data = data)

summary(glm_decid) # y = 8030.8 + 1240.9(decid) + -367.5(site)
plot(Biomass_Kg_ha ~ dBA_HA, data = data)
abline(glm_decid, col = "red")
plot(glm_decid)


##### Setting up ################
# defining data
data <- data %>%
  dplyr::mutate(SITE = recode(SITE, 'DALTON' = 0, 'STEESE' = 1))

x_dens <- as.vector(data$Count_HA)
y_biomass <- as.vector(data$Biomass_Kg_ha)

x_ba <- as.vector(data$dBA_HA)
x_sol <- as.vector(data$AV_DEPTH)

x_site <- as.vector(data$SITE)
x_site <- as.factor(x_site)

jagsdata_decid1 <- with(data, # creates a list of three elements
                        list(y_biomass = y_biomass,
                             x_dens = x_dens,
                             x_ba = x_ba,
                             x_sol = x_sol,
                             N = length(data$Biomass_Kg_ha),
                             mu0 = 0, g0 = 0.0025,
                             mu1 = 0, g1 = 0.0025,
                             mu2 = 0, g2 = 0.0025,
                             mu3 = 0, g3 = 0.0025,
                             mu4 = 0, g4 = 0.0025,
                             a = 0.001, b = 0.001))

#### model 1 - uninformative priors ################
jagsmod_1 <- function(){
  # Likelihood:
  for (i in 1:N){
    y_biomass[i] ~ dgamma(#x, #shape, #rate)
  }
  # Priors:
  beta0 ~ dnorm(mu0, g0) # intercept
  beta1 ~ dnorm(mu1, g1) #
  beta2 ~ dnorm(mu2, g2) #
  beta3 ~ dnorm(mu3, g3)
  invsigma2 ~ dgamma(a, b)
}

# specifies initial parameter values
init_values_decid1 <- function(){
  list(beta0 = rnorm(1), beta1 = rnorm(1), beta2 = rnorm(1), beta3 = rnorm(1), beta4 = rnorm(1),
       invsigma2 = runif(1))
}

params_decid1 <- c("beta0", "beta1", "beta2", "beta3", "beta4", "invsigma2") # chooses the parameters for which to report posteriors

#### Run the model in JAGS#########################
fit_decid1 <- jags(data = jagsdata_decid1, # specifies data
                   inits = init_values_decid1, # specifies initial values
                   parameters.to.save = params_decid1,
                   model.file = jagsmod_decid1,
                   n.chains = 3, # number of chains
                   n.iter = 12000, # number of iterations
                   n.burnin = 6000, # discards the first 2k values
                   n.thin = 10) # only keeps every 10th iteration

#### Diagnostics ############################

fit_decid1 # model output
# DIC = 901.8

traceplot(fit_decid1, mfrow = c(2, 2), ask = F) # traceplots of all three chains

plot(fit_decid1)


# #### Prediction ###########
#
# # create new sequence of predictor values
# nvalues <- 42
# data_new <- seq(min(data$Biomass_Kg_ha),
#                 max(data$dBA_HA),
#                 length.out = nvalues)
#
# # combine 3 chains into one mcmc object
# lm1_mcmc_combi <- as.mcmc(rbind(lm1_mcmc[[1]], lm1_mcmc[[2]], lm1_mcmc[[3]]))
#
# # calculate predicted biomass for new densities
# pred_mean_mean <- mean(lm1_mcmc_combi[, "beta0"]) + data_new * mean(lm1_mcmc_combi[, "beta1"] +
#                                                                       data_new * mean(lm1_mcmc_combi[, "beta2"]))
#
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
# ### Plot  prediction #####
#
# dev.off()
#
# plot(Biomass_Kg_ha ~ dBA_HA, data = data)
# points(data_new, col = "red")
# lines(data_new, credible_lower, lty = 2)
# lines(data_new, credible_upper, lty = 2)
# lines(data_new, uncertain_lower, lty = 2, col = "red")
# lines(data_new, uncertain_upper, lty = 2, col = "red")


## Logged Linear model ######################
glm_decid_log <- glm(log(Biomass_Kg_ha) ~ dBA_HA + SITE, data = data)
summary(glm_decid_log)


jagsdata_decid2 <- with(data, # creates a list of three elements
                        list(y_biomass_log = y_biomass_log,
                             x_dens = x_dens,
                             x_site = x_site,
                             N = length(data$Biomass_Kg_ha),
                             mu0 = 0, g0 = 0.0025,
                             mu1 = 0, g1 = 0.0025,
                             mu2 = 0, g2 = 0.0025,
                             a = 0.001, b = 0.001))

#### model 1 - uninformative priors ################
jagsmod_decid2 <- function(){
  # Likelihood:
  for (i in 1:N){
    y_biomass_log[i] ~ dnorm(beta0 + beta1*x_dens[i] + beta2*x_site[i], invsigma2)
  }
  # Priors:
  beta0 ~ dnorm(mu0, g0) # intercept
  beta1 ~ dnorm(mu1, g1) #
  beta2 ~ dnorm(mu2, g2) #
  invsigma2 ~ dgamma(a, b)
}

# specifies initial parameter values
init_values_decid2 <- function(){
  list(beta0 = rnorm(1), beta1 = rnorm(1), beta2 = rnorm(1), invsigma2 = runif(1))
}

params_decid2 <- c("beta0", "beta1", "beta2", "invsigma2") # chooses the parameters for which to report posteriors

#### Run the model in JAGS#########################
fit_decid2 <- jags(data = jagsdata_decid2, # specifies data
                   inits = init_values_decid2, # specifies initial values
                   parameters.to.save = params_decid2,
                   model.file = jagsmod_decid2,
                   n.chains = 3, # number of chains
                   n.iter = 12000, # number of iterations
                   n.burnin = 6000, # discards the first 2k values
                   n.thin = 10) # only keeps every 10th iteration

#### Diagnostics ############################

fit_decid2 # model output
# DIC = 94.9
# log(biomass) = 10.279 + -0.014(decid) + -0.225(site)

traceplot(fit_decid2, mfrow = c(2, 2), ask = F) # traceplots of all three chains

plot(fit_decid2)

#### Prediction ###########

# create new sequence of predictor values
nvalues <- 100
data_new <- seq(min(data$Biomass_Kg_ha),
                max(data$dBA_HA),
                length.out = nvalues)

# combine 3 chains into one mcmc object
lm1_mcmc_combi <- as.mcmc(rbind(lm1_mcmc[[1]], lm1_mcmc[[2]], lm1_mcmc[[3]]))

# calculate predicted biomass for new densities
pred_mean_mean <- mean(lm1_mcmc_combi[, "beta0"]) + data_new * mean(lm1_mcmc_combi[, "beta1"] +
                                                                      data_new * mean(lm1_mcmc_combi[, "beta2"]))

# also want to show uncertainty around mean predicted value
# 2 forms of uncertainty
# uncertainty about true parameter values (1)
# uncertainty caused through stochastic relationship between biomass/density (2)

# 1) credible uncertainty = credible interval around predicted biomass for given density
pred_mean_dist <- matrix(NA, nrow = nrow(lm1_mcmc_combi), ncol = nvalues)
# calculates 95% credible interval for each individual
for (i in 1:nrow(pred_mean_dist)){
  pred_mean_dist[i,] <- lm1_mcmc_combi[i,"beta0"] + data_new * lm1_mcmc_combi[i,"beta1"] + data_new * lm1_mcmc_combi[i, "beta2"]
}
credible_lower <- apply(pred_mean_dist, MARGIN = 2, quantile, prob = 0.025)
credible_upper <- apply(pred_mean_dist, MARGIN = 2, quantile, prob = 0.975)

# 2) drawing random values for normal distribution w/ mean defined by density, beta values + variance
lm1_mcmc_combi_rep <- do.call(rbind, rep(list(lm1_mcmc_combi), 50)) # replication

# Draw random values for all parameter combinations (rows) and density values (columns):
pred_data_dist <- matrix(NA, nrow = nrow(lm1_mcmc_combi_rep), ncol = nvalues)
for (i in 1:nrow(pred_data_dist)){
  pred_data_dist[i,] <- lm1_mcmc_combi_rep[i,"beta0"] + data_new * lm1_mcmc_combi_rep[i,"beta1"] +
    data_new * lm1_mcmc_combi_rep[i,"beta2"]
  rnorm(nvalues, mean = 0, sd = lm1_mcmc_combi_rep[i, "invsigma2"])
}

# Calculate quantiles:
uncertain_lower <- apply(pred_data_dist, MARGIN = 2, quantile, prob = 0.025)
uncertain_upper <- apply(pred_data_dist, MARGIN = 2, quantile, prob = 0.975)

### Plot  prediction #####

dev.off()

plot(Biomass_Kg_ha ~ dBA_HA, data = data)
points(data_new, col = "red")
lines(data_new, credible_lower, lty = 2)
lines(data_new, credible_upper, lty = 2)
lines(data_new, uncertain_lower, lty = 2, col = "red")
lines(data_new, uncertain_upper, lty = 2, col = "red")
