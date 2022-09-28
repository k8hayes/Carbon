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

# model 1 #########################################
# jags data ######################################

  # defining variables
    biomass_y <- as.vector(data$Biomass_gm2)

    # independent variables
    density_x1 <- as.vector(data$dens_gm2)
    ba_x2 <- as.vector(data$decidba_gm2)
    SOL_x3 <- as.vector(data$avSOLdepth_CM)

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
                n.burnin = 0) # only keeps every 10th iteration)
fit_mod1

plot(fit_mod1)

traceplot(fit_mod1)

mod1_mcmc <- as.mcmc(fit_mod1)

mcmc_areas(mod1_mcmc, pars = c("beta1", "beta2", "beta3"), prob = 0.8) +
  ggtitle("Posterior distributions",
          "with medians and 80% intervals")

# ppc_dens_overlay(y = mod1_mcmc[[1, y]],
#                  yrep = posterior_predict(mod1_mcmc, draws = 50))

# # predictions
# nvalues <- 100
# x_new <- seq(min(data_a$x), max(fake_data_a$x),
#              length.out = nvalues)
# z_new <- seq(min(fake_data_a$z), max(fake_data_a$z),
#              length.out = nvalues)
#
# # combine 2 chains into one mcmc object
# lm1_mcmc_combi <- as.mcmc(rbind(lm_mcmc[[1]], lm_mcmc[[2]]))
#
# # calculate predicted son heights for new father heights
# pred_mean_mean <- mean(lm1_mcmc_combi[, "beta0"]) + x_new * mean(lm1_mcmc_combi[, "beta1"]) + z_new * mean(lm1_mcmc_combi[, "beta2"])
#
# # also want to show uncertainty around mean predicted value
# # 2 forms of uncertainty
# # uncertainty about true parameter values (1)
# # uncertainty caused through stochastic relationship between father/son heights (2)
#
# # 1) credible uncertainty = credible interval around predicted sheight for given fheight
# pred_mean_dist <- matrix(NA, nrow = nrow(lm1_mcmc_combi), ncol = nvalues)
# # calculates 95% credible interval for each individual (n = 1078)
# for (i in 1:nrow(pred_mean_dist)){
#   pred_mean_dist[i,] <- lm1_mcmc_combi[i,"beta0"] + x_new * lm1_mcmc_combi[i,"beta1"] + z_new * lm1_mcmc_combi[i,"beta2"]
# }
# credible_lower <- apply(pred_mean_dist, MARGIN = 2, quantile, prob = 0.025)
# credible_upper <- apply(pred_mean_dist, MARGIN = 2, quantile, prob = 0.975)
#
# # 2) drawing random values for normal distribution w/ mean defined by fheight, alpha/beta + variance
# lm1_mcmc_combi_rep <- do.call(rbind, rep(list(lm1_mcmc_combi), 50)) # replication
#
# # Draw random values for all parameter combinations (rows) and body length values (columns):
# pred_data_dist <- matrix(NA, nrow = nrow(lm1_mcmc_combi_rep), ncol = nvalues)
# for (i in 1:nrow(pred_data_dist)){
#   pred_data_dist[i,] <- lm1_mcmc_combi_rep[i,"beta0"] + x_new * lm1_mcmc_combi_rep[i,"beta1"] + z_new * lm1_mcmc_combi_rep[i,"beta2"]
#   rnorm(nvalues, mean = 0, sd = lm1_mcmc_combi_rep[i, "shape"])
# }
#
# # Calculate quantiles:
# uncertain_lower <- apply(pred_data_dist, MARGIN = 2, quantile, prob = 0.025)
# uncertain_upper <- apply(pred_data_dist, MARGIN = 2, quantile, prob = 0.975)
#
#
# ### Plot of mean model prediction + 2 uncertainty measures #####
#
# dev.off()
#
# lm <- glm(y ~ x + z, data = fake_data_a, family = Gamma(link = "log"))
# summary(lm)
# # estimates - beta0  0.31, x = 1.1, z = 0.02
#
# plot(y ~ x, data = fake_data_a)
# lines(x_new, pred_mean_mean)
# lines(x_new, credible_lower, lty = 2)
# lines(x_new, credible_upper, lty = 2)
# lines(x_new, uncertain_lower, lty = 2, col = "red")
# lines(x_new, uncertain_upper, lty = 2, col = "red")

# # model 2 - 100m2 #########################################
# ## jags data ######################################
#
# # defining variables
# biomass_y <- as.vector(data$Biomass_gm2 * 100)
#
# # independent variables
# density_x1 <- as.vector(data$dens_gm2 * 100)
# ba_x2 <- as.vector(data$decidba_gm2 * 100)
# SOL_x3 <- as.vector(data$avSOLdepth_CM)
#
# jagsdata_1 <- with(data, # creates a list of elements
#                    list(y = biomass_y,
#                         x1 = density_x1,
#                         x2 = ba_x2,
#                         x3 = SOL_x3,
#                         N = length(data$Biomass_gm2)))
#
#
# mod1_jags <- function(){
#   # Likelihood:
#
#   for (i in 1:N){
#     y[i] ~ dgamma(shape, shape / exp(mu[i]))
#     mu[i] <- intercept + beta1 * x1[i] + beta2 * x2[i] + beta3 * x3[i]
#   }
#   # Priors:
#   intercept ~ dnorm(0, 2)
#   beta1 ~ dnorm(0, 2) # density coeff (x1)
#   beta2 ~ dnorm(0, 2) # BA coeff (x2)
#   beta3 ~ dnorm(0, 2) # SOL coeff (x3)
#   shape ~ dgamma(0.01, 0.01) # shape
#
# }
#
# # initial values ############################
# init_values <- function(){ # specifies initial parameter values
#   list(intercept = runif(1), beta1 = runif(1), beta2 = runif(1), beta3 = runif(1),
#        shape = rgamma(1,1))
# }
#
# # parameters to track
# params <- c("intercept", "beta1","beta2","beta3", "shape")
#
# # Run the model in JAGS ###########################################3
# fit_mod1 <- jags(data = jagsdata_1, # specifies data
#                  inits = init_values,
#                  parameters.to.save = params,
#                  model.file = mod1_jags,
#                  n.chains = 3, # number of chains
#                  n.iter = 2000,
#                  n.burnin = 500) # only keeps every 10th iteration)
# fit_mod1
#
# plot(fit_mod1)
#
# traceplot(fit_mod1)
#
# mod1_mcmc <- as.mcmc(fit_mod1)
#
# mcmc_areas(mod1_mcmc, pars = c("intercept", "beta1", "beta2", "beta3"), prob = 0.8) +
#   ggtitle("Posterior distributions",
#           "with medians and 80% intervals")
#
# ppc_dens_overlay(y = mod1_mcmc[[1, y]],
#                  yrep = posterior_predict(mod1_mcmc, draws = 50))
#
# # # predictions
# # nvalues <- 100
# # x_new <- seq(min(data_a$x), max(fake_data_a$x),
# #              length.out = nvalues)
# # z_new <- seq(min(fake_data_a$z), max(fake_data_a$z),
# #              length.out = nvalues)
# #
# # # combine 2 chains into one mcmc object
# # lm1_mcmc_combi <- as.mcmc(rbind(lm_mcmc[[1]], lm_mcmc[[2]]))
# #
# # # calculate predicted son heights for new father heights
# # pred_mean_mean <- mean(lm1_mcmc_combi[, "beta0"]) + x_new * mean(lm1_mcmc_combi[, "beta1"]) + z_new * mean(lm1_mcmc_combi[, "beta2"])
# #
# # # also want to show uncertainty around mean predicted value
# # # 2 forms of uncertainty
# # # uncertainty about true parameter values (1)
# # # uncertainty caused through stochastic relationship between father/son heights (2)
# #
# # # 1) credible uncertainty = credible interval around predicted sheight for given fheight
# # pred_mean_dist <- matrix(NA, nrow = nrow(lm1_mcmc_combi), ncol = nvalues)
# # # calculates 95% credible interval for each individual (n = 1078)
# # for (i in 1:nrow(pred_mean_dist)){
# #   pred_mean_dist[i,] <- lm1_mcmc_combi[i,"beta0"] + x_new * lm1_mcmc_combi[i,"beta1"] + z_new * lm1_mcmc_combi[i,"beta2"]
# # }
# # credible_lower <- apply(pred_mean_dist, MARGIN = 2, quantile, prob = 0.025)
# # credible_upper <- apply(pred_mean_dist, MARGIN = 2, quantile, prob = 0.975)
# #
# # # 2) drawing random values for normal distribution w/ mean defined by fheight, alpha/beta + variance
# # lm1_mcmc_combi_rep <- do.call(rbind, rep(list(lm1_mcmc_combi), 50)) # replication
# #
# # # Draw random values for all parameter combinations (rows) and body length values (columns):
# # pred_data_dist <- matrix(NA, nrow = nrow(lm1_mcmc_combi_rep), ncol = nvalues)
# # for (i in 1:nrow(pred_data_dist)){
# #   pred_data_dist[i,] <- lm1_mcmc_combi_rep[i,"beta0"] + x_new * lm1_mcmc_combi_rep[i,"beta1"] + z_new * lm1_mcmc_combi_rep[i,"beta2"]
# #   rnorm(nvalues, mean = 0, sd = lm1_mcmc_combi_rep[i, "shape"])
# # }
# #
# # # Calculate quantiles:
# # uncertain_lower <- apply(pred_data_dist, MARGIN = 2, quantile, prob = 0.025)
# # uncertain_upper <- apply(pred_data_dist, MARGIN = 2, quantile, prob = 0.975)
# #
# #
# # ## Plot of mean model prediction + 2 uncertainty measures #####
# #
# # dev.off()
# #
# # lm <- glm(y ~ x + z, data = fake_data_a, family = Gamma(link = "log"))
# # summary(lm)
# # # estimates - beta0  0.31, x = 1.1, z = 0.02
# #
# # plot(y ~ x, data = fake_data_a)
# # lines(x_new, pred_mean_mean)
# # lines(x_new, credible_lower, lty = 2)
# # lines(x_new, credible_upper, lty = 2)
# # lines(x_new, uncertain_lower, lty = 2, col = "red")
# # lines(x_new, uncertain_upper, lty = 2, col = "red")

# model 3 - scaled #########################################
## jags data ######################################

# defining variables
data <- na.omit(data)
biomass_y <- as.vector(data$Biomass_gm2)

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

biomassplot <- mcmc_areas(mod1_mcmc, pars = c("beta1", "beta2", "beta3"), prob = 0.8) +
  ggtitle("Posterior distributions - Biomass model",
          "with medians and 80% intervals")

# only works if soil plot is saved in workspace
plot_grid(biomassplot, soilplot, labels = c("A.", "B.")) # 900 x 300



ppc_dens_overlay(y = mod1_mcmc[[1, y]],
                 yrep = posterior_predict(mod1_mcmc, draws = 50))

# model 4 - scaled + uncertainty #########################################
## jags data ######################################

# defining variables
data <- na.omit(data)
biomass_y <- as.vector(data$Biomass_gm2)

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

biomassplot <- mcmc_areas(mod1_mcmc, pars = c("beta1", "beta2", "beta3"), prob = 0.8) +
  ggtitle("Posterior distributions - Biomass model",
          "with medians and 80% intervals")

# only works if soil plot is saved in workspace
plot_grid(biomassplot, soilplot, labels = c("A.", "B.")) # 900 x 300


ppc_dens_overlay(y = mod1_mcmc[[1, y]],
                 yrep = posterior_predict(mod1_mcmc, draws = 50))
