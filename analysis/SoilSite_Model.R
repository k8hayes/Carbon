# log normal with variables # DALTON and STEESE specific
#https://www.y1zhou.com/series/bayesian-stat/bayesian-stat-bayesian-linear-regression/
library(R2jags)
library(tidyverse)
library(cowplot)
library(here)
theme_set(theme_cowplot())

library(ggmcmc) # http://www.jkarreth.net/files/Lab5_Postestimation.html
library(ggridges)

# START #######################
data <- read.csv(here("data/output/soil_variables.csv"))

data <- na.omit(data)

# DALTON ###################################
dalton <- data %>%
  filter(TREAT != 0) %>%
  filter(SITE == "DALTON")

# setting up biomass data
soil_y <- as.numeric(dalton$Soil_gm2)
nPlot <- as.numeric(length(unique(dalton$PLOT)))

summary(soil_y)

J <- 4 # number of regression coefficients

# independent variables
density_x1 <- as.vector((dalton$dens_gm2 - mean(dalton$dens_gm2)) / sd(dalton$dens_gm2))
ba_x2 <- as.vector((dalton$decidba_gm2 - mean(dalton$decidba_gm2)) / sd(dalton$decidba_gm2))
SOL_x3 <- as.vector((dalton$avSOLdepth_CM - mean(dalton$avSOLdepth_CM)) / sd(dalton$avSOLdepth_CM))

#log.glm <- glm(log(Soil_gm2) ~ scale(dens_gm2) + scale(decidba_gm2) + scale(avSOLdepth_CM), family = gaussian, data = dalton)

#summary(log.glm)
# y = 7.44 + -0.35*density + -0.04*basal + 0.3*SOL

## LOGNORMAL - no random effects #####################################################

jagsdata_dalton <- with(dalton, # creates a list of elements
                        list(y = soil_y,
                             J = J,
                             x1 = density_x1,
                             x2 = ba_x2,
                             x3 = SOL_x3,
                             N = length(dalton$Soil_gm2)))

dalt_model <- function(){
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

## Run the model in JAGS ###########################################
fit_dalton <- jags(data = jagsdata_dalton, # specifies data
                   inits = init_values,
                   parameters.to.save = params,
                   model.file = dalt_model,
                   n.chains = 3, # number of chains
                   n.iter = 2500,
                   n.burnin = 0) # burns out first 100
fit_dalton

traceplot(fit_dalton)
plot(fit_dalt_mcmc)

fit_dalt_mcmc <- as.mcmc(fit_dalton)


## plots ###########################
dalt_fit_mat <- as.matrix(fit_dalt_mcmc)
dalt_fit_df <- as.data.frame(dalt_fit_mat)

dalt_fit_betas <- dalt_fit_df[, grep(x = colnames(dalt_fit_df),
                                     pattern = "beta[",
                                     fixed = TRUE)]

dalt_coef <- dalt_fit_betas %>%
  pivot_longer(cols = everything(),
               names_to = "Coef",
               values_to = "Value") %>%
  filter(Coef != "beta[1]")

ggplot(dalt_coef, aes(Value, Coef)) +
  stat_density_ridges(quantile_lines = TRUE,
                      quantiles = c(0.025, 0.5, 0.975),
                      alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() + xlab("Posterior estimate") + ylab("")

# STEESE ###################################
data <- read.csv(here("data/output/soil_variables.csv"))

data <- na.omit(data)

steese <- data %>%
  filter(TREAT != 0) %>%
  filter(SITE == "STEESE")

# setting up fake biomass data
soil_y <- as.numeric(steese$Soil_gm2)
nPlot <- as.numeric(length(unique(steese$PLOT)))

summary(soil_y)

J <- 4 # number of regression coefficients

# independent variables
density_x1 <- as.vector((steese$dens_gm2 - mean(steese$dens_gm2)) / sd(steese$dens_gm2))
ba_x2 <- as.vector((steese$decidba_gm2 - mean(steese$decidba_gm2)) / sd(steese$decidba_gm2))
SOL_x3 <- as.vector((steese$avSOLdepth_CM - mean(steese$avSOLdepth_CM)) / sd(steese$avSOLdepth_CM))

jagsdata_ste <- with(steese, # creates a list of elements
                     list(y = soil_y,
                          J = J,
                          x1 = density_x1,
                          x2 = ba_x2,
                          x3 = SOL_x3,
                          N = length(steese$Soil_gm2)))

ste_model <- function(){
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
fit_steese <- jags(data = jagsdata_ste, # specifies data
                   inits = init_values,
                   parameters.to.save = params,
                   model.file = ste_model,
                   n.chains = 3, # number of chains
                   n.iter = 2000,
                   n.burnin = 0) # only keeps every 10th iteration)
fit_steese

plot(fit_steese)

traceplot(fit_steese)

steese_mcmc <- as.mcmc(fit_steese)

## plots ###########################
steese_fit_mat <- as.matrix(steese_mcmc)
steese_fit_df <- as.data.frame(steese_fit_mat)

steese_fit_betas <- steese_fit_df[, grep(x = colnames(steese_fit_df),
                                         pattern = "beta[",
                                         fixed = TRUE)]

steese_coef <- steese_fit_betas %>%
  pivot_longer(cols = everything(),
               names_to = "Coef",
               values_to = "Value") %>%
  filter(Coef != "beta[1]")

ggplot(steese_coef, aes(Value, Coef)) +
  stat_density_ridges(quantile_lines = TRUE,
                      quantiles = c(0.025, 0.5, 0.975),
                      alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() + xlab("Posterior estimate") + ylab("")

# COMBINING ##########################################
dalt_coef$Site <- "Upland"
steese_coef$Site <- "Lowland"

both_coef <- rbind(dalt_coef, steese_coef)

both_coef$var[both_coef$Coef == "beta[2]"] <- "Tree Density"
both_coef$var[both_coef$Coef == "beta[3]"] <- "Decid. Basal Area"
both_coef$var[both_coef$Coef == "beta[4]"] <- "SOL Depth"

ggplot(both_coef, aes(Value, var, fill = Site)) +
  stat_density_ridges(quantile_lines = TRUE,
                      quantiles = c(0.025, 0.5, 0.975),
                      alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() + xlab("Posterior estimate") +
  xlim(c(-1,1)) +
  scale_y_discrete(expand = expansion(add = c(0.2, 2))) +
  ylab("Coefficient") + ggtitle("Soil Posterior Density Distributions")

# export as 750 x 350

# run to combine with biomass model figure
soil_plot <- ggplot(both_coef, aes(Value, var, fill = Site)) +
  stat_density_ridges(quantile_lines = TRUE,
                      quantiles = c(0.025, 0.5, 0.975),
                      alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() + xlab("Posterior estimate") +
  xlim(c(-1,1)) +
  scale_y_discrete(expand = expansion(add = c(0.2, 2))) +
  ylab("") + ggtitle("Soil Posterior Density Distributions") +
  theme(axis.text.y = element_blank(), # taking y axis labels off
         axis.ticks.y = element_blank())
soil_plot
