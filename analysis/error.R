library(R2jags)
library(tidyverse)
library(cowplot)
library(here)
theme_set(theme_cowplot())

library(ggmcmc) # http://www.jkarreth.net/files/Lab5_Postestimation.html
library(ggridges)

# START #######################
data <- read.csv(here("data/output/biomass_variables.csv"))

data <- na.omit(data)

# DALTON ###################################
dalton <- data %>%
  filter(TREAT != 0) %>%
  filter(SITE == "DALTON")

# setting up biomass data
biomass_y <- as.numeric(dalton$Biomass_gm2)
nPlot <- as.numeric(length(unique(dalton$PLOT)))

summary(biomass_y)

J <- 4 # number of regression coefficients

# independent variables
density_x1 <- as.vector((dalton$dens_gm2 - mean(dalton$dens_gm2)) / sd(dalton$dens_gm2))
ba_x2 <- as.vector((dalton$decidba_gm2 - mean(dalton$decidba_gm2)) / sd(dalton$decidba_gm2))
SOL_x3 <- as.vector((dalton$avSOLdepth_CM - mean(dalton$avSOLdepth_CM)) / sd(dalton$avSOLdepth_CM))

# log.glm <- glm(log(Biomass_gm2) ~ scale(dens_gm2) + scale(decidba_gm2) + scale(avSOLdepth_CM), family = gaussian, data = dalton)
#
# summary(log.glm)
# # y = 6.93 + -0.09*density + 0.18*basal + 0.1*SOL

## LOGNORMAL - no random effects #####################################################

jagsdata_dalton <- with(dalton, # creates a list of elements
                        list(y = biomass_y,
                             J = J,
                             x1 = density_x1,
                             x2 = ba_x2,
                             x3 = SOL_x3,
                             N = length(dalton$Biomass_gm2)))

dalt_model <- function(){
  # Likelihood:
  for (i in 1:N){
    truY[i] ~ dnorm(y[i], 377) # 30% of mean biomass (g/m2)
    y[i] ~ dlnorm(mu[i], tau) # tau is precision (1 / variance)
    mu[i] <- beta[1] + beta[2] * truX1[i] + beta[3] * x2[i] + beta[4] * x3[i]

    truX1[i] ~ dnorm(0, 1)
    x[i] ~ dnorm(truX1[i], tauX1)

  }
  # Priors:
  for(i in 1:J){
    beta[i] ~ dnorm(0, 0.01)
  }

  sigma ~ dgamma(0.01, 0.01) # sigma^2 doesn't work in JAGS
  # Need to have model calculate sigma = 1/tau
  tau <- 1/(sigma * sigma)

  tauX1 ~ dunif(.03, .05)
}

# initial values ############################
init_values <- function(){ # specifies initial parameter values
  list(beta = rnorm(J),
       sigma = runif(1))
}

# parameters to track
params <- c( "beta","tau")

## Run the model in JAGS ###########################################
fit_dalton_error <- jags(data = jagsdata_dalton, # specifies data
                   inits = init_values,
                   parameters.to.save = params,
                   model.file = dalt_model,
                   n.chains = 3, # number of chains
                   n.iter = 2500,
                   n.burnin = 0) # burns out first 100
fit_dalton

traceplot(fit_dalton_error)


fit_dalt_mcmc <- as.mcmc(fit_dalton_error)
plot(fit_dalt_mcmc)

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
