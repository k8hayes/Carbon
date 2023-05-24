library(cowplot)
library(here)
theme_set(theme_cowplot())

library(ggmcmc) # http://www.jkarreth.net/files/Lab5_Postestimation.html
library(ggridges)

# START #######################
data <- read.csv(here("data/output/biomass_variables.csv"))

data <- na.omit(data)

# VARIABLES ###################################
data <- data %>%
  filter(TREAT != 0) 

# setting up biomass data
biomass_y <- as.numeric(data$Biomass_gm2)
nPlot <- as.numeric(length(unique(data$PLOT)))

summary(biomass_y)

data$site <- ifelse(data$SITE == "DALTON", 1,2)
site <- data$site

J <- 4 # number of regression coefficients

# independent variables
density_x1 <- as.vector((data$dens_gm2 - mean(data$dens_gm2)) / sd(data$dens_gm2))
ba_x2 <- as.vector((data$decidba_gm2 - mean(data$decidba_gm2)) / sd(data$decidba_gm2))
SOL_x3 <- as.vector((data$avSOLdepth_CM - mean(data$avSOLdepth_CM)) / sd(data$avSOLdepth_CM))

# log.glm <- glm(log(Biomass_gm2) ~ scale(dens_gm2) + scale(decidba_gm2) + scale(avSOLdepth_CM), family = gaussian, data = data)
#
# summary(log.glm)
# # y = 6.93 + -0.09*density + 0.18*basal + 0.1*SOL

## LOGNORMAL - no random effects #####################################################

jagsdata <- with(data, # creates a list of elements
                        list(y = biomass_y,
                             J = J,
                             Q = 2,
                             site = site,
                             density = density_x1,
                             x2 = ba_x2,
                             x3 = SOL_x3,
                             N = length(data$Biomass_gm2)))

dalt_model <- function(){
  # Likelihood:
  
  for (k in 1:N){ # plot level
    y[k] ~ dlnorm(mu[k], tau) # tau is precision (1 / variance)
    mu[j] <- beta[1] + beta[2] * x1[k] + beta[3] * x2[k] + beta[4] * x3[k]
    
  }
  # Priors:
  for(i in 1:J){ # 4 regression coefficients
    beta[i] ~ dnorm(0, 0.01)
  }

  sigma ~ dgamma(0.01, 0.01) # sigma^2 doesn't work in JAGS
  # Need to have model calculate sigma = 1/tau
  tau <- 1/(sigma * sigma)
  
  mu.alpha ~ dnorm(0, 0.01)
  tau.alpha <- 1/(sigma.alpha * sigma.alpha)
  sigma.alpha ~ dgamma(0.01, 0.01)
  
  # Hyperpriors
  
  for(j in 1:J) { # species-level
    x1 ~ dnorm(density, sd(density))
    
    for(i in 1:N){ # tree-level
      
    } 
  } 
}

# initial values ############################
init_values <- function(){ # specifies initial parameter values
  list(beta = rnorm(J),
       sigma = runif(1))
}

# parameters to track
params <- c( "beta","tau")

## Run the model in JAGS ###########################################
fit_data <- jags(data = jagsdata, # specifies data
                   inits = init_values,
                   parameters.to.save = params,
                   model.file = dalt_model,
                   n.chains = 3, # number of chains
                   n.iter = 2500,
                   n.burnin = 0) # burns out first 100
fit_data

traceplot(fit_data)


fit_dalt_mcmc <- as.mcmc(fit_data)
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