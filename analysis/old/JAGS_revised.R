library(R2jags)
library(here)


data <- read.csv(here("data/output/biomass_plot.csv"))

# Linear model #######

lm_fs <- lm(Biomass_m2 ~ TREAT, data = data)

summary(lm_fs) # intercept 1660.4, slope -533.1
plot(Biomass_m2 ~ TREAT, data = data)
abline(lm_fs, col = "red")

# Setting up JAGS ########

jagsdata_s1 <- with(data, # creates a list of three elements
                    list(treat = TREAT,
                         Biomass_m2 = Biomass_m2,
                         N = length(data$Biomass_m2)))

 # model 1 - uninformative priors
 lm1_jags <- function(){
  # Likelihood:
  for (i in 1:N){
    Biomass_m2[i] ~ dnorm(mu[i], tau) # tau is precision (1 / variance)
    mu[i] <- alpha + beta * treat[i]

  }
  # Priors:
  alpha ~ dnorm(0, 0.01) # intercept
  beta ~ dnorm(0, 0.01) # slope
  sigma ~ dunif(0, 100) # standard deviation
  tau <- 1 / (sigma * sigma) # sigma^2 doesn't work in JAGS
}

# # model 2 - priors informed by glm
# lm2_jags <- function(){
#   # Likelihood:
#   for (i in 1:N){
#     sheight[i] ~ dnorm(mu[i], tau) # tau is precision (1 / variance)
#     mu[i] <- alpha + beta * fheight[i]
#
#   }
#   # Priors:
#   alpha ~ dnorm(0, 33) # intercept
#   beta ~ dnorm(0, 0.05) # slope
#   sigma ~ dunif(0, 100) # standard deviation
#   tau <- 1 / (sigma * sigma) # sigma^2 doesn't work in JAGS
# }
#
 init_values <- function(){ # specifies initial parameter values
   list(alpha = rnorm(1, mean = 1660), beta = rnorm(1), sigma = runif(1))
 }

 params <- c("alpha", "beta", "tau") # chooses the parameters for which to report posteriors

 fit_lm1 <- jags(data = jagsdata_s1, # specifies data
                                 inits = init_values, # specifies initial values
                                 parameters.to.save = params,
                                 model.file = lm1_jags,
                                 n.chains = 3, # number of chains
                                 n.iter = 12000, # number of iterations
                                 n.burnin = 2000, # discards the first 2k values
                                 n.thin = 10) # only keeps every 10th iteration
 fit_lm1 # model output

 traceplot(fit_lm1, mfrow = c(2, 2), ask = F)
