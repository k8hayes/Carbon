library(R2jags)
library(UsingR)

data("father.son")

# Linear model #######

lm_fs <- lm(sheight ~ fheight, data = father.son)

summary(lm_fs) # intercept 33.8, slope 0.5414
plot(sheight ~ fheight, data = father.son)
abline(lm_fs, col = "red")

# Setting up JAGS ########

jagsdata_s1 <- with(father.son, # creates a list of three elements
                    list(fheight = fheight,
                         sheight = sheight,
                         N = length(father.son$sheight)))

# model 1 - uninformative priors
lm1_jags <- function(){
  # Likelihood:
  for (i in 1:N){
    sheight[i] ~ dnorm(mu[i], tau) # tau is precision (1 / variance)
    mu[i] <- alpha + beta * fheight[i] + error

  }
  # Priors:
  alpha ~ dnorm(0, 0.01) # intercept
  beta ~ dnorm(0, 0.01) # slope
  sigma ~ dunif(0, 100) # standard deviation
  tau <- 1 / (sigma * sigma) # sigma^2 doesn't work in JAGS
  error ~ dgamma(0.01, 0.01)
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
#
#
# }

init_values <- function(){ # specifies initial parameter values
  list(alpha = rnorm(1), beta = rnorm(1), sigma = runif(1))
}

# # model 3 - setting priors to different distributions
# lm3_jags <- function(){
#   # Likelihood:
#   for (i in 1:N){
#     sheight[i] ~ dnorm(mu[i], tau) # tau is precision (1 / variance)
#     mu[i] <- alpha + beta * fheight[i]
#
#   }
#   # Priors:
#   alpha ~ dgamma(0.001, 0.001) # intercept
#   beta ~ dnorm(0, 0.01) # slope
#   sigma  <- 1 / sqrt(tau) # need to invert gamma
#   tau ~ dgamma(0.001, 0.001) # sigma^2 doesn't work in JAGS
# }
#
 init_values <- function(){ # specifies initial parameter values
   list(alpha = rnorm(1), beta = rnorm(1), tau = rgamma(1, 1))
 }

params <- c("alpha", "beta", "tau") # chooses the parameters for which to report posteriors

# # Run the model in JAGS#########################
fit_lm1 <- jags(data = jagsdata_s1, # specifies data # specifies initial values
                parameters.to.save = params,
                model.file = lm1_jags,
                n.chains = 3, # number of chains
                n.iter = 1200, # number of iterations
                n.burnin = 2000, # discards the first 2k values
                n.thin = 10) # only keeps every 10th iteration
#
fit_lm2 <- jags(data = jagsdata_s1, # specifies data
                inits = init_values, # specifies initial values
                parameters.to.save = params,
                model.file = lm2_jags,
                n.chains = 3, # number of chains
                n.iter = 12000, # number of iterations
                n.burnin = 2000, # discards the first 2k values
                n.thin = 10) # only keeps every 10th iteration

# fit_lm3 <- jags(data = jagsdata_s1, # specifies data
                # inits = init_values, # specifies initial values
                # parameters.to.save = params,
                # model.file = lm3_jags,
                # n.chains = 3, # number of chains
                # n.iter = 12000, # number of iterations
                # n.burnin = 2000, # discards the first 2k values
                # n.thin = 10)

# Diagnostics ############################

fit_lm1 # model output
  # DIC 4984

fit_lm2 # output from priors
  # DIC 5278

traceplot(fit_lm1, mfrow = c(2, 2), ask = F) # traceplots of all three chains

plot(fit_lm1) # marginal posterior distributions


lm1_mcmc <- as.mcmc(fit_lm1) # need to convert to class 'mcmc' first
plot(lm1_mcmc) # complete posterior distributions
                  # traceplots + densities

# Plotting the data with the model prediction ###########

  # create new sequence of predictor values
  nvalues <- 100
  fheight_new <- seq(min(father.son$sheight),
                      max(father.son$fheight),
                      length.out = nvalues)

  # combine 3 chains into one mcmc object
  lm1_mcmc_combi <- as.mcmc(rbind(lm1_mcmc[[1]], lm1_mcmc[[2]], lm1_mcmc[[3]]))

  # calculate predicted son heights for new father heights
  pred_mean_mean <- mean(lm1_mcmc_combi[, "alpha"]) + fheight_new * mean(lm1_mcmc_combi[, "beta"])

# also want to show uncertainty around mean predicted value
  # 2 forms of uncertainty
    # uncertainty about true parameter values (1)
    # uncertainty caused through stochastic relationship between father/son heights (2)

  # 1) credible uncertainty = credible interval around predicted sheight for given fheight
      pred_mean_dist <- matrix(NA, nrow = nrow(lm1_mcmc_combi), ncol = nvalues)
      # calculates 95% credible interval for each individual (n = 1078)
       for (i in 1:nrow(pred_mean_dist)){
        pred_mean_dist[i,] <- lm1_mcmc_combi[i,"alpha"] + fheight_new * lm1_mcmc_combi[i,"beta"]
      }
      credible_lower <- apply(pred_mean_dist, MARGIN = 2, quantile, prob = 0.025)
      credible_upper <- apply(pred_mean_dist, MARGIN = 2, quantile, prob = 0.975)

  # 2) drawing random values for normal distribution w/ mean defined by fheight, alpha/beta + variance
        lm1_mcmc_combi_rep <- do.call(rbind, rep(list(lm1_mcmc_combi), 50)) # replication

        # Draw random values for all parameter combinations (rows) and body length values (columns):
        pred_data_dist <- matrix(NA, nrow = nrow(lm1_mcmc_combi_rep), ncol = nvalues)
        for (i in 1:nrow(pred_data_dist)){
          pred_data_dist[i,] <- lm1_mcmc_combi_rep[i,"alpha"] + fheight_new * lm1_mcmc_combi_rep[i,"beta"] +
            rnorm(nvalues, mean = 0, sd = lm1_mcmc_combi_rep[i, "tau"])
        }

        # Calculate quantiles:
        uncertain_lower <- apply(pred_data_dist, MARGIN = 2, quantile, prob = 0.025)
        uncertain_upper <- apply(pred_data_dist, MARGIN = 2, quantile, prob = 0.975)


## Plot of mean model prediction + 2 uncertainty measures #####

dev.off()

plot(sheight ~ fheight, data = father.son)
lines(fheight_new, pred_mean_mean)
lines(fheight_new, credible_lower, lty = 2)
lines(fheight_new, credible_upper, lty = 2)
lines(fheight_new, uncertain_lower, lty = 2, col = "red")
lines(fheight_new, uncertain_upper, lty = 2, col = "red")

