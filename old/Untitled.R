<- 1000
p<- 0.7
yp1 <- rnorm(p*n, 18, 2.8)
yp2 <- rnorm((1-p)*n, 30, 5)
​
ydat<-c(yp1,yp2)
hist(ydat)
​
list<-cbind(seq(1:1000),ydat)
my_list<-as.data.frame(list)
​
​
​
​
###Try to recreate
​
​
library(R2jags)
library(UsingR)
​
length(my_list$ydat)
​
# Setting up JAGS ########
​
jagsdata_s1 <- with(my_list, # creates a list
                    list(ydat=ydat,
                         N = length(my_list$V1)))
​
lm1_jags <- function(){
  # Likelihood:
  for (i in 1:N){
    ydat[i] ~ dnorm(mu[i], tau) # tau is precision (1 / variance)
    mu[i] <- alpha + p[i] * beta
    #p[i] ~ dbern(p1)
    p[i] ~ dbern(p1)
  }
  # Priors:
  p1 ~ dunif(0,1)
  alpha ~ dnorm(0,0.01)
  beta ~ dnorm(0,0.01)
  sigma ~ dnorm(0,0.01)
  #sigma ~ dunif(100) # standard deviation
  tau <- 1 / (sigma * sigma) # sigma^2 doesn't work in JAGS
}
​
init_values <- function(){ # specifies initial parameter values
  list(p1 = runif(1), alpha = rnorm(1), beta = rnorm(1), sigma = runif(1))
}
​
params <- c("p1","alpha", "beta", "sigma") # chooses the parameters for which to report posteriors
​
# Run the model in JAGS#########################
fit_lm1 <- jags(data = jagsdata_s1, # specifies data
                inits = init_values, # specifies initial values
                parameters.to.save = params,
                model.file = lm1_jags,
                n.chains = 3, # number of chains
                n.iter = 12000, # number of iterations
                n.burnin = 2000, # discards the first 2k values
                n.thin = 10) # only keeps every 10th iteration
​
​
# Diagnostics ############################
​
fit_lm1 # model output
​
traceplot(fit_lm1, mfrow = c(2, 2), ask = F) # traceplots of all three chains
​
plot(fit_lm1) # marginal posterior distributions
Collapse



