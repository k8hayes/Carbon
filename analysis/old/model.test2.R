model{
  ## Likelihood
  for(i in 1:N){
    y[i] ~ dnegbin(p[i],r)
    p[i] <- r/(r+lambda[i])
    log(lambda[i]) <- mu[i]
    mu[i] <- inprod(beta[],X[i,])
  }
  ## Priors
  beta ~ dmnorm(0, 100000)
  r ~ dunif(0,50)
}
