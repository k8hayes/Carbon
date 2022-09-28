model {
  # Priors:
  a ~ dnorm(0, 0.0001) # mean, precision = N(0, 10^4)
  b ~ dnorm(0, 0.0001)
  shape ~ dunif(0, 100)

  # Likelihood data model:
  for (i in 1:N) {
    linear_predictor[i] <- a + b * x[i]
    # dgamma(shape, rate) in JAGS:
    y[i] ~ dgamma(shape, shape / exp(linear_predictor[i]))
  }
}
