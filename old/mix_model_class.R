library(rjags)

# Generating Data
N <- 1874
p <- 0.3

yp1 <- rnorm(p*N, -18, 2.8)
yp2 <- rnorm((1-p)*N, -32, 2.8)
data <- c(yp1, yp2)
hist(data)
nsamp <- length(data)

jags_data <- with(data,
                  list(y = data,
                       N = length(data),
                       Nclust = 2)) # number of clusters

erica_mix <- function() {
  # Likelihood
  for (i in 1:N) {
    y[i] ~ dnorm(mu[k[i]], tau[k[i]])
    k[i] <- dcat(pi1:NClust)
  }

  # Priors
  pi[1:K] ~ ddirch(c(1,1))
  for ( i in 1: NClust ) {
    mu[i] ~ dnorm(0, .001)
    tau[i] ~ dgamma(.001, .001)
    sigma[i] <- 1/sqrt(tau[i])
  }
}

# parameters

params <- c("alpha", "beta", "sigma", "sigma2")
