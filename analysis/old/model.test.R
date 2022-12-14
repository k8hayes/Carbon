model {
  for ( i in 1:N ) {
    y[i] ~ dgamma( sh , ra )
  }
  # parameterized by mean (m) and standard deviation (sd)
  sh <- pow(m,2) / pow(sd,2)
  ra <-     m    / pow(sd,2)
  m ~ dunif(0,100)
  sd ~ dunif(0,100)
}

