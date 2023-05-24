# START #####################################
data <-read.csv(here("data/dbh.csv"))

data <- data %>%
  filter(TREAT != 0)

data <- na.omit(data)

data$spp[data$SPP == "PIME"] <- 1
data$spp[data$SPP == "BENE"] <- 2
data$spp[data$SPP == "POTR"] <- 3
data$spp[data$SPP == "SALIX"] <- 4


dbh <- data$DBH
age <- data$AGE
spp <- data$spp

jags_data <- with(data, list(DBH = dbh,
                             AGE = age,
                             spp = spp,
                             N = length(dbh)))

mod_jags <- function(){
  # Biomass
  for(i in 1:N){ # i = tree # N = number of observations
    biomass[i] ~ dnorm(mu[spp[i]], tau[spp[i]])
    mu[j] <- beta0[j] + beta1[j]*((log(DBH) /log(10))) + beta2[j]*(AGE) + beta3[j]*((log(DBH) /log(10))*AGE)
  }
    
    for(j in 1:4){ # j = species, # N = 4 species
      mu[j] <- beta0[j] + beta1[j]*((log(DBH) /log(10))) + beta2[j]*(AGE) + beta3[j]*((log(DBH) /log(10))*AGE)
  }
  
  }
  
  # Priors
  beta0[1] <- 3.011
  beta1[1] <- 1.202
  beta2[1] <- -0.01
  beta3[1] <- 0.011

  beta0[2] <- 2.462
  beta1[2] <- 1.905
  beta2[2] <- 0
  beta3[2] <- 0
  
  beta0[3] <- 2.614
  beta1[3] <- 0.852
  beta2[3] <- 0
  beta3[3] <- 0.026
  
  beta0[4] <- 2.481
  beta1[4] <- 1.19
  beta2[4] <- 0
  beta3[4] <- 0
   
    tau[1] <- 1
    tau[2] <- 1
    tau[3] <- 1
    tau[4] <- 1
  }
  

# initial values
init_values <- function() {
  list(biomass = runif(1))
}

# parameters to track
params <- c("biomass")

fit_mod <- jags(data = jags_data,
                inits = init_values,
                parameters.to.save = params,
                model.file = mod_jags,
                n.chains = 2,
                n.iter = 2500)
