
library(R2jags)
library(tidyverse)
library(cowplot)
library(here)
theme_set(theme_cowplot())

library(bayesplot)
library(ggridges)


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
  for(i in 1:N){
    biomass[i] ~ dnorm(mu[spp[i]], tau[spp[i]])

  }
  
  for(i in 1:4){
    mu[1] ~ dnorm( 3.011 + 1.202*((log(DBH) /log(10))) + -0.01*(AGE) + 0.011*((log(DBH) /log(10))*AGE), 1) # PIME
    mu[2] ~ dnorm(2.462 + 1.095*((log(DBH) /log(10))), 1) # BENE
    mu[3] ~ dnorm(2.614 + 0.852*((log(DBH) /log(10)))  + 0.026*((log(DBH) /log(10))*AGE), 1) # POTR
    mu[4] ~ dnorm(2.481 + 1.19*((log(DBH) /log(10))), 1) # SALIX
    
    tau[1] <- 1
    tau[2] <- 1
    tau[3] <- 1
    tau[4] <- 1
  }

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
