install.packages("rethinking")

library(rethinking)

data <- read.csv(here("data/output/biomass_variables.csv"))

data <- data %>%
  filter(TREAT != 0)


# defining variables
data <- na.omit(data)

data$Biomass_gm2 <- as.vector(data$Biomass_gm2)

# independent variables
data$dens_gm2 <- (data$dens_gm2 - mean(data$dens_gm2)) / sd(data$dens_gm2)
data$decid_index <- (data$decidba_gm2 - mean(data$decidba_gm2)) / sd(data$decidba_gm2)
data$SOL_index <- as.vector(data$avSOLdepth_CM) / max(data$avSOLdepth_CM)

data$SID <- ifelse(data$SITE == "DALTON", 1,2)

mod <- quap(
  alist(
    Biomass_gm2 ~ dnorm( mu, sigma),
    mu <- a[SID] + b[SID]*(dens_gm2),
    a[SID] ~ dnorm(1, 0.1),
    b[SID] ~ dnorm(0,0.01),
    sigma ~ dexp(1)),
  data = data
)

precis(mod, depth = 2)

mod <- quap(
  alist(
    Biomass_gm2 ~ dnorm( mu, sigma),
    mu <- a[SID] + b*(dens_gm2),
    a[SID] ~ dnorm(1, 0.1),
    b ~ dnorm(0,0.01),
    sigma ~ dexp(1)),
  data = data
)

precis(mod, depth = 2)


# example ######################3
data(rugged)

  d <- rugged

  # make log version of outcome
  d$log_gdp <- log( d$rgdppc_2000 )

  # extract countries with GDP data
  dd <- d[ complete.cases(d$rgdppc_2000) , ]

  # rescale variables
  dd$log_gdp_std <- dd$log_gdp / mean(dd$log_gdp)
  dd$rugged_std <- dd$rugged / max(dd$rugged)

  # make variable to index Africa (1) or not (2)
  dd$cid <- ifelse( dd$cont_africa==1 , 1 , 2 )
