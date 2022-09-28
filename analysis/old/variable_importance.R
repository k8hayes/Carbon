# Variable importance #####
library(tidyverse)
library(here)
library(AICcmodavg)

# Set up ################
data <- read.csv(here("data/output/biomass_plot.csv"))

data <- data %>%
  group_by(SITE, TREAT, PLOT) %>%
  summarise(Biomass_m2 = sum(Biomass_m2))

# Pulling out basal area from deciduous species ######
  # calculating basal area
  ba <- read.csv(here("data/dbh.csv"))
  ba$BA <- (pi * (ba$DBH/2)^2)/10000
  ba$BA_HA <- ba$BA * ba$EXP_FACT # scaling up from expansion factor
  write.csv(ba, here("data/ba.csv"), row.names= F)

  # summing basal area according to species
  ba <- ba %>%
    group_by(SITE, TREAT, PLOT, SPP) %>%
    summarise(BA_SPP = sum(BA_HA)) %>%
    pivot_wider(names_from = SPP, values_from = BA_SPP, values_fill = 0)

  # setting up to pull values out to use as variable
  # (basal area)
  plot.names <- data$PLOT

    ## Salix #####
       plot.ba.salix <- vector( length = length(data$PLOT))

       for(i in 1:length(plot.names)) {
         plot.ba.salix[i] <- ba$SALIX[ba$PLOT == plot.names[i]]
       }

       plot.ba.salix

       data$BA_salix <- plot.ba.salix

    ## Birch ####
      plot.ba.birch <- vector( length = length(data$PLOT))

      for(i in 1:length(plot.names)) {
        plot.ba.birch[i] <- ba$BENE[ba$PLOT == plot.names[i]]
      }

      plot.ba.birch # checking if it worked

      data$BA_birch <- plot.ba.birch

      ## Aspen ####
       plot.ba.aspen <- vector( length = length(data$PLOT))

       for(i in 1:length(plot.names)) {
         plot.ba.aspen[i] <- ba$POTR[ba$PLOT == plot.names[i]]
       }

       plot.ba.aspen # checking if it worked

       data$BA_aspen <- plot.ba.aspen

       rm(plot.ba.aspen, plot.ba.birch, plot.ba.salix, i, plot.names)

# taking out unburned sites
data <- data %>%
  filter(TREAT != 0)

data$TREAT <- as.factor(data$TREAT)

# Models ######################

# single variable
glm_none <- glm(Biomass_m2 ~ 1, data = data, family = Gamma(link = "log"))

glm_treat <- glm(Biomass_m2 ~ TREAT, data = data, family = Gamma(link = "log"))

glm_site <- glm(Biomass_m2 ~  SITE, data = data, family = Gamma(link = "log"))

glm_birch <- glm(Biomass_m2 ~  BA_birch, data = data, family = Gamma(link = "log"))

glm_aspen <- glm(Biomass_m2 ~  BA_aspen, data = data, family = Gamma(link = "log"))

glm_salix <- glm(Biomass_m2 ~  BA_salix, data = data, family = Gamma(link = "log"))

# combinations

glm_all <- glm(Biomass_m2 ~ TREAT  + SITE + BA_birch + BA_aspen + BA_salix,
               data = data, family = Gamma(link = "log"))

# treat + things
glm_treat_birch <- glm(Biomass_m2 ~ TREAT  + BA_birch, data = data, family = Gamma(link = "log"))

glm_treat_salix <- glm(Biomass_m2 ~ TREAT  + BA_salix, data = data, family = Gamma(link = "log"))

glm_treat_aspen <- glm(Biomass_m2 ~ TREAT  + BA_aspen, data = data, family = Gamma(link = "log"))

glm_treat_site <- glm(Biomass_m2 ~  TREAT + SITE, data = data, family = Gamma(link = "log"))

# site + things
glm_site_birch <- glm(Biomass_m2 ~  SITE + BA_birch, data = data, family = Gamma(link = "log"))

glm_site_aspen <- glm(Biomass_m2 ~  SITE + BA_aspen, data = data, family = Gamma(link = "log"))

glm_site_salix <- glm(Biomass_m2 ~  SITE + BA_salix, data = data, family = Gamma(link = "log"))

# birch + things


# parameters
mod.list <- list(glm_none,glm_treat, glm_birch, glm_site, glm_all,
                 glm_treat_ba, glm_treat_site, glm_site_birch,
                 glm_aspen, glm_salix, glm_treat_aspen, glm_treat_salix,
                 glm_site_salix)

mod.names <- as.character(unlist(lapply(mod.list,formula)))

(mod.results <- aictab(mod.list,modnames=mod.names))

treat_weight <- sum(mod.results$AICcWt[1:4])

site_weight <- sum(mod.results$AICcWt[c(2,4,7,8)])

birch_weight <- sum(mod.results$AICcWt[c(3,4,6,8)])

total <- sum(treat_weight, site_weight, birch_weight)

treat_weight / total

birch_weight / total

site_weight / total
