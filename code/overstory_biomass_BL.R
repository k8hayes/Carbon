# Overstory Biomass ###############################

# input: dbh.csv
# output: data/output/overstory_biomass_plot.csv

# Equations used:
# Yarie et al. 2007
# NOTE - I actually stopped using any Yarie ones -
# the math wasn't working, (negative values for biomass even with intercepts )
# never heard back from michelle mack

# Alexander et al. 2012
# species: aspen, poplar, birch, spruce
# Bond-Lamberty et al. 2002
# species: salix
# Binkley et al. 1984
# species: alder

# STILL NEED ARCTIC BIRCH

# other things to resolve:
# - never heard back from Michelle Mack about uncertainty / data

## Start ###############################

library(tidyverse)
library(here)
library(cowplot); theme_set(theme_cowplot())
options(scipen = 999)

dbh <- read.csv(here("data/dbh.csv"))
unique(dbh$SPP)

# BIRCH ###########################################################################
# Alexander et al. 2012

birch <- dbh %>%
  filter(SPP == "BENE")
hist(birch$DBH) # big stuff (dbh > 18) is all unburned sites, all dead
summary(birch$DBH) # min 0.1, max 23.5
# n = 1473

dalt_birch <- birch %>%
  filter(SITE == "DALTON")
summary(dalt_birch$DBH)
length(dalt_birch$DBH)

stee_birch <- birch %>%
  filter(SITE == "STEESE")
summary(stee_birch$DBH)
length(stee_birch$DBH)

rm(dalt_birch, stee_birch)

## total ########################

tab_bene <- function(DBH, AGE) 10^(2.462 + 1.095*(log10(DBH)) + 0*(AGE) + 0*(log10(DBH)*AGE)) * 1.032
# n = 19, r2 = 0.98

birch$EQ_tab <- tab_bene(birch$DBH, birch$AGE)

## components ######################

# FOLIAGE
fol_bene  <- function(DBH, AGE) 10^(2.253 + 0*(log10(DBH)) + -0.055*(AGE) + 0*(log10(DBH)*AGE)) * 1.03
# n = 19, r2 = 0.89

birch$fol <- fol_bene(birch$DBH, birch$AGE)

## graph #################################

# subtracting foliage for dead individuals
birch$Biomass <- birch$EQ_tab
birch$Biomass[birch$CANOPY == 0] <- birch$Biomass[birch$CANOPY == 0] - birch$fol[birch$CANOPY == 0]


ggplot(birch, aes(x = DBH)) +
  geom_point(aes(y = Biomass)) +
  geom_ribbon(aes(ymax = Biomass, ymin = Biomass), alpha = .3) +
  geom_vline(xintercept = c(0.1, 23.5), linetype = "dashed") +
  labs(x = "DBH (cm)", y = "Dry Biomass (g)",
       title = "Alaskan Birch (Total Above. Biomass)") 

birch <- birch %>%
  select(!c(EQ_tab, fol))

# BLACK SPRUCE ############################################################################
# Alexander at al. 2012

pime <- dbh %>%
  filter(SPP == "PIME") %>%
  filter(DBH <=20)
hist(pime$DBH) # 2 trees over 20, one large one in unburned site, one dead one in once-burned
summary(pime$DBH) # min 0.1, max 26
# n = 2292

dalt_pime <- pime %>%
  filter(SITE == "DALTON")
summary(dalt_pime$DBH)
length(dalt_pime$DBH)

stee_pime <- pime %>%
  filter(SITE == "STEESE")
summary(stee_pime$DBH)
length(stee_pime$DBH)

rm(dalt_pime, stee_pime)

## total ########################

# second equation in table
tab_pime <- function(DBH, AGE) 10^(3.011 + 1.202*(log10(DBH)) + -0.01*(AGE) + 0.011*(log10(DBH)*AGE)) * 1.057
# n = 78, r2 = 0.92

pime$EQ_tab <- tab_pime(pime$DBH, pime$AGE)

##  components ######################
# testing out calculating each component

# FOLIAGE
fol_pime <- function(DBH, AGE) 10^(3.016 + 0.680*(log10(DBH)) + -0.02*(AGE) + 0.017*(log10(DBH)*AGE)) * 1.08
# n = 56, r2 = 0.5

pime$fol <- fol_pime(pime$DBH, pime$AGE)

## graph ###########################################

# subtracting foliage for dead individuals
pime$Biomass <- pime$EQ_tab
pime$Biomass[pime$CANOPY == 0] <- pime$Biomass[pime$CANOPY == 0] - pime$fol[pime$CANOPY == 0]

ggplot(pime, aes(x = DBH)) +
  geom_point(aes(y = Biomass)) +
  geom_ribbon(aes(ymax = Biomass, ymin = Biomass), alpha = .3) +
  geom_vline(xintercept = c(0.1, 26), linetype = "dashed") +
  labs(x = "DBH (cm)", y = "Dry Biomass (g)",
       title = "Black Spruce (Total Above. Biomass)") 
pime <- pime %>%
  select(!c(EQ_tab, fol))

# ASPEN ############################################################################
# Bond-Lamberty et al. 2002

potr <- dbh %>%
  filter(SPP == "POTR")
hist(potr$DBH) # everything smaller than 6 cm
summary(potr$DBH) # min 0.1, max 6.5

dalt_potr <- potr %>%
  filter(SITE == "DALTON")
summary(dalt_potr$DBH)
length(dalt_potr$DBH)

stee_potr <- potr %>%
  filter(SITE == "STEESE")
summary(stee_potr$DBH)
length(stee_potr$DBH)

rm(dalt_potr, stee_potr)

## total ######################
tab_potr <- function(DBH, AGE) 10^(2.614 + 0.852*(log10(DBH)) + 0*(AGE) + 0.026*(log10(DBH)*AGE)) * 1.043
# n = 40, r2 = 0.99
# SE

potr$EQ_tab <- tab_potr(potr$DBH, potr$AGE)

## component ######################
# calculating each component

# FOLIAGE
fol_potr <- function(DBH, AGE) 10^(1.961 + 0.909*(log10(DBH)) + -0.023*(AGE) + 0.021*(log10(DBH)*AGE)) * 1.121
# n = 41,r2 = 0.91

potr$fol <- fol_potr(potr$DBH, potr$AGE)

## graph ###################################

# subtracting foliage for dead individuals
potr$Biomass <- potr$EQ_tab
potr$Biomass[potr$CANOPY == 0] <- potr$Biomass[potr$CANOPY == 0] - potr$fol[potr$CANOPY == 0] 


ggplot(potr, aes(x = DBH)) +
  geom_point(aes(y = Biomass)) +
  geom_ribbon(aes(ymax = Biomass , ymin = Biomass), alpha = .3) +
  geom_vline(xintercept = c(0.1, 6.5), linetype = "dashed")   +
  labs(x = "DBH (cm)", y = "Dry Biomass (g)",
       title = "Aspen (Total Above. Biomass)") 

potr <- potr %>%
  select(!c(EQ_tab, fol))

# POPLAR ############################################################################
# Alexander et al. 2012

# NOTE: Alexander et al. 2012 only published mean tree dbh (cm), not a range
# no data provided for poplar

poba <- dbh %>%
  filter(SPP == "POBA")
hist(poba$DBH) # we only have three of these above dbh height...
summary(poba$DBH) # min 1.3, max 2.3

dalt_poba <- poba %>%
  filter(SITE == "DALTON")
summary(dalt_poba$DBH)
length(dalt_poba$DBH)

stee_poba <- poba %>%
  filter(SITE == "STEESE")
summary(stee_poba$DBH)
length(stee_poba$DBH)

rm(dalt_poba, stee_poba)

## total########################
# testing total aboveground biomass equations
# only testing those that only require dbh

# first equation in table
tab_poba <- function(DBH) 0.261*exp(1)^(0.0591*DBH)*1000
# n = 17, r2 = 0.96

poba$Biomass <- tab_poba(poba$DBH)

## component ######################
# testing out calculating each component


# FOLIAGE
#fol_poba <- function(x) 22.16*x^(1.62)
# n = 17, r2 = 0.84

#poba$fol <- fol_poba(poba$DBH)

## graph ##########################

ggplot(poba, aes(x = DBH)) +
  geom_point(aes(y = Biomass)) +
  geom_ribbon(aes(ymax = Biomass , ymin = Biomass), alpha = .3) +
  geom_vline(xintercept = c(1.3, 2.3), linetype = "dashed")   +
  labs(x = "DBH (cm)", y = "Dry Biomass (g)", 
       title = "Balsam Poplar (Total Above. Biomass)") 

# SALIX ##################################################################
# Bond-Lamberty et al. 2002

salix <- dbh %>%
  filter(SPP == "SALIX")
hist(salix$DBH)
summary(salix$DBH) # min 0 (CHECK), max 8.1

salix[salix$DBH == 0,] # just one, 41_1
# checked the books, it's 0 in the books too
# just going to delete it for now
salix <- salix %>%
  filter(DBH >0)


dalt_salix <- salix %>%
  filter(SITE == "DALTON")
summary(dalt_salix$DBH)
length(dalt_salix$DBH)

stee_salix <- salix %>%
  filter(SITE == "STEESE")
summary(stee_salix$DBH)
length(stee_salix$DBH)

rm(dalt_salix, stee_salix)

## total ##########################
tab_salix <- function(DBH, AGE) 10^(2.481 + 1.19*(log10(DBH)) + 0*(AGE) + 0*(log10(DBH)*AGE)) * 1.121

salix$EQ_tab <- tab_salix(salix$DBH, salix$AGE)
hist(salix$EQ_tab)

## components #################

## foliage
fol_salix <- function(DBH, AGE) 10^(2.023 + 1.065*(log10(DBH)) + -0.041*(AGE) + 0*(log10(DBH)*AGE)) * 1.103
# n = 2, r2 = 0.8.31

salix$fol <- fol_salix(salix$DBH, salix$AGE) 

## graph ###################

# subtracting foliage for dead individuals
salix$Biomass <- salix$EQ_tab
salix$Biomass[salix$CANOPY == 0] <- salix$Biomass[salix$CANOPY == 0] - salix$fol[salix$CANOPY == 0]


ggplot(salix, aes(x = DBH)) +
  geom_point(aes(y = Biomass)) +
  geom_ribbon(aes(ymax = Biomass , ymin = Biomass), alpha = .3) +
  geom_vline(xintercept = c(0.1, 8.1), linetype = "dashed")  +
  labs(x = "DBH (cm)", y = "Dry Biomass (g)", 
       title = "Salix (Total Above. Biomass)")
# geom_text(mapping = aes(x = 1.35, y = 900, label = "Observed Range"),
#           angle = 90, size = 3, check_overlap = TRUE)

salix <- salix %>%
  select(!c(EQ_tab, fol))

# ALDER #######################################

# Having a hard time finding alnus crispa equations that use DBH

# one interesting thing: He et al. 2018 developed alnus / salix and bog birch equations for boreal fen
# they use basal diameter which is a bummer
# but find "a single generalized equation is sufficient for estimating aboveground biomass for all three genera"

# Binkley et al. 1984

# technically for Alnus viridis sinuata, not Alnus virisidis
# BUT! the first equation I've found period for viridis that uses DBH, not basal diameter

alcr <- dbh %>%
  filter(SPP == "ALCR")
hist(alcr$DBH) # n = 415
summary(alcr$DBH) # min 0.2, max 4.5

dalt_alcr <- alcr %>%
  filter(SITE == "DALTON")
summary(dalt_alcr$DBH)
length(dalt_alcr$DBH)

stee_alcr <- alcr %>%
  filter(SITE == "STEESE")
summary(stee_alcr$DBH)
length(stee_alcr$DBH)

rm(dalt_alcr, stee_alcr)

# no total aboveground biomass equation

## component ######################
# 2 equations given, one for stems and one for leaves

# LEAVES
leaf_alcr <- function(x) exp(1.82 + 2.38*log(x))
# n = , r2 = 0.88
# SE = 0.276

alcr$leaf <- leaf_alcr(alcr$DBH)

# STEM
stem_alcr <- function(x) exp(4.5 + 2.30*log(x))
# n = , r2 = 0.88
# SE = 0.127

alcr$stem <- stem_alcr(alcr$DBH)

## graph ###########################

# summing up components
alcr$Biomass <- alcr$leaf + alcr$stem

ggplot(alcr, aes(x = DBH)) +
  geom_point(aes(y = Biomass, col = "red")) +
  geom_ribbon(aes(ymax = Biomass , ymin = Biomass, col = "red"), alpha = .3) +
  geom_vline(xintercept = c(0.2, 4.5), linetype = "dashed")   +
  geom_vline(xintercept = c(2,7), linetype = "dashed", col = "blue") +
  labs(x = "DBH (cm)", y = "Dry Biomass (g)", title = "Alnus Crispa (Total Above. Biomass)") +
  geom_text(mapping = aes(x = 0.7, y = 2500, label = "Observed Range"),
            angle = 90, size = 3, check_overlap = TRUE) +
  geom_text(mapping = aes(x = 2.5, y = 2500, label = "Equation Range"),
            angle = 90, size = 3, check_overlap = TRUE, col = "Blue")

# subtracting foliage for dead individuals
alcr$Biomass[alcr$CANOPY == 0] <- alcr$Biomass[alcr$CANOPY == 0] - alcr$leaf[alcr$CANOPY == 0]

alcr <- alcr %>%
  select(!c(leaf, stem))

# ALL ###################################################################
# testing out numbers (for those that work)

test <- rbind(pime, birch, potr, salix, alcr, poba)

ggplot(test, aes(x = as.factor(TREAT), y = Biomass)) + geom_point() + geom_jitter()

ggplot(test, aes(x = as.factor(TREAT), y = Biomass, fill = SITE)) + geom_boxplot() +
  labs(title = "Biomass of each tree between sites")

# checking outliers # just some big boys
test[which(test$Biomass > 60000),] # I've double checked both of these in the field notes

# right now, biomass in grams per tree

## Species #############################3
spp_biomass <- test %>%
  group_by(SITE, TREAT, QUAD, EXP_FACT, PLOT, SPP) %>%
  summarise(Biomass_sum = sum(Biomass)) %>%
  complete(expand(test, SPP))

spp_biomass$Biomass_sum[is.na(spp_biomass$Biomass_sum)] <- 0

write.csv(spp_biomass, here("data/output/spp_biomass_plot.csv"), row.names = F)


## Per Plot ####################################
plot_biomass <- test %>%
  group_by(SITE, TREAT, QUAD, EXP_FACT, PLOT) %>%
  summarise(Biomass_sum = sum(Biomass),
            Biomass_SD = sd(Biomass))

plot_biomass$Biomass_sum_plot <- NA

plot_biomass$Biomass_sum_plot[plot_biomass$QUAD== 1.0] <- plot_biomass$Biomass_sum[plot_biomass$QUAD == 1.0]*4
plot_biomass$Biomass_sum_plot[plot_biomass$QUAD== 2.0] <- plot_biomass$Biomass_sum[plot_biomass$QUAD == 2.0]*2
plot_biomass$Biomass_sum_plot[plot_biomass$QUAD== 0.2] <- plot_biomass$Biomass_sum[plot_biomass$QUAD == 0.2]*20

colnames(plot_biomass)

plot_biomass$biomass_m2 <- plot_biomass$Biomass_sum_plot / 400


# Exporting file ###############################

# output
write.csv(plot_biomass, here("data/output/overstory_biomass_plot.csv"), row.names = FALSE)

plot_biomass <- read.csv(here("data/output/overstory_biomass_plot.csv"))

# plotting biomass per plot between upland and lowland (in g/m2)
ggplot(plot_biomass, aes(x = as.factor(TREAT), y = biomass_m2, fill = SITE)) + geom_boxplot() +
  labs(title = "Total Overstory Biomass (live)", x = "Number of Fires", y = "Biomass (grams/m2)") +
  scale_fill_manual(name = "Site", labels = c("Upland", "Lowland"),
                    values = c("#99d8c9","#2ca25f"))


plot0x <- plot_biomass %>%
  filter(TREAT == 0) %>%
  ggplot(aes(x = as.factor(TREAT), y = biomass_m2, fill = SITE)) + geom_boxplot() +
  labs(title = "Total Overstory Biomass (Live)", x = " ", y = "Biomass (grams/m2)") +
  scale_fill_manual(name = "Site", labels = c("Upland", "Lowland"),
                    values = c("#99d8c9","#2ca25f")) +
  theme(legend.position = "none") +
  geom_hline(yintercept = 1500, linetype = "dashed", color = "grey")
plot0x

plot123x <- plot_biomass %>%
  filter(TREAT != 0) %>%
  ggplot(aes(x = as.factor(TREAT), y = biomass_m2, fill = SITE)) + geom_boxplot() +
  labs(title = "", x = "Number of Fires", y = "") +
  scale_fill_manual(name = "Site", labels = c("Upland", "Lowland"),
                    values = c("#99d8c9","#2ca25f")) +
  geom_hline(yintercept = 1500, linetype = "dashed", color = "grey")

plot_grid(plot0x, plot123x, rel_widths = c(0.75, 1.5)) # 575 by 275

reburn_p_biomass <-plot_biomass %>%
  filter(TREAT != 0)
summary(reburn_p_biomass$Biomass)

unburn_p_biomass <-plot_biomass %>%
  filter(TREAT == 0)
summary(unburn_p_biomass$Biomass)

mean(unburn_p_biomass$Biomass) / mean(reburn_p_biomass$Biomass)

# plotting only live biomass
live_plot_biomass <- test %>%
  filter(CANOPY != 0) %>%
  group_by(SITE, TREAT, QUAD, EXP_FACT, PLOT) %>%
  summarise(Biomass = sum(Biomass))

live_plot_biomass$Biomass_m2[live_plot_biomass$QUAD== 1.0] <- live_plot_biomass$Biomass[live_plot_biomass$QUAD == 1.0]/400
live_plot_biomass$Biomass_m2[live_plot_biomass$QUAD== 2.0] <- live_plot_biomass$Biomass[live_plot_biomass$QUAD == 2.0]/200
live_plot_biomass$Biomass_m2[live_plot_biomass$QUAD== 0.2] <- live_plot_biomass$Biomass[live_plot_biomass$QUAD == 0.2]/80

ggplot(live_plot_biomass, aes(x = as.factor(TREAT), y = Biomass_m2, fill = SITE)) + geom_boxplot() +
  labs(title = "Live Biomass", x = "Number of Fires", y = "Biomass (grams/m2)") +
  scale_fill_manual(name = "Site", labels = c("Upland", "Lowland"),
                    values = c("#99d8c9","#2ca25f"))
live_plot_biomass %>%
  filter(TREAT != 0) %>%
  ggplot(aes(x = as.factor(TREAT), y = Biomass_m2, fill = SITE)) + geom_boxplot() +
  labs(title = "Live Biomass across Reburns", x = "Number of Fires", y = "Biomass (grams/m2)") +
  scale_fill_manual(name = "Site", labels = c("Upland", "Lowland"),
                    values = c("#99d8c9","#2ca25f"))

# plotting according to species
spp_biomass <- test %>%
  group_by(SITE, TREAT, QUAD, EXP_FACT, PLOT, SPP) %>%
  summarise(Biomass = sum(Biomass))

spp_biomass$Biomass_m2[spp_biomass$QUAD== 1.0] <- spp_biomass$Biomass[spp_biomass$QUAD == 1.0]/400
spp_biomass$Biomass_m2[spp_biomass$QUAD== 2.0] <- spp_biomass$Biomass[spp_biomass$QUAD == 2.0]/200
spp_biomass$Biomass_m2[spp_biomass$QUAD== 0.2] <- spp_biomass$Biomass[spp_biomass$QUAD == 0.2]/80

ggplot(spp_biomass, aes(x = as.factor(TREAT), y = Biomass_m2, fill = SPP)) + geom_boxplot() + facet_wrap(~SITE) +
  labs(title = "Biomass summed across species within plots")

# live species biomass
live_spp_biomass <- test %>%
  filter(CANOPY != 0) %>%
  group_by(SITE, TREAT, QUAD, EXP_FACT, PLOT, SPP) %>%
  summarise(Biomass = sum(Biomass))

live_spp_biomass$Biomass_m2[live_spp_biomass$QUAD== 1.0] <- live_spp_biomass$Biomass[live_spp_biomass$QUAD == 1.0]/400
live_spp_biomass$Biomass_m2[live_spp_biomass$QUAD== 2.0] <- live_spp_biomass$Biomass[live_spp_biomass$QUAD == 2.0]/200
live_spp_biomass$Biomass_m2[live_spp_biomass$QUAD== 0.2] <- live_spp_biomass$Biomass[live_spp_biomass$QUAD == 0.2]/80

ggplot(live_spp_biomass, aes(x = as.factor(TREAT), y = Biomass_m2, fill = SPP)) + geom_boxplot() + facet_wrap(~SITE) +
  labs(title = "Biomass summed across species within plots")

# plotting without unburned controls
live_spp_biomass %>%
  filter(TREAT != 0) %>%
  ggplot(aes(x = as.factor(TREAT), y = Biomass_m2, fill = SPP)) + geom_boxplot() + facet_wrap(~SITE) +
  labs(title = "Live Species Biomass within reburned plots", y = "Biomass (g/m2)", x = "Number of Fires")

# # double checking stees unburned
# steese0 <- dbh %>%
#   filter(SITE == "STEESE") %>%
#   filter(TREAT == 0)
#
dalt0 <- dbh %>%
  filter(SITE == "DALTON") %>%
  filter(TREAT == 0)

dalt0[dalt0$DBH > 20,]
