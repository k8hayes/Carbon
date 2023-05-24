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

library(cowplot); theme_set(theme_cowplot())
library(tidyverse)
library(here)
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

  tab_bene <- function(x) 164.18*x^2.29
  # n = 19, r2 = 0.98

  birch$EQ_tab <- tab_bene(birch$DBH)

## components ######################
# testing out calculating each component

  # STEMWOOD / BARK
   stemw_bene <- function(x) 147.96*x^(2.25)
  # n = 19, r2 = 0.98

   birch$stem <- stemw_bene(birch$DBH)

   # FOLIAGE
   fol_bene  <- function(x) 6.39*x^2.1
   # n = 19, r2 = 0.89

   birch$fol <- fol_bene(birch$DBH)

  # LIVE CROWN
  crown_bene <- function(x) 15.15*(x^2.49)
  # n = 19, r2 = 0.94

  birch$crown <- crown_bene(birch$DBH)

  # CROWN GROWTH
   growth_bene <- function(x) 10.47*(x^1.92)
    # n = 10, r2 = 0.88

     birch$growth <- growth_bene(birch$DBH)

## graph #################################

# summing up components
 birch$EQ_sum <- birch$fol + birch$stem + birch$crown + birch$growth

birch <- birch %>%
  pivot_longer(
    cols = starts_with("EQ"),
    names_to = "EQ",
    values_to = "Biomass")

 # subtracting foliage for dead individuals
 birch$Biomass[birch$CANOPY == 0] <- birch$Biomass[birch$CANOPY == 0] - birch$fol[birch$CANOPY == 0] - birch$crown[birch$CANOPY == 0]

ggplot(birch, aes(x = DBH)) +
  geom_point(aes(y = Biomass, col = EQ)) +
  geom_ribbon(aes(ymax = Biomass, ymin = Biomass, fill = EQ), alpha = .3) +
  geom_vline(xintercept = c(0.1, 23.5), linetype = "dashed") +
  geom_vline(xintercept = c(3.7, 32.7), linetype = "dashed", col = "blue")  +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Sum of Components", "Tot. Above. Biomass"),
                     name = "Equation") +
  scale_fill_manual(values = c("red", "blue"),
                    labels = c("Sum of Components", "Tot. Above. Biomass"),
                    name = "Equation") +
  labs(x = "DBH (cm)", y = "Dry Biomass (g)", title = "Alaskan Birch (Total Above. Biomass)") +
  geom_text(mapping = aes(x = 1.5, y = 185000, label = "Observed Range"),
            angle = 90, size = 3, check_overlap = TRUE) +
 geom_text(mapping = aes(x = 5, y = 185000, label = "Equation Range"),
            angle = 90, size = 3, color = "Blue", check_overlap = TRUE) +
   ylim(c(0, 225000))

birch <- birch %>%
  filter(EQ == "EQ_tab") %>%
  select(!c(fol, crown, stem, growth, EQ))

# BLACK SPRUCE ############################################################################
# Alexander at al. 2012

pime <- dbh %>%
  filter(SPP == "PIME")
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
  tab_pime <- function(x) 271.46*(x^1.84)
  # n = 78, r2 = 0.92

  pime$EQ_tab <- tab_pime(pime$DBH)


##  components ######################
# testing out calculating each component

  # STEMWOOD
   stem_pime <- function(x) 117.91*(x^1.99)
  # n = 78, r2 = 0.95

    pime$stem <- stem_pime(pime$DBH)

  # FOLIAGE
   fol_pime <- function(x) 55.4*x^(1.47)
   # n = 56, r2 = 0.5

  pime$fol <- fol_pime(pime$DBH)

  # LIVE CROWN
   crown_pime <- function(x) 83.52*x^(1.8)
  # n = 78, r2 = 0.72

   pime$crown <- crown_pime(pime$DBH)

   # CROWN GROWTH
   growth_pime <- function(x) 5.26*x^(1.55)
   # n = 56, r2 = 0.5

   pime$growth <- growth_pime(pime$DBH)


## graph ###########################################

# summing up components
   pime$EQ_sum <- pime$fol + pime$stem + pime$crown + pime$growth

# pivoting out
  pime <- pime %>%
    pivot_longer(
      cols = starts_with("EQ"),
      names_to = "EQ",
      values_to = "Biomass")

  ggplot(pime, aes(x = DBH)) +
    geom_point(aes(y = Biomass / 2, col = EQ)) +
    geom_ribbon(aes(ymax = Biomass, ymin = Biomass, fill = EQ), alpha = .3) +
    geom_vline(xintercept = c(0.1, 26), linetype = "dashed") +
    geom_vline(xintercept = c(0, 12.8), linetype = "dashed", col = "blue")  +
    scale_color_manual(values = c("red", "blue"),
                       labels = c("Sum of Components", "Tot. Above. Biomass"),
                       name = "Equation") +
    scale_fill_manual(values = c("red", "blue"),
                      labels = c("Sum of Components", "Tot. Above. Biomass"),
                      name = "Equation") +
    labs(x = "DBH (cm)", y = "Dry Biomass (g)", title = "Black Spruce (Total Above. Biomass)") +
    geom_text(mapping = aes(x = 1.5, y = 100000, label = "Observed Range"),
              angle = 90, size = 3, check_overlap = TRUE) +
    geom_text(mapping = aes(x = 14, y = 100000, label = "Equation Range"),
              angle = 90, size = 3, color = "Blue", check_overlap = TRUE)

  # subtracting foliage for dead individuals
  pime$Biomass[pime$CANOPY == 0] <- pime$Biomass[pime$CANOPY == 0] - pime$fol[pime$CANOPY == 0] - pime$crown[pime$CANOPY == 0]

pime <- pime %>%
  filter(EQ == "EQ_tab") %>%
  select(!c(fol, crown, stem, growth, EQ))

# ASPEN ############################################################################
# Alexander et al. 2012

  # NOTE: Alexander et al. 2012 only published mean tree dbh (cm), not a range
    # for aspen, that's 2 cm

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
  tab_potr <- function(x) 134.1*x^(2.26)
  # n = 40, r2 = 0.99
  # SE

  potr$EQ_tab <- tab_potr(potr$DBH)

  ## component ######################
  # calculating each component

  # STEMWOOD/BARK
  stem_potr <- function(x) 64.01*x^(2.51)
  # n = 23, r2 = 0.98

  potr$stem <- stem_potr(potr$DBH)

  # FOLIAGE
  fol_potr <- function(x) 18.98*x^(1.53)
  # n = 41,r2 = 0.91

  potr$fol <- fol_potr(potr$DBH)

  # LIVE CROWN
   crown_potr <- function(x)  41.74*x^(1.83)
  # n = 21, r2 = 0.83

  potr$crown <- crown_potr(potr$DBH)

  # CROWN GROWTH
  growth_potr <- function(x) 10.24*x^(1.76)
  # n = 23, r2 = 0.91

  potr$growth <- growth_potr(potr$DBH)

  ## graph ###################################

  # summing up components
  potr$EQ_sum <- potr$fol + potr$crown + potr$stem + potr$growth

 potr <- potr %>%
    pivot_longer(
      cols = starts_with("EQ"),
      names_to = "EQ",
      values_to = "Biomass")

 ggplot(potr, aes(x = DBH)) +
   geom_point(aes(y = Biomass, col = EQ)) +
   geom_ribbon(aes(ymax = Biomass , ymin = Biomass , fill = EQ), alpha = .3) +
   geom_vline(xintercept = c(0.1, 6.5), linetype = "dashed")   +
   scale_color_manual(values = c("red", "blue"),
                      labels = c("Sum of Components", "Tot. Above. Biomass"),
                      name = "Equation") +
   scale_fill_manual(values = c("red", "blue"),
                     labels = c("Sum of Components", "Tot. Above. Biomass"),
                     name = "Equation") +
   labs(x = "DBH (cm)", y = "Dry Biomass (g)", title = "Aspen (Total Above. Biomass)") +
   geom_text(mapping = aes(x = 0.45, y = 8000, label = "Observed Range"),
             angle = 90, size = 3, check_overlap = TRUE)

 # NOTE: Alexander et al. 2012 only published mean tree dbh (cm), not a range

 # subtracting foliage for dead individuals
 potr$Biomass[potr$CANOPY == 0] <- potr$Biomass[potr$CANOPY == 0] - potr$fol[potr$CANOPY == 0] - potr$crown[potr$CANOPY == 0]

 potr <- potr %>%
   filter(EQ == "EQ_tab") %>%
   select(!c(fol, crown, stem, growth, EQ))


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
  tab_poba <- function(x) 133.71*(x^2.29)
  # n = 17, r2 = 0.96

  poba$EQ_tab <- tab_poba(poba$DBH)

## component ######################
# testing out calculating each component

  # STEMWOOD / BARK
  stem_poba <- function(x) 98.26*x^(2.32)
  # n = 17, r2 = 0.96

  poba$stem <- stem_poba(poba$DBH)

  # FOLIAGE
    fol_poba <- function(x) 22.16*x^(1.62)
    # n = 17, r2 = 0.84

    poba$fol <- fol_poba(poba$DBH)

  # LIVE CROWN
    crown_poba <- function(x) 17.24*x^(2.3)
    # n = 17, r2 = 0.94

    poba$crown <- crown_poba(poba$DBH)

    # CROWN GROWTH
    growth_poba <- function(x) 27.14*x^(1.60)
    # n = 15, r2 = 0.85

    poba$growth <- growth_poba(poba$DBH)

## graph ##########################

    # summing up components
    poba$EQ_sum <- poba$fol + poba$crown + poba$stem + poba$growth

    poba <- poba %>%
      pivot_longer(
        cols = starts_with("EQ"),
        names_to = "EQ",
        values_to = "Biomass")

    ggplot(poba, aes(x = DBH)) +
      geom_point(aes(y = Biomass, col = EQ)) +
      geom_ribbon(aes(ymax = Biomass , ymin = Biomass , fill = EQ), alpha = .3) +
      geom_vline(xintercept = c(1.3, 2.3), linetype = "dashed")   +
      scale_color_manual(values = c("red", "blue"),
                         labels = c("Sum of Components", "Tot. Above. Biomass"),
                         name = "Equation") +
      scale_fill_manual(values = c("red", "blue"),
                        labels = c("Sum of Components", "Tot. Above. Biomass"),
                        name = "Equation") +
      labs(x = "DBH (cm)", y = "Dry Biomass (g)", title = "Balsam Poplar (Total Above. Biomass)") +
      geom_text(mapping = aes(x = 1.35, y = 900, label = "Observed Range"),
                angle = 90, size = 3, check_overlap = TRUE)

    # subtracting foliage for dead individuals
    poba$Biomass[poba$CANOPY == 0] <- poba$Biomass[poba$CANOPY == 0] - poba$fol[poba$CANOPY == 0] - poba$crown[poba$CANOPY == 0]

poba <- poba %>%
  filter(EQ == "EQ_tab") %>%
  select(!c(fol, crown, stem, growth, EQ))

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
  tab_salix <- function(x) 10^(rnorm(length(salix$DBH),2.481, 0.043) + rnorm(length(salix$DBH),1.190, 0.043)*(log10(x)))
  # n =10, r2 = 0.54

  salix$EQ_tab <- tab_salix((salix$DBH))
  hist(salix$EQ_tab)

## components #################
  ## stem
    stem_salix <- function(x) 10^(rnorm(length(salix$DBH),2.167, 0.033) + rnorm(length(salix$DBH),1.03, 0.033)*(log10(x)))
    # n = 10, r2 = 0.537

    salix$stem <- stem_salix(salix$DBH)

  ## total branch ## also includes new vs old branch, but not including here
  branch_salix <- function(x) 10^(rnorm(length(salix$DBH),2.021, 0.08) + rnorm(length(salix$DBH),1.44, 0.08)*(log10(x)))
  # n = 10, r2 = 0.489

  salix$branch <- branch_salix(salix$DBH)

  ## foliage
  fol_salix <- function(x, b) 10^(rnorm(length(salix$DBH),2.023, 0.037) + rnorm(length(salix$DBH),1.065, 0.037)*(log10(x)) + rnorm(length(salix$DBH),-0.041, 0.037)*b)
  # n = 2, r2 = 0.8.31
  # where b = stand age
  # Just going to call it 20 right now and leave it

  salix$fol <- fol_salix(salix$DBH, 20) # should add in rnorm(mean age, sd)

  ## wood
  wood_salix <- function(x) 10^(rnorm(length(salix$DBH), 2.404, 0.038) + rnorm(length(salix$DBH), 1.172, 0.038)*(log10(x)))
  # n - 2, r2 = 0.562

  salix$wood <- wood_salix(salix$DBH)

## graph ###################

  # summing up components
  salix$EQ_sum <- salix$stem + salix$branch + salix$fol + salix$wood

  salix <- salix %>%
    pivot_longer(
      cols = starts_with("EQ"),
      names_to = "EQ",
      values_to = "Biomass")

  ggplot(salix, aes(x = DBH)) +
    geom_point(aes(y = Biomass, col = EQ)) +
    geom_ribbon(aes(ymax = Biomass , ymin = Biomass , fill = EQ), alpha = .3) +
    geom_vline(xintercept = c(0.1, 8.1), linetype = "dashed")  +
    geom_vline(xintercept = c(0.3, 1), linetype = "dashed", col = "blue")  +
    scale_color_manual(values = c("red", "blue"),
                       labels = c("Sum of Components", "Tot. Above. Biomass"),
                       name = "Equation") +
    scale_fill_manual(values = c("red", "blue"),
                      labels = c("Sum of Components", "Tot. Above. Biomass"),
                      name = "Equation") +
     labs(x = "DBH (cm)", y = "Dry Biomass (g)", title = "Salix (Total Above. Biomass)")
    # geom_text(mapping = aes(x = 1.35, y = 900, label = "Observed Range"),
    #           angle = 90, size = 3, check_overlap = TRUE)

  # subtracting foliage for dead individuals
  salix$Biomass[salix$CANOPY == 0] <- salix$Biomass[salix$CANOPY == 0] - salix$fol[salix$CANOPY == 0]

  salix <- salix %>%
    filter(EQ == "EQ_sum") %>%
    select(!c(fol, branch, stem, wood, EQ))

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

test <- rbind(pime, birch, potr, poba, salix, alcr)

  ggplot(test, aes(x = as.factor(TREAT), y = Biomass)) + geom_point() + geom_jitter()

  ggplot(test, aes(x = as.factor(TREAT), y = Biomass, fill = SITE)) + geom_boxplot() +
    labs(title = "Biomass of each tree between sites")

 # checking outliers # just some big boys
 test[which(test$Biomass > 150000),] # I've double checked both of these in the field notes

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
    labs(title = "Total Overstory Biomass (live + dead)", x = "Number of Fires", y = "Biomass (grams/m2)") +
    scale_fill_manual(name = "Site", labels = c("Upland", "Lowland"),
                      values = c("#99d8c9","#2ca25f"))


   plot0x <- plot_biomass %>%
     filter(TREAT == 0) %>%
     ggplot(aes(x = as.factor(TREAT), y = Biomass_m2, fill = SITE)) + geom_boxplot() +
     labs(title = "Total Overstory Biomass (live + dead)", x = " ", y = "Biomass (grams/m2)") +
     scale_fill_manual(name = "Site", labels = c("Upland", "Lowland"),
                       values = c("#99d8c9","#2ca25f")) +
     theme(legend.position = "none") +
     geom_hline(yintercept = 1500, linetype = "dashed", color = "grey")
   plot0x

   plot123x <- plot_biomass %>%
     filter(TREAT != 0) %>%
     ggplot(aes(x = as.factor(TREAT), y = Biomass_m2, fill = SITE)) + geom_boxplot() +
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
