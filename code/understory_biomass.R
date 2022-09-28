# Understory Biomass ####################################

# input: herb.csv
# output: data/output/understory_biomass_plot.csv

library(ggplot2)
library(cowplot); theme_set(theme_cowplot())
library(tidyverse)
library(here)
options(scipen = 999)

regen <- read.csv(here("data/herbivory/herb.csv")) # has heights

# filtering out everything below DBH
regen <- regen %>%
  filter(HEIGHT_M <1.37) %>%
  rename("BD" = "DBH_CM")

unique(regen$SPP)

# BIRCH ###########################################################################
# Berner et al. 2015

birch <- regen %>%
  filter(SPP == "BENE") %>%
  filter(BD < 5)
hist(birch$BD)
summary(birch$BD) # min 0.08, max 3.81

# 32_2 has 10.5 cm birch, 3.5 feet tall, dead - taking out of regen
# found typo in 40_2 BENE height, fixed
# found typo in 58_0 BENE height, fixed
# 33_1 has dead 5.49 cm birch, 2 feet tall - taking out

## total ########################

tab_bene <- function(x) 28.1*x^2.97
# n = 19, r2 = 0.98

birch$EQ_tab <- tab_bene(birch$BD)

## components ######################
# testing out calculating each component

# STEM
stem_bene <- function(x) 17.47*x^(2.36)
# n = 19, r2 = 0.89

birch$stem <- stem_bene(birch$BD)

# BRANCH
branch_bene <- function(x) 5.73*(x^4.06)
# n = 19, r2 = 0.98

birch$branch <- branch_bene(birch$BD)

# NEW GROWTH
growth_bene <- function(x) 4.57*(x^2.45)
# n = 10, r2 = 0.88

birch$growth <- growth_bene(birch$BD)

## graph #################################

# summing up components
birch$EQ_sum <-  birch$stem + birch$branch + birch$growth

birch <- birch %>%
  pivot_longer(
    cols = starts_with("EQ"),
    names_to = "EQ",
    values_to = "Biomass")

# subtracting foliage for dead individuals
# birch$Biomass[birch$CANOPY == 0] <- birch$Biomass[birch$CANOPY == 0] - birch$fol[birch$CANOPY == 0] - birch$crown[birch$CANOPY == 0]

ggplot(birch, aes(x = BD)) +
  geom_point(aes(y = Biomass, col = EQ)) +
  geom_ribbon(aes(ymax = Biomass, ymin = Biomass, fill = EQ), alpha = .3) +
  geom_vline(xintercept = c(0.1, 3.8), linetype = "dashed") +
  geom_vline(xintercept = c(0.09, 2.53), linetype = "dashed", col = "blue")  +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Sum of Components", "Tot. Above. Biomass"),
                     name = "Equation") +
  scale_fill_manual(values = c("red", "blue"),
                    labels = c("Sum of Components", "Tot. Above. Biomass"),
                    name = "Equation") +
  labs(x = "DBH (cm)", y = "Dry Biomass (g)", title = "Alaskan Birch (Total Above. Biomass)")
 # geom_text(mapping = aes(x = 1.5, y = 185000, label = "Observed Range"),
  #          angle = 90, size = 3, check_overlap = TRUE)
# geom_text(mapping = aes(x = 5, y = 185000, label = "Equation Range"),
#          angle = 90, size = 3, color = "Blue", check_overlap = TRUE)
# ylim(c(0, 225000))

birch <- birch %>%
  filter(EQ == "EQ_tab") %>%
  select(!c(branch, stem, growth))

# BLACK SPRUCE ############################################################################
# Alexander at al. 2012

pime <- regen %>%
  filter(SPP == "PIME")
hist(pime$BD)
summary(pime$BD) # min 0.05, max 8

## total ########################

# second equation in table
tab_pime <- function(x) 271.46*(x^1.84)
# n = 78, r2 = 0.92

pime$EQ_tab <- tab_pime(pime$BD)


##  components ######################
# testing out calculating each component

# STEMWOOD
stem_pime <- function(x) 117.91*(x^1.99)
# n = 78, r2 = 0.95

pime$stem <- stem_pime(pime$BD)

# FOLIAGE
fol_pime <- function(x) 55.4*x^(1.47)
# n = 56, r2 = 0.5

pime$fol <- fol_pime(pime$BD)

# LIVE CROWN
crown_pime <- function(x) 83.52*x^(1.8)
# n = 78, r2 = 0.72

pime$crown <- crown_pime(pime$BD)

# CROWN GROWTH
growth_pime <- function(x) 5.26*x^(1.55)
# n = 56, r2 = 0.5

pime$growth <- growth_pime(pime$BD)


## graph ###########################################

# summing up components
pime$EQ_sum <- pime$fol + pime$stem + pime$crown + pime$growth

# pivoting out
pime <- pime %>%
  pivot_longer(
    cols = starts_with("EQ"),
    names_to = "EQ",
    values_to = "Biomass")

ggplot(pime, aes(x = BD)) +
  geom_point(aes(y = Biomass, col = EQ)) +
  geom_ribbon(aes(ymax = Biomass, ymin = Biomass, fill = EQ), alpha = .3) +
  geom_vline(xintercept = c(0.1, 8), linetype = "dashed") +
  # geom_vline(xintercept = c(0, 12.8), linetype = "dashed", col = "blue")  +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Sum of Components", "Tot. Above. Biomass"),
                     name = "Equation") +
  scale_fill_manual(values = c("red", "blue"),
                    labels = c("Sum of Components", "Tot. Above. Biomass"),
                    name = "Equation") +
  labs(x = "BD (cm)", y = "Dry Biomass (g)", title = "Black Spruce (Total Above. Biomass)")
 # geom_text(mapping = aes(x = 1.5, y = 100000, label = "Observed Range"),
  #          angle = 90, size = 3, check_overlap = TRUE)
#geom_text(mapping = aes(x = 14, y = 100000, label = "Equation Range"),
#         angle = 90, size = 3, color = "Blue", check_overlap = TRUE)
# ylim(c(0, 110000))

# subtracting foliage for dead individuals
pime$Biomass[pime$CANOPY == 0] <- pime$Biomass[pime$CANOPY == 0] - pime$fol[pime$CANOPY == 0] - pime$crown[pime$CANOPY == 0]

pime <- pime %>%
  filter(EQ == "EQ_tab") %>%
  select(!c(fol, crown, stem, growth))

# ASPEN ############################################################################
# Alexander et al. 2012

potr <- regen %>%
  filter(SPP == "POTR")
hist(potr$BD)
summary(potr$BD) # min 0.08, max 2.6

## total ######################
tab_potr <- function(x) 134.1*x^(2.26)
# n = 40, r2 = 0.99
# SE

potr$EQ_tab <- tab_potr(potr$BD)

## component ######################
# calculating each component

# STEMWOOD/BARK
stem_potr <- function(x) 64.01*x^(2.51)
# n = 23, r2 = 0.98

potr$stem <- stem_potr(potr$BD)

# FOLIAGE
fol_potr <- function(x) 18.98*x^(1.53)
# n = 41,r2 = 0.91

potr$fol <- fol_potr(potr$BD)

# LIVE CROWN
crown_potr <- function(x)  41.74*x^(1.83)
# n = 21, r2 = 0.83

potr$crown <- crown_potr(potr$BD)

# CROWN GROWTH
growth_potr <- function(x) 10.24*x^(1.76)
# n = 23, r2 = 0.91

potr$growth <- growth_potr(potr$BD)

## graph ###################################

# summing up components
potr$EQ_sum <- potr$fol + potr$crown + potr$stem + potr$growth

potr <- potr %>%
  pivot_longer(
    cols = starts_with("EQ"),
    names_to = "EQ",
    values_to = "Biomass")

ggplot(potr, aes(x = BD)) +
  geom_point(aes(y = Biomass, col = EQ)) +
  geom_ribbon(aes(ymax = Biomass , ymin = Biomass , fill = EQ), alpha = .3) +
  geom_vline(xintercept = c(0.1, 6.5), linetype = "dashed")   +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Sum of Components", "Tot. Above. Biomass"),
                     name = "Equation") +
  scale_fill_manual(values = c("red", "blue"),
                    labels = c("Sum of Components", "Tot. Above. Biomass"),
                    name = "Equation") +
  labs(x = "BD (cm)", y = "Dry Biomass (g)", title = "Aspen (Total Above. Biomass)")
  # geom_text(mapping = aes(x = 0.45, y = 8000, label = "Observed Range"),
  #           angle = 90, size = 3, check_overlap = TRUE)
# geom_text(mapping = aes(x = 14, y = 100000, label = "Equation Range"),
#           angle = 90, size = 3, color = "Blue", check_overlap = TRUE)

# subtracting foliage for dead individuals
potr$Biomass[potr$CANOPY == 0] <- potr$Biomass[potr$CANOPY == 0] - potr$fol[potr$CANOPY == 0] - potr$crown[potr$CANOPY == 0]

potr <- potr %>%
  filter(EQ == "EQ_tab") %>%
  select(!c(fol, crown, stem, growth))


# POPLAR ############################################################################
# Alexander et al. 2012

poba <- regen %>%
  filter(SPP == "POBA")
hist(poba$BD) # 4 of these
summary(poba$BD) # min 0.3, 1.1

## total########################
# testing total aboveground biomass equations
# only testing those that only require BD

# first equation in table
tab_poba <- function(x) 13.31*(x^3.15)
# n = 17, r2 = 0.96

poba$EQ_tab <- tab_poba(poba$BD)


## component ######################
# testing out calculating each component

# STEMWOOD / BARK
stem_poba <- function(x) 4.28*x^(3.68)
# n = 17, r2 = 0.96

poba$stem <- stem_poba(poba$BD)

# BRANCH
branch_poba <- function(x) 2.23*x^(3.19)
# n = 17, r2 = 0.94

poba$branch <- branch_poba(poba$BD)

# GROWTH
growth_poba <- function(x) 10.88*x^(1.55)
# n = 15, r2 = 0.85

poba$growth <- growth_poba(poba$BD)

## graph ##########################

# summing up components
poba$EQ_sum <-  poba$branch + poba$stem + poba$growth

poba <- poba %>%
  pivot_longer(
    cols = starts_with("EQ"),
    names_to = "EQ",
    values_to = "Biomass")

ggplot(poba, aes(x = BD)) +
  geom_point(aes(y = Biomass, col = EQ)) +
  geom_ribbon(aes(ymax = Biomass , ymin = Biomass , fill = EQ), alpha = .3) +
  geom_vline(xintercept = c(0.3, 1.1), linetype = "dashed")   +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Sum of Components", "Tot. Above. Biomass"),
                     name = "Equation") +
  scale_fill_manual(values = c("red", "blue"),
                    labels = c("Sum of Components", "Tot. Above. Biomass"),
                    name = "Equation") +
  labs(x = "BD (cm)", y = "Dry Biomass (g)", title = "Balsam Poplar (Total Above. Biomass)")
  # geom_text(mapping = aes(x = 1.35, y = 900, label = "Observed Range"),
  #           angle = 90, size = 3, check_overlap = TRUE)

# subtracting foliage for dead individuals
# poba$Biomass[poba$CANOPY == 0] <- poba$Biomass[poba$CANOPY == 0] - poba$fol[poba$CANOPY == 0] - poba$crown[poba$CANOPY == 0]

poba <- poba %>%
  filter(EQ == "EQ_tab") %>%
  select(!c(branch, stem, growth))

# SALIX ##################################################################
# Berger

salix <- regen %>%
  filter(SPP == "SALIX")
hist(salix$BD)
summary(salix$BD) # min 0.8, max 2.46


## total ##########################
tab_salix <- function(x) 27.58*x^(2.36)
# n =10, r2 = 0.54

salix$EQ_tab <- tab_salix((salix$BD))

## components #################
    ## stem
    stem_salix <- function(x) 20.62*x^(2.29)
    # n = 10, r2 = 0.537

    salix$stem <- stem_salix(salix$BD)

    ## branch
    branch_salix <- function(x) 5.19*x^(2.37)
    # n = 10, r2 = 0.489

    salix$branch <- branch_salix(salix$BD)

    ## growth
     growth_salix <- function(x) 10.54*x^(1.71)
    # n = 10, r2 = 0.489

    salix$growth <- growth_salix(salix$BD)


## graph ###################

# summing up components
salix$EQ_sum <- salix$stem + salix$branch + salix$growth

salix <- salix %>%
  pivot_longer(
    cols = starts_with("EQ"),
    names_to = "EQ",
    values_to = "Biomass")

ggplot(salix, aes(x = BD)) +
  geom_point(aes(y = Biomass, col = EQ)) +
  geom_ribbon(aes(ymax = Biomass , ymin = Biomass , fill = EQ), alpha = .3) +
  geom_vline(xintercept = c(0.08, 2.46), linetype = "dashed")  +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Sum of Components", "Tot. Above. Biomass"),
                     name = "Equation") +
  scale_fill_manual(values = c("red", "blue"),
                    labels = c("Sum of Components", "Tot. Above. Biomass"),
                    name = "Equation") +
  labs(x = "BD (cm)", y = "Dry Biomass (g)", title = "Salix (Total Above. Biomass)")
# geom_text(mapping = aes(x = 1.35, y = 900, label = "Observed Range"),
#           angle = 90, size = 3, check_overlap = TRUE)

# subtracting foliage for dead individuals
# salix$Biomass[salix$CANOPY == 0] <- salix$Biomass[salix$CANOPY == 0] - salix$fol[salix$CANOPY == 0]

salix <- salix %>%
  filter(EQ == "EQ_sum") %>%
  select(!c(branch, stem, growth))

# ALDER #######################################






# ALL ###################################################################
# testing out numbers (for those that work)

test <- rbind(pime, birch, potr, poba, salix)

ggplot(test, aes(x = as.factor(TREAT), y = Biomass)) + geom_point() + geom_jitter()

ggplot(test, aes(x = as.factor(TREAT), y = Biomass, fill = SITE)) + geom_boxplot() +
  labs(title = "Biomass of each seedling between sites")

ggplot(test, aes(x = as.factor(TREAT), y = Biomass, fill = SPP)) + geom_boxplot() + facet_wrap(~SITE) +
  labs(title = "Biomass")

test[which(test$Biomass > 7500),] # one spruce with BD = 8 [double check] 33_1

test <- test %>%
  filter(Biomass <75000)

# right now, biomass in grams per tree
plot_biomass <- test %>%
  group_by(SITE, TREAT, PLOT) %>%
  summarise(Biomass = sum(Biomass))

### START HERE - ADD SUBPLOT SIZE FOR REGEN
  # adding in quandrants/m2
  plot_biomass$m2 <- 9 # default
      # exceptions:
      plot_biomass$m2[plot_biomass$PLOT == "15_3"] <- 25
      plot_biomass$m2[plot_biomass$PLOT == "17_3"] <- 25
      plot_biomass$m2[plot_biomass$PLOT == "32_2"] <- 100
      plot_biomass$m2[plot_biomass$PLOT == "52_1"] <- 100
      plot_biomass$m2[plot_biomass$PLOT == "40_2"] <- 100
      plot_biomass$m2[plot_biomass$PLOT == "55_3"] <- 100
      plot_biomass$m2[plot_biomass$PLOT == "10_0"] <- 100
      plot_biomass$m2[plot_biomass$PLOT == "33_1"] <- 100

# scaling to grams per meter2
      plot_biomass$g.m2 <- plot_biomass$Biomass / plot_biomass$m2

      plot_biomass$biomass_plot <- plot_biomass$g.m2 * 400

ggplot(plot_biomass, aes(x = as.factor(TREAT), y = g.m2, fill = SITE)) + geom_boxplot() +
  labs(title = "Understory Biomass")

plot0x <- plot_biomass %>%
  filter(TREAT == 0) %>%
  ggplot(aes(x = as.factor(TREAT), y = g.m2, fill = SITE)) +
  geom_boxplot() +
  scale_fill_manual(name = "Site", labels = c("Upland", "Lowland"),
                    values = c("#d9f0a3","#78c679")) +
  labs(title = "Understory Biomass", y = "Total Biomass (grams/m2)", x = "" ) +
  theme(legend.position = "none") +
  geom_hline(yintercept = 400, linetype = "dashed", color = "grey")
plot0x

plot123x <- plot_biomass %>%
  filter(TREAT != 0) %>%
  ggplot(aes(x = as.factor(TREAT), y = g.m2, fill = SITE)) +
  geom_boxplot() +
  scale_fill_manual(name = "Site", labels = c("Upland", "Lowland"),
                    values = c("#d9f0a3","#78c679")) +
  labs(title = " ", y = " ", x = "Number of Fires" ) +
  geom_hline(yintercept = 400, linetype = "dashed", color = "grey")
plot123x

plot_grid(plot0x, plot123x, rel_widths = c(0.75, 1.5))

# Exporting file ##################################

# averaging dalon 2 for 47_2
# averaging steese 2 for 34_2

plot_biomass %>%
  group_by(SITE, TREAT) %>%
  summarise(mean(Biomass), mean(biomass_plot), max(m2), mean(g.m2)) %>%
  filter(TREAT == 2) %>%
  ungroup()

plot_biomass <- plot_biomass %>%
  ungroup() %>%
  add_row(SITE = "DALTON", TREAT = 2, PLOT = "47_2",
          Biomass = 7213, biomass_plot = 122491, m2 = NA, g.m2 = 306) %>%
  add_row(SITE = "STEESE", TREAT = 2, PLOT = "34_2",
          Biomass = 14040, biomass_plot = 623980, m2 = 9, g.m2 = 1560)
# output
write.csv(plot_biomass, here("data/output/understory_biomass_plot.csv"), row.names = FALSE)
