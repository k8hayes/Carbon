# testing Yarie et al. 2007
# species: birch, spruce, aspen, poplar
# birch and spruce really the 2 that work

library(ggplot2)
library(cowplot); theme_set(theme_cowplot())
library(tidyverse)
library(here)
options(scipen = 999)

dbh <- read.csv(here("data/dbh.csv"))

#############################################################################
# starting off just with birch

birch <- dbh %>%
  filter(SPP == "BENE")
hist(birch$DBH) # big stuff (dbh > 18) is all unburned sites, all dead
summary(birch$DBH)

##########################
# testing total aboveground biomass equations
# only testing those that only require dbh

# first equation in table # UPDATE - this one gives negative values, even for the 3.7 dbh (which was the yarie minimum)
# EQ_tab1_bene <- function(x) -16809 + (2656.482*x) + (284.416*(x^2))
# n = 19, r2 = 0.98
# SE
  # intercept = 10934
  # dbh = 1462.65
  # dbh2 = 42.72
    # tab1_beneSE <- sqrt((10934^2) + (1462.65^2) + (42.72^2))

# second equation in table
 tab2_bene <- function(x) 361.738*(x^2)
# n = 19, r2 = 0.987
# SE
  # dbh2 = 9.89
    tab2_beneSE <- sqrt((9.89)^2)

#birch$tab1 <- EQ_tab1_bene(birch$DBH)
birch$Biomass <- tab2_bene(birch$DBH)
birch$SE <- tab2_beneSE

# birch$SE_tab1 <- tab1_beneSE
# birch$SE_tab2 <- tab2_beneSE

 # plot(birch$EQ_tab1 ~ birch$DBH)
 # points(birch$tab2 ~ birch$DBH, col = "red")
 # abline(v = c(0.1,23.5), col = "red") # my ranges
 # abline(v = c(3.7, 32.7), col = "blue", lty = 2) # equation's ranges

# testing out how to do this in ggplot
# ggplot(birch, aes(x = DBH)) +
#   geom_point(aes(y = tab1)) +
#   geom_point(aes(y = tab2, col = "red")) +
#   geom_ribbon(aes(ymin = tab1 -tab1_beneSE,
#                   ymax = tab1 + tab1_beneSE,
#                   alpha = 0.3)) +
#   geom_vline(xintercept = c(0.1, 23.5), linetype = "dashed") +
#   geom_vline(xintercept = c(3.7, 32.7), linetype = "dashed", col = "blue") +
#   geom_ribbon(aes(ymin = tab2 - tab2_beneSE,
#                   ymax = tab2 + tab2_beneSE,
#                   alpha = 0.3, col = "red")) +
#   theme(legend.position = "none") +
#   labs(x = "DBH (cm)", y = "Dry Biomass (g)", title = "Alaskan Birch (Total Above. Biomass)")

# seems like second equation (red) might over-estimate smaller trees

########################
# testing out calculating each component

# FOLIAGE
#fol1_bene <- function(x) -1494.788 + (331.746*x) - 4.784*(x^2)
# n = 19, r2 = 0.776
# SE
  # intercept = 612.75
  # dbh = 81.96
  # dbh2 = 2.39
    # fol1_beneSE <- sqrt((612.76)^2 + (81.96)^2 + (2.39)^2)

# fol2_bene  <- function(x) 145.892*x
# n = 19, r2 = 0.877
# SE
  # dbh = 12.89
    # fol2_beneSE <- sqrt(12.89^2)

#birch$fol1 <- fol1_bene(birch$DBH)
#birch$fol2 <- fol2_bene(birch$DBH)

# LIVE BRANCHES
# branch1_bene <- function(x) 3322.457 - 883.631*x + 92.016*(x^2)
# n = 19, r2 = 0.927
# SE
  # intercept = 4050.06
  # dbh = 541.76
  # dbh2 = 15.83
    # branch1_beneSE <- sqrt((4050.06)^2 + (541.76)^2 + (15.83)^2)

#branch2_bene <- function(x) -472.032*x + 81.536*(x^2)
# n = 19, r2 = 0.949
# SE
  # dbh = 202.38
  # dbh2 = 9.25
  #  branch2_beneSE <- sqrt((202.38)^2  + (9.25)^2)

# birch$branch1 <- branch1_bene(birch$DBH)
# birch$branch2 <- branch2_bene(birch$DBH)

# STEMWOOD
# stemw1_bene <- function(x) -30966 + 5360.216*x + 90.984*(x^2)
# n = 11, r2 = 0.967
# SE
  # intercept = 16521
  # dbh = 2054.69
  # dbh2 = 55.98
    # stemw1_beneSE <- sqrt((16521)^2 + (2054.69)^2 + (55.98)^2)

#stemw2_bene <- function(x) 1783.003*x + 177.304*(x^2)
# n = 11, r2 = 0.978
  # SE
    # dbh = 860.5
    # dbh2 = 35.99
     # stemw2_beneSE <- sqrt(860.5^2 + 35.99^2)

# birch$stem1 <- stemw1_bene(birch$DBH)
#birch$stem2 <- stemw2_bene(birch$DBH)

# BARK
# bark1_bene <- function(x) -1044.436 + 50.747*(x^2)
# n = 10, r2 = 0.989
# SE
  # intercept = 753.22
  # dbh2 = 1.86
   # bark1_beneSE <- sqrt(753.22^2 + 1.86^2)

#bark2_bene <- function(x) 49.01*(x^2)
# n = 10, r2 = 0.992
# SE
  # dbh2 = 1.44
   # bark2_beneSE <- sqrt(1.44^2)

# birch$bark1 <- bark1_bene(birch$DBH)
# birch$bark2 <- bark2_bene(birch$DBH)

# summing up components
# birch$sum1 <- birch$fol1 + birch$branch1 + birch$stem1 + birch$bark1
# birch$sum2 <- birch$fol2 + birch$branch2 + birch$stem2 + birch$bark2
# birch$EQ_sumR <- birch$fol2 + birch$branch2 + birch$stem2 + birch$bark2

# SE_sum1 <- sqrt(fol1_beneSE^2 + branch1_beneSE^2 + stemw1_beneSE^2 + bark1_beneSE^2)
# sumR_beneSE <- sqrt(fol2_beneSE^2 + branch2_beneSE^2 + stemw2_beneSE^2 + bark2_beneSE^2)

# plot(birch$sum1 ~ birch$DBH)
# points(birch$sum2 ~ birch$DBH, col = "green")
# points(birch$tab1 ~ birch$DBH, col = "red")
# points(birch$tab2 ~ birch$DBH, col = "blue")

# birch <- birch %>%
#   pivot_longer(
#     cols = starts_with("EQ"),
#     names_to = "EQ",
#     values_to = "Biomass")

# clean up variable name
# birch$EQ[birch$EQ == "EQ_tab1"] <- "tab1"
# birch$EQ[birch$EQ == "EQ_sumR"] <- "sumR"

# adding in standard errors
# birch$SE <- NA
# birch$SE[birch$EQ == "tab1"] <- tab1_beneSE
# birch$SE[birch$EQ == "sumR"] <- sumR_beneSE

# birch <- subset(birch, select = -c(fol1, fol2, bark1, bark2, stem1, stem2, branch1, branch2))
# birch <- subset(birch, select = -c(fol2, bark2, stem2, branch2))

ggplot(birch, aes(x = DBH)) +
  geom_point(aes(y = Biomass)) +
  geom_ribbon(aes(ymax = Biomass + SE, ymin = Biomass - SE ), alpha = .3) +
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
            angle = 90, size = 3, color = "Blue", check_overlap = TRUE)

#############################################################################
# black spruce

pime <- dbh %>%
  filter(SPP == "PIME")
hist(pime$DBH) # 2 trees over 20, one large one in unburned site, one dead one in once-burned
summary(pime$DBH)

##########################
# testing total aboveground biomass equations
# only testing those that only require dbh

  # first equation in table
  # tab1_pime <- function(x) 493.254  + 190.364*(x^2)
  # n = 78, r2 = 0.81
    # SE
      # intercept = 613.26
      # dbh2 = 10.59
      # tab1_pimeSE <- sqrt((613.26)^2 + (10.59)^2)

  # second equation in table
  tab2_pime <- function(x)  358.352*x + 158.166*(x^2)
  # n = 78, r2 = 0.905
    # SE
      # dbh = 240.81
      # dbh2 = 26.75
     tab2_pimeSE <- sqrt((240.81)^2 + (26.75)^2)

  # pime$tab1 <- tab1_pime(pime$DBH)
  pime$Biomass <- tab2_pime(pime$DBH)
  pime$SE <- tab2_pimeSE

# may eventually get rid of:

  # plot(pime$tab1 ~ pime$DBH)
  # points(pime$tab2 ~ pime$DBH, col = "red")
  # abline(v = c(0.1,26), col = "red") # my ranges
  # abline(v = c(0, 12.8), col = "blue", lty = 2) # equation's ranges

  # ggplot(pime, aes(x = DBH)) +
  #   geom_point(aes(y = tab1)) +
  #   geom_point(aes(y = tab2, col = "red")) +
  #   geom_ribbon(aes(ymin = tab1 -tab1_pimeSE,
  #                   ymax = tab1 + tab1_pimeSE,
  #                   alpha = 0.5)) +
  #   geom_vline(xintercept = c(0.1, 26), linetype = "dashed") +
  #   geom_vline(xintercept = c(0, 12.8), linetype = "dashed", col = "blue") +
  #   geom_ribbon(aes(ymin = tab2 - tab2_pimeSE,
  #                   ymax = tab2 + tab2_pimeSE,
  #                   alpha = 0.5, col = "red")) +
  #   labs(x = "DBH (cm)", y = "Dry Biomass (g)", main = "Black Spruce")

# may look for equations with larger ranges - these really diverge past dbh = 13

########################
# testing out calculating each component

  # FOLIAGE
  # fol1_pime <- function(x) -27.774 + (157.489*x)
  # n = 56, r2 = 0.433 # pretty bad
    # SE
      # intercept = 131.24
      # dbh = 24.53

 #  fol2_pime  <- function(x) 161.583*x
 #  # n = 56, r2 = 0.680
 #    # SE
 #    # dbh = 14.95
 #    fol2_pimeSE <- sqrt(14.95^2)
 #
 #  # pime$fol1 <- fol1_pime(pime$DBH)
 #  pime$fol2 <- fol2_pime(pime$DBH)
 #
 #  # LIVE BRANCHES
 #  # branch1_pime <- function(x) -176.986 + 188.234*x
 #  # n = 56, r2 = 0.518 # also not great
 #    # SE
 #      # intercept = 132.25
 #      # dbh = 24.72
 #
 #  branch2_pime <- function(x) 98.535*x + 7.693*(x^2)
 #  # n = 56, r2 = 0.685
 #    # SE
 #      # dbh = 43.83
 #      # dbh2 = 4.98
 #        branch2_pimeSE <- sqrt(43.83^2 + 4.98^2)
 #
 #  # pime$branch1 <- branch1_pime(pime$DBH)
 #  pime$branch2 <- branch2_pime(pime$DBH)
 #
 #  # STEMWOOD
 # # stemw1_pime <- function(x) -726.276 + 142.399*(x^2)
 #  # n = 9, r2 = 0.994
 #    # SE
 #      # intercept = 16521
 #      # dbh = 2054.69
 #      # dbh2 = 55.98
 #
 #  stemw2_pime <- function(x) -267.688*x + 159.822*(x^2)
 #  # n = 9, r2 = 0.998
 #    # SE
 #      # dbh = 128.39
 #      # dbh2 = 11.34
 #        stemw2_pimeSE <- sqrt(128.39^2 + 11.34^2)
 #
 #  # pime$stem1 <- stemw1_pime(pime$DBH)
 #  pime$stem2 <- stemw2_pime(pime$DBH)
 #
 #  # BARK
 #  # bark1_pime <- function(x) 62.498 + 25.647*(x^2)
 #  # n = 9, r2 = 0.996
 #    # SE
 #      # intercept = 417.39
 #      # dbh2 = 4.16
 #
 #  bark2_pime <- function(x) 26.134*(x^2)
 #  # n = 9, r2 = 0.998
 #    # SE
 #      # dbh2 = 0.4
 #  bark2_pimeSE <- sqrt(0.4^2)
 #
 #  # pime$bark1 <- bark1_pime(pime$DBH)
 #  pime$bark2 <- bark2_pime(pime$DBH)

# summing up components
# pime$sum1 <- pime$fol1 + pime$branch1 + pime$stem1 + pime$bark1
# pime$sum2 <- pime$fol2 + pime$branch2 + pime$stem2 + pime$bark2
  # pime$EQ_sumR <- pime$fol2 + pime$branch2 + pime$stem2 + pime$bark2

  # sumR_pimeSE <- sqrt(fol2_pimeSE^2 + branch2_pimeSE^2 + stemw2_pimeSE^2 +  bark2_pimeSE^2)

  # pivoting out
  # pime <- pime %>%
    # pivot_longer(
    #   cols = starts_with("EQ"),
    #   names_to = "EQ",
    #   values_to = "Biomass")

  # clean up variable name
  # pime$EQ[pime$EQ == "EQ_tab2"] <- "tab2"
  # pime$EQ[pime$EQ == "EQ_sumR"] <- "sumR"
  #
  # # adding in standard errors
  # pime$SE <- NA
  # pime$SE[pime$EQ == "tab2"] <- tab2_pimeSE
  # pime$SE[pime$EQ == "sumR"] <- sumR_pimeSE

  # pime <- subset(pime, select = -c(fol1, fol2, bark1, bark2, stem1, stem2, branch1, branch2))
  #pime <- subset(pime, select = -c(fol2, bark2, stem2, branch2))

  ggplot(pime, aes(x = DBH)) +
    geom_point(aes(y = Biomass, col = EQ)) +
    geom_ribbon(aes(ymax = Biomass + SE, ymin = Biomass - SE, fill = EQ), alpha = .3) +
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
              angle = 90, size = 3, color = "Blue", check_overlap = TRUE) +
    ylim(c(0, 110000))

#############################################################################
# quaking aspen

potr <- dbh %>%
  filter(SPP == "POTR")
hist(potr$DBH) # everything smaller than 6 cm
summary(potr$DBH) # min 0.1, max 6.5

##########################
# testing total aboveground biomass equations
# only testing those that only require dbh

# first equation in table
tab1_potr <- function(x) 4768.253 - (3033.186*x) + 497.269*(x^2)
# n = 41, r2 = 0.974
  # SE
    # intercept = 2066.15
    # DBH = 618.83
    # dbh2 = 34.58

# second equation in table
tab2_potr <- function(x) -1834.45*x + 439.664*(x^2)
# n = 41, r2 = 0.980
  # SE
    # dbh = 354.54
    # dbh2 = 25.23

potr$tab1 <- tab1_potr(potr$DBH)
potr$tab2 <- tab2_potr(potr$DBH)

plot(potr$tab1 ~ potr$DBH)
points(potr$tab2 ~ potr$DBH, col = "red")
abline(v = c(0.5,6.5), col = "red") # my ranges
abline(v = c(0.6, 19.3), col = "blue", lty = 2) # equation's ranges

# don't understand this at all

########################
# testing out calculating each component

# FOLIAGE
fol1_potr <- function(x) -31.892 + 6.226*(x^2)
# n = 41, r2 = 0.903
  # SE
    # intercept = 41.27
    # dbh2 = 0.33

fol2_potr  <- function(x) 6.387*(x^2)
# n = 31, r2 = 0.942
  # SE
    # dbh2 = 0.25

potr$fol1 <- fol1_potr(potr$DBH)
potr$fol2 <- fol2_potr(potr$DBH)

# LIVE BRANCHES
branch1_potr <- function(x) 2102.347 - 722.293*x + 68.792*(x^2)
# n = 23, r2 = 0.770
  # SE
    # intercept = 1680.90
    # dbh = 353.52
    # dbh2 = 17.13

branch2_potr <- function(x) -325.581*x + 52.188*(x^2)
# n = 23, r2 = 0.867
  # SE
    # dbh = 158.19
    # dbh2 = 10.98

potr$branch1 <- branch1_potr(potr$DBH)
potr$branch2 <- branch2_potr(potr$DBH)

# STEMWOOD
stemw1_potr <- function(x) 3842.733 - 2388.907*x + 370.644*(x^2)
# n = 39, r2 = 0.975
  # SE
    # intercept = 1627.52
    # dbh = 469.52
    # dbh2 = 25.87

stemw2_potr <- function(x) -1445.992*x + 325.786*(x^2)
# n = 39, r2 = 0.981
  # SE
    # dbh = 261.73
    # dbh2 = 18.62

potr$stem1 <- stemw1_potr(potr$DBH)
potr$stem2 <- stemw2_potr(potr$DBH)

# BARK
bark1_potr <- function(x) -554.585 + 47.289*(x^2)
# n = 21, r2 = 0.945
  # SE
    # intercept = 458.92
    # dbh2 = 2.61

bark2_potr <- function(x) 44.621*(x^2)
# n = 21, r2 = 0.980
  # SE
  # dbh2 = 1.41

potr$bark1 <- bark1_potr(potr$DBH)
potr$bark2 <- bark2_potr(potr$DBH)

# summing up components
potr$sum1 <- potr$fol1 + potr$branch1 + potr$stem1 + potr$bark1
potr$sum2 <- potr$fol2 + potr$branch2 + potr$stem2 + potr$bark2

plot(potr$sum1 ~ potr$DBH)
points(potr$sum2 ~ potr$DBH, col = "green")
points(potr$tab1 ~ potr$DBH, col = "red")
points(potr$tab2 ~ potr$DBH, col = "blue")
abline(v = c(0.1,26), col = "red") # my ranges
abline(v = c(0, 12.8), col = "blue", lty = 2) # equation's ranges

# need to find different aspen equations

#############################################################################
# balsam poplar

poba <- dbh %>%
  filter(SPP == "POBA")
hist(poba$DBH) # we only have three of these above dbh height...
summary(poba$DBH) # min 1.3, max 2.3

##########################
# testing total aboveground biomass equations
# only testing those that only require dbh

# first equation in table
tab1_poba <- function(x) -63610 + 8393.574*x + 141.872*(x^2)
# n = 17, r2 = 0.961
  # SE
    # intercept = 31024
    # DBH = 3032.78
    # dbh2 = 63.2

# second equation in table
tab2_poba <- function(x) 2695.76*x + 246.721*(x^2)
# n = 17, r2 = 0.974
  # SE
    # dbh = 1337.89
    # dbh2 = 40.92

poba$tab1 <- tab1_poba(poba$DBH)
poba$tab2 <- tab2_poba(poba$DBH)

plot(poba$tab1 ~ poba$DBH, xlim = c(0, 5))
points(poba$tab2 ~ poba$DBH, col = "red")
abline(v = c(1.3, 2.3), col = "red") # my ranges
abline(v = c(4.1, 46.5), col = "blue", lty = 2) # equation's ranges

# also doesn't really work
# need to find equation built off smaller stems

########################
# testing out calculating each component

# FOLIAGE
fol1_poba <- function(x) -3123.246 + 484.62*x - 5.997*(x^2)
# n = 17, r2 = 0.807
  # SE
    # intercept = 1101.84
    # dbh = 107.71
    # dbh2 = 2.24

fol2_poba  <- function(x) 178.69*(x)
# n = 17, r2 = 0.874
  # SE
    # dbh = 16.99

poba$fol1 <- fol1_poba(poba$DBH)
poba$fol2 <- fol2_poba(poba$DBH)

# LIVE BRANCHES
branch1_poba <- function(x) -11505 + 1611.775*x
# n = 17, r2 = 0.87
  # SE
    # intercept = 3598.65
    # dbh = 160.61

branch2_poba <- function(x) 631.112*x + 17.346*(x^2)
# n = 17, r2 = 0.918
  # SE
    # dbh = 274.17
    # dbh2 = 8.38

poba$branch1 <- branch1_poba(poba$DBH)
poba$branch2 <- branch2_poba(poba$DBH)

# STEMWOOD
stemw1_poba <- function(x) 14561 + 199.999*(x^2)
# n = 17, r2 = 0.906
  # SE
    # intercept = 12206
    # dbh2 = 16.66

stemw2_poba <- function(x) 1926.135*x + 158.062*(x^2)
# n = 17, r2 = 0.958
  # SE
    # dbh = 1145.23
    # dbh2 = 35.03

poba$stem1 <- stemw1_poba(poba$DBH)
poba$stem2 <- stemw2_poba(poba$DBH)

# BARK
bark1_poba <- function(x) -23308 + 2869*x
# n = 17, r2 = 0.896
  # SE
    # intercept = 5664.03
    # dbh2 = 252.79
      bark1_pobaSE <- function(x) sqrt((5664.03*x)^2 + (252.79*x)^2)

bark2_poba <- function(x) 61.936*(x^2)
# n = 17, r2 = 0.939
   # SE
    # dbh2 = 3.95

poba$bark1 <- bark1_poba(poba$DBH)
poba$bark2 <- bark2_poba(poba$DBH)

# summing up components
poba$sum1 <- poba$fol1 + poba$branch1 + poba$stem1 + poba$bark1
poba$sum2 <- poba$fol2 + poba$branch2 + poba$stem2 + poba$bark2

plot(poba$sum1 ~ poba$DBH, xlim = c(0, 7))
points(poba$sum2 ~ poba$DBH, col = "green")
points(poba$tab1 ~ poba$DBH, col = "red")
points(poba$tab2 ~ poba$DBH, col = "blue")
abline(v = c(0.1,2), col = "red") # my ranges
abline(v = c(4.1, 46.5), col = "blue", lty = 2) # equation's ranges

# not going to work for poba either

####################################################################
# testing out numbers (for those that work)

test <- rbind(pime, birch, poba, potr)

  # ignoring negative values for now
  test <- test %>%
    filter(tab1 > 0) %>%
    filter(tab2 > 0) %>%
    filter(sum1 > 0) %>%
    filter(sum2 > 0)

  # grouping at plot level
  test <- test %>%
    group_by(SITE, TREAT, PLOT) %>%
    summarise(tab1 = sum(tab1), tab2 = sum(tab2), sum1 = sum(sum1), sum2 = sum(sum2))

plot(test$tab1 ~ test$TREAT)
points(test$tab2 ~ test$TREAT, col = "red")
points(test$sum1 ~ test$TREAT, col = "blue")
points(test$sum2 ~ test$TREAT, col = "green")

# getting treatment averages to compare to browsing # (at least initially)
av <- test %>%
  group_by(SITE, TREAT, PLOT) %>%
  summarise(AV_ALL = mean(c(tab1, tab2, sum1, sum2)))
summary(av$AV_ALL) # min 491, # max 2423509
  # browsing summary per plot has min 290, max 345317.9
