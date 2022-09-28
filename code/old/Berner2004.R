# Berner et al. 2004
# used for alder, dwarf birch and willow
  # but, only for basal diamater

library(ggplot2)
library(cowplot); theme_set(theme_cowplot())
library(tidyverse)
library(here)

# dbh <- read.csv(here("data/dbh.csv"))
# will use diff file

#############################################################################
# starting off with aspen

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
