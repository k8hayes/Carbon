# Ung et al. 2008
# used for alder

library(ggplot2)
library(cowplot); theme_set(theme_cowplot())
library(tidyverse)
library(here)

 dbh <- read.csv(here("data/dbh.csv"))

#############################################################################
# starting off with aspen

potr <- dbh %>%
  filter(SPP == "POTR")
hist(potr$DBH) # everything smaller than 6 cm
summary(potr$DBH) # min 0.1, max 6.5

########################
# calculating each component

# FOLIAGE
fol_potr <- function(x) 0.0235*x^(1.6656)
# n = ,r2 = 0.42
  # SE
    # intercept = 0.0032
    # dbh^ = 0.0440
    fol_potrSE <- sqrt((0.0032^2) + (0.0440^2))

potr$fol <- fol_potr(potr$DBH)

# LIVE BRANCHES
branch_potr <- function(x)  0.0082*x^(2.5139)
# n = , r2 = 0.76
  # SE
    # intercept = 0.0008
    # dbh^ = 0.0327
  branch_potrSE <- sqrt((0.0008^2) + (0.0327^2))

potr$branch <- branch_potr(potr$DBH)

# WOOD
wood_potr <- function(x) 0.0608*x^(2.4735)
# n = , r2 =0.95
  # SE
    # intercept = 0.0029
    # dbh^ = 0.0153
    wood_potrSE <- sqrt((0.0029^2) + (0.0153^2))

potr$wood <- wood_potr(potr$DBH)

# BARK
bark_potr <- function(x) 0.0159*x^(2.4123)
# n = , r2 = 0.91
  # SE
    # intercept = 0.0006
    # dbh^ = 0.0131
    bark_potrSE <- sqrt((0.0006^2) + (0.0131^2))

potr$bark <- bark_potr(potr$DBH)

# summing up components
potr$sum <- potr$fol + potr$branch + potr$wood + potr$bark

# standard error
potr$SE <- sqrt(fol_potrSE^2 + branch_potrSE^2 + wood_potrSE^2 + bark_potrSE^2)

plot(potr$sum ~ potr$DBH)
abline(v = c(0.1, 6.5), col = "red") # my ranges
abline(v = c(0.7, 47.2), col = "blue", lty = 2) # equation's ranges

ggplot(potr, aes(x = DBH, y = sum)) +
  geom_point() +
  geom_ribbon(aes(ymax = sum + SE, ymin = sum - SE), alpha = .3) +
  labs(x = "DBH (cm)", y = "Dry Biomass (g)", title = "Trembling Aspen (Total Above. Biomass)") +
  geom_vline(xintercept = c(0.1, 6.5), linetype = "dashed") +
  geom_vline(xintercept = c(0.7, 47.2), linetype = "dashed", col = "blue")  +
  xlim(0, 8) +
  geom_text(mapping = aes(x = 0.35, y = 8, label = "Observed Range"),
            angle = 90, size = 3, check_overlap = TRUE) +
  geom_text(mapping = aes(x = 1, y = 8, label = "Equation Range"),
            angle = 90, size = 3, color = "Blue", check_overlap = TRUE)
