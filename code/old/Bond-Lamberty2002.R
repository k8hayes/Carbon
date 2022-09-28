# Bond-Lamberty et al. 2002
# has equations for spruce, aspen and willow
# but, comes from northern manitoba, will have to see if too far south

library(ggplot2)
library(cowplot); theme_set(theme_cowplot())
library(tidyverse)
library(here)

dbh <- read.csv(here("data/dbh.csv"))

#############################################################################
# willow

salix <- dbh %>%
  filter(SPP == "SALIX")
hist(salix$DBH) # everything smaller than 8 cm

# one salix in 41_1 has dbh = 0, even in field notebook
# dropping value
salix <- salix[-615,]

summary(salix$DBH) # min 0.1, max 8.1

########################
# calculating each component

# FOLIAGE
fol_salix <- function(x) 0.0235*x^(1.6656)
# n = ,r2 = 0.42
# SE
# intercept = 0.0032
# dbh^ = 0.0440
fol_salixSE <- sqrt((0.0032^2) + (0.0440^2))

salix$fol <- fol_salix(salix$DBH)

# LIVE BRANCHES
branch_salix <- function(x)  0.0082*x^(2.5139)
# n = , r2 = 0.76
# SE
# intercept = 0.0008
# dbh^ = 0.0327
branch_salixSE <- sqrt((0.0008^2) + (0.0327^2))

salix$branch <- branch_salix(salix$DBH)

# WOOD
wood_salix <- function(x) 0.0608*x^(2.4735)
# n = , r2 =0.95
# SE
# intercept = 0.0029
# dbh^ = 0.0153
wood_salixSE <- sqrt((0.0029^2) + (0.0153^2))

salix$wood <- wood_salix(salix$DBH)

# BARK
bark_salix <- function(x) 0.0159*x^(2.4123)
# n = , r2 = 0.91
# SE
# intercept = 0.0006
# dbh^ = 0.0131
bark_salixSE <- sqrt((0.0006^2) + (0.0131^2))

salix$bark <- bark_salix(salix$DBH)

# summing up components
salix$sum <- salix$fol + salix$branch + salix$wood + salix$bark

# standard error
salix$SE <- sqrt(fol_salixSE^2 + branch_salixSE^2 + wood_salixSE^2 + bark_salixSE^2)

plot(salix$sum ~ salix$DBH)
abline(v = c(0.1, 6.5), col = "red") # my ranges
abline(v = c(0.7, 47.2), col = "blue", lty = 2) # equation's ranges

ggplot(salix, aes(x = DBH, y = sum)) +
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
