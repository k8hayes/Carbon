library(ggplot2)
library(cowplot); theme_set(theme_cowplot())
library(tidyverse)
library(here)
install.packages("allometree")
options(scipen = 999)

dbh <- read.csv(here("data/herb_unclump.csv"))
dbh <- dbh %>%
  rename("DBH" = "DBH_CM")
unique(dbh$SPP)




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

## graph ##########################

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

gamma <- glm(Biomass ~ DBH, family = "poisson", data = alcr)

summary(gamma)

plot(gamma)

test <- predict(gamma, interval = "prediction")
preds <- as.vector(test)
plot(preds ~ alcr$Biomass)

plot(alcr$Biomass ~ alcr$DBH)
points(preds, col = "red")
# subtracting foliage for dead individuals
alcr$Biomass[alcr$CANOPY == 0] <- alcr$Biomass[alcr$CANOPY == 0] - alcr$leaf[alcr$CANOPY == 0]

alcr <- alcr %>%
  select(!c(leaf, stem))