# stand age

library(tidyverse)
library(here)
library(cowplot) ; theme_set(theme_cowplot)

age <- read.csv(here("data/stand age/unburn_stand_age.csv"))

age %>%
  group_by(SITE, PLOT) %>%
  summarise(AV = mean(STAND_AGE), SD = sd(STAND_AGE))

age %>%
  group_by(SITE) %>%
  summarise(AV = mean(STAND_AGE), SD = sd(STAND_AGE))

