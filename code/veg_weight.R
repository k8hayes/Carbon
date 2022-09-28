# understory (true) biomass
# veg weights

# input: understory_veg.csv
# output: veg_weight_plot.csv

library(tidyverse)
library(here)

data <- read.csv(here("data/understory_veg.csv"))

data$diff <- data$WET_WEIGHT - data$DRY_WEIGHT

data <- data[data$DRY_WEIGHT != "NA",]

data %>%
  group_by(TREAT) %>%
  summarise(mean(DRY_WEIGHT))

data_plot <- data %>%
  group_by(SITE, TREAT, PLOT) %>%
  summarise(Biomass_g = mean(DRY_WEIGHT, na.rm = T))

data_site <- data %>%
  filter(SITE == "DALTON") %>%
  group_by(SITE, TREAT) %>%
  summarise(Biomass_g = mean(DRY_WEIGHT, na.rm = T))

data_plot <- data_plot[-17,]

# Export ###########################
write.csv(data_plot, here("data/output/veg_weight_plot.csv"), row.names = F)
