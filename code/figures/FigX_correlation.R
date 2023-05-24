# correlation plot

# input:
  # biomass_variables.csv
  # totalC.csv

# start ####################################

library(GGally) # correlation plots
library(here)
library(tidyverse)

variables <- read.csv(here("data/output/biomass_variables.csv"))

totalC <- read.csv(here("data/output/totalC.csv"))

totalC <- totalC %>%
  group_by(SITE, TREAT, PLOT) %>%
  summarise(carbon = sum(Carbon_m2))

data <- merge(variables, totalC, by = c("PLOT", "SITE", "TREAT"))

data <- data %>%
  select(!c(Biomass_gm2))

data$SITE[data$SITE == "DALTON"] <- "UPLAND"
data$SITE[data$SITE == "STEESE"] <- "LOWLAND"

# Correlation ###################
colnames(data)
# with fire
GGally::ggpairs(data, columns = c(3:7),
                ggplot2::aes(fill = SITE, color = SITE),
                columnLabels = c("Fires", "Biomass (Kg/Ha)", "Density (Tree/Ha)",
                                 "Decid BA", "Org. Layer depth (cm)")) +
  scale_fill_manual(values = c("#7bccc4", "#fe9929"),
                    labels = c("Upland", "Lowland")) +
  scale_color_manual(values = c("#7bccc4", "#fe9929"),
                     labels = c("Upland", "Lowland"))

# without fire
GGally::ggpairs(data, columns = c(4:7),
                ggplot2::aes(fill = SITE, color = SITE),
                columnLabels = c("Carbon (g/m2)", "Density (Tree/m2)",
                                 "Decid BA", "Org. Layer depth (cm)")) +
  scale_fill_manual(values = c("#7bccc4", "#fe9929")) +
  scale_color_manual(values = c("#7bccc4", "#fe9929"))
# 675 * 375
