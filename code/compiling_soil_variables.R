# adding density, BA and SOL into soil file to model

# input:
  # soil_c.csv
  # ba.csv
  # org_depth.csv

# output: soil_variables.csv

library(tidyverse)
library(here)
library(cowplot)
theme_set(theme_cowplot())

# Set up ################
soil <- read.csv(here("data/output/soil_c.csv"))

colnames(soil)

soil <- soil %>%
  group_by(SITE, TREAT, PLOT) %>%
  summarise(Soil_gm2 = sum(C_gm2_AV))

# Density #################
density <- read.csv(here("data/density.csv"))

density$dens_plot <- density$TREE_COUNT_PLOT + density$SEED_COUNT_PLOT

density$dens_gm2 <- density$dens_plot / 400

density <- density %>%
  filter(SPP != "ARCTO") %>%
  filter(SPP != "PIGL") %>%
  group_by(SITE, TREAT, PLOT) %>%
  summarise(dens_gm2 = mean(dens_gm2))

## Merging biomass and density #############################

test <- merge(soil, density, by = c("PLOT", "SITE", "TREAT"))

colnames(test)

# ## Plots #############################
# test %>%
#   filter(TREAT != 0) %>%
#   ggplot(aes(x = dens_gm2, y = Biomass_gm2,
#              col = as.factor(TREAT), shape = SITE)) +
#   geom_point() +
#   scale_shape_manual(values = c(3,15),
#                      name = "Site + Number of Fires") +
#   scale_color_manual(name = "Site + Number of Fires",
#                     values = c('#000000','#E69F00', '#56B4E9')) +
#   labs(x = "Trees and Seedlings per m2", y = "Biomass (g/m2)",
#        title = "Stand density") +
#   theme(legend.position = "none")

# Decid BA ##################################
ba <- read.csv(here("data/ba.csv"))

ba_div <- ba %>%
  group_by(SITE, TREAT, PLOT, DIV) %>%
  summarise(DIV_COUNT = sum(BA)) %>%
  pivot_wider(names_from = DIV, values_from = DIV_COUNT)
ba_div$c[which(is.na(ba_div$c))] <- 0

ba_div$decidba_gm2 <- ba_div$d / 400

ba_div <- ba_div %>%
  select(!c(c, d))
rm(ba)

## Merging biomass and density #############################)

data <- merge(test, ba_div, by = c("SITE", "TREAT", "PLOT"))
rm(test, ba_div)

# Soil organic layer #####################################
orgdepth <- read.csv(here("data/soil/org_depth.csv"))

orgdepth <- orgdepth %>%
  group_by(SITE, TREAT, PLOT) %>%
  summarise(avSOLdepth_CM = mean(ORG_DEPTH))

## Merging ###################
data <- merge(data, orgdepth, by = c("SITE", "TREAT", "PLOT"))

write.csv(data, here("data/output/soil_variables.csv"), row.names = F)
