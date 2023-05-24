
library(tidyverse)
library(here)
library(cowplot); theme_set(theme_cowplot())
options(scipen = 999)

plot_biomass <- read.csv(here("data/output/biomass_plot.csv"))

# plotting biomass per plot between upland and lowland (in g/m2)

# pivot
biomass_pivot <- plot_biomass %>%
  pivot_longer(c(Overstory, Understory, CWD, TAB),
               names_to = "Pool", values_to = "gm2")

biomass_pivot$Pool <- factor(biomass_pivot$Pool,
                           levels = c("TAB", "Overstory",
                                      "CWD", "Understory"))

biomass_pivot$SITENAME[biomass_pivot$SITE == "DALTON"] <- "UPLAND"
biomass_pivot$SITENAME[biomass_pivot$SITE == "STEESE"] <- "LOWLAND"

ggplot(biomass_pivot, aes(x = as.factor(TREAT),
                          y = gm2, fill = Pool)) +
  geom_boxplot() +
  labs(title = "Aboveground Biomass",
       x = "Number of Fires", y = "Biomass (grams/m2)") +
  scale_fill_manual(name = "Pool",
                    label = c("Total Aboveground Biomass",
                              "Overstory Biomass", "Coarse Woody Debris",
                              "Understory Biomass"),
                    values = c("#2b8cbe",
                               "#74c476","#bae4b3","#edf8e9")) +
  facet_wrap(~SITENAME)


plot0x <- plot_biomass %>%
  filter(TREAT == 0) %>%
  ggplot(aes(x = as.factor(TREAT), y = Biomass_m2, fill = SITE)) + geom_boxplot() +
  labs(title = "Total Overstory Biomass (live + dead)", x = " ", y = "Biomass (grams/m2)") +
  scale_fill_manual(name = "Site", labels = c("Upland", "Lowland"),
                    values = c("#99d8c9","#2ca25f")) +
  theme(legend.position = "none") +
  geom_hline(yintercept = 1500, linetype = "dashed", color = "grey")
plot0x

plot123x <- plot_biomass %>%
  filter(TREAT != 0) %>%
  ggplot(aes(x = as.factor(TREAT), y = Biomass_m2, fill = SITE)) + geom_boxplot() +
  labs(title = "", x = "Number of Fires", y = "") +
  scale_fill_manual(name = "Site", labels = c("Upland", "Lowland"),
                    values = c("#99d8c9","#2ca25f")) +
  geom_hline(yintercept = 1500, linetype = "dashed", color = "grey")

plot_grid(plot0x, plot123x, rel_widths = c(0.75, 1.5)) # 575 by 275

reburn_p_biomass <-plot_biomass %>%
  filter(TREAT != 0)
summary(reburn_p_biomass$Biomass)

unburn_p_biomass <-plot_biomass %>%
  filter(TREAT == 0)
summary(unburn_p_biomass$Biomass)

mean(unburn_p_biomass$Biomass) / mean(reburn_p_biomass$Biomass)

# plotting only live biomass
live_plot_biomass <- test %>%
  filter(CANOPY != 0) %>%
  group_by(SITE, TREAT, QUAD, EXP_FACT, PLOT) %>%
  summarise(Biomass = sum(Biomass))

live_plot_biomass$Biomass_m2[live_plot_biomass$QUAD== 1.0] <- live_plot_biomass$Biomass[live_plot_biomass$QUAD == 1.0]/400
live_plot_biomass$Biomass_m2[live_plot_biomass$QUAD== 2.0] <- live_plot_biomass$Biomass[live_plot_biomass$QUAD == 2.0]/200
live_plot_biomass$Biomass_m2[live_plot_biomass$QUAD== 0.2] <- live_plot_biomass$Biomass[live_plot_biomass$QUAD == 0.2]/80

ggplot(live_plot_biomass, aes(x = as.factor(TREAT), y = Biomass_m2, fill = SITE)) + geom_boxplot() +
  labs(title = "Live Biomass", x = "Number of Fires", y = "Biomass (grams/m2)") +
  scale_fill_manual(name = "Site", labels = c("Upland", "Lowland"),
                    values = c("#99d8c9","#2ca25f"))
live_plot_biomass %>%
  filter(TREAT != 0) %>%
  ggplot(aes(x = as.factor(TREAT), y = Biomass_m2, fill = SITE)) + geom_boxplot() +
  labs(title = "Live Biomass across Reburns", x = "Number of Fires", y = "Biomass (grams/m2)") +
  scale_fill_manual(name = "Site", labels = c("Upland", "Lowland"),
                    values = c("#99d8c9","#2ca25f"))

# plotting according to species
spp_biomass <- test %>%
  group_by(SITE, TREAT, QUAD, EXP_FACT, PLOT, SPP) %>%
  summarise(Biomass = sum(Biomass))

spp_biomass$Biomass_m2[spp_biomass$QUAD== 1.0] <- spp_biomass$Biomass[spp_biomass$QUAD == 1.0]/400
spp_biomass$Biomass_m2[spp_biomass$QUAD== 2.0] <- spp_biomass$Biomass[spp_biomass$QUAD == 2.0]/200
spp_biomass$Biomass_m2[spp_biomass$QUAD== 0.2] <- spp_biomass$Biomass[spp_biomass$QUAD == 0.2]/80

ggplot(spp_biomass, aes(x = as.factor(TREAT), y = Biomass_m2, fill = SPP)) + geom_boxplot() + facet_wrap(~SITE) +
  labs(title = "Biomass summed across species within plots")

# live species biomass
live_spp_biomass <- test %>%
  filter(CANOPY != 0) %>%
  group_by(SITE, TREAT, QUAD, EXP_FACT, PLOT, SPP) %>%
  summarise(Biomass = sum(Biomass))

live_spp_biomass$Biomass_m2[live_spp_biomass$QUAD== 1.0] <- live_spp_biomass$Biomass[live_spp_biomass$QUAD == 1.0]/400
live_spp_biomass$Biomass_m2[live_spp_biomass$QUAD== 2.0] <- live_spp_biomass$Biomass[live_spp_biomass$QUAD == 2.0]/200
live_spp_biomass$Biomass_m2[live_spp_biomass$QUAD== 0.2] <- live_spp_biomass$Biomass[live_spp_biomass$QUAD == 0.2]/80

ggplot(live_spp_biomass, aes(x = as.factor(TREAT), y = Biomass_m2, fill = SPP)) + geom_boxplot() + facet_wrap(~SITE) +
  labs(title = "Biomass summed across species within plots")

# plotting without unburned controls
live_spp_biomass %>%
  filter(TREAT != 0) %>%
  ggplot(aes(x = as.factor(TREAT), y = Biomass_m2, fill = SPP)) + geom_boxplot() + facet_wrap(~SITE) +
  labs(title = "Live Species Biomass within reburned plots", y = "Biomass (g/m2)", x = "Number of Fires")

# # double checking stees unburned
# steese0 <- dbh %>%
#   filter(SITE == "STEESE") %>%
#   filter(TREAT == 0)
#
dalt0 <- dbh %>%
  filter(SITE == "DALTON") %>%
  filter(TREAT == 0)

dalt0[dalt0$DBH > 20,]
