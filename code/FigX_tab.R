# Figure 2 - total aboveground biomass

library(tidyverse)
library(cowplot); theme_set(theme_cowplot())
library(here)

# Dataset combined #############
tab <- read.csv(here("data/output/tab_plot.csv"))
tab$Overstory <- tab$Overstory / 2

tab_pivot <- tab %>%
  pivot_longer(cols = c(Overstory, Understory, CWD, TAB)) %>%
  rename("Biomass_m2" = "value", "Type" = "name")

tab_pivot$Type <- factor(tab_pivot$Type, levels = c("TAB", "CWD", "Overstory", "Understory"))

# Plot #####################
ggplot(tab_pivot, aes(x = as.factor(TREAT), y = Biomass_m2, fill = Type)) + geom_boxplot() +
  labs(x ="Number of Fires",
       y = "Total Biomass (g/m2)",
       title = "Total Aboveground Biomass") +
  scale_fill_manual(name = "Carbon Pool",
                    label = c("Total Aboveground Biomass",
                              "Coarse Woody Debris", "Overstory Biomass",
                              "Understory Biomass"),
                    values = c("#fec44f","#4292c6","#9ecae1", "#deebf7")) +
  geom_hline(yintercept = 2770, linetype = "dashed", color = "grey")
# save as 650 by 450

# Plot by site #######################

tab_pivot$SITENAME[tab_pivot$SITE == "DALTON"] <- "Upland"
tab_pivot$SITENAME[tab_pivot$SITE == "STEESE"] <- "Lowland"

tab_pivot %>%
  filter(Type != "TAB") %>%
  ggplot(aes(x = as.factor(TREAT), y = Biomass_m2, fill = Type)) + geom_boxplot() +
  labs(x ="Number of Fires",
       y = "Total Biomass (g/m2)",
       title = "Total aboveground biomass") +
  scale_fill_manual(name = "Carbon Pool",
                    label = c(
                              "Coarse Woody Debris", "Overstory Biomass",
                              "Understory Biomass"),
                    values = c("#fec44f","#2171b5","#9ecae1")) +
  facet_wrap(~SITENAME)  

# export as FigX_tab # 900 by 400

# code for splitting unburned and burned
# plot0x <- tab_pivot %>%
#   filter(TREAT == 0) %>%
#   ggplot(aes(x = as.factor(TREAT), y = Biomass_m2, fill = Type)) + geom_boxplot() +
#   labs(x ="Number of Fires",
#        y = "Total Biomass (g/m2)",
#        title = "Total Aboveground Biomass") +
#   theme(legend.position = "none") +
#   geom_hline(yintercept = 12000, linetype = "dashed", color = "grey") +
#   scale_fill_manual(name = "Carbon Pool",
#                     label = c("Total Aboveground Biomass",
#                               "Coarse Woody Debris", "Overstory Biomass",
#                               "Understory Biomass"),
#                     values = c("#fec44f","#4292c6","#9ecae1", "#deebf7"))
#
# plot0x
#
# plot123x <- tab_pivot %>%
#   filter(TREAT != 0) %>%
#   ggplot(aes(x = as.factor(TREAT), y = Biomass_m2, fill = Type)) + geom_boxplot() +
#   labs(x ="Number of Fires",
#        y = "Total Biomass (g/m2)",
#        title = "Total Aboveground Biomass") +
#   scale_fill_manual(name = "Carbon Pool",
#                     label = c("Total Aboveground Biomass",
#                               "Coarse Woody Debris","Overstory Biomass",
#                               "Understory Biomass"),
#                     values = c("#fec44f","#4292c6","#9ecae1", "#deebf7"))
#
#
# plot123x
#
# plot_grid(plot0x, plot123x, rel_widths = c(0.75, 1.5))

# proportions ############################

tab_sum <- tab %>%
  group_by(TREAT) %>%
  summarise(Over = mean(Overstory),
            Under = mean(Understory),
            cwd = mean(CWD),
            tab = mean(TAB))
tab_sum$per_cwd <- tab_sum$cwd / tab_sum$tab *100

tab_sum$per_over <- tab_sum$Over / tab_sum$tab *100

# adding site age
age <- read.csv(here("data/Site_age.csv"))

test <- merge(tab, age, by = c("SITE", "TREAT", "PLOT"))

plot(TAB ~ Interval, data = test)
