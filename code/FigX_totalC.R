# Fig X - total carbon

# input:
  # biomass_plot.csv
  # soil_c.csv
# output
  # FigX_totalC
  # total_c.csv

library(tidyverse)
library(here)
library(cowplot)
theme_set(theme_cowplot())

'%!in%' <- Negate('%in%')

biomass <- read.csv(here("data/output/biomass_plot.csv"))

# soil #############################
soil <- read.csv(here("data/output/soil_c.csv"))

soil <- soil %>%
  pivot_wider(values_from  = c(C_gm2, SD),
              names_from = ORG_MIN)

soil$C_gm2 <- soil$C_gm2_M + soil$C_gm2_O

totalC <- merge(biomass, soil, by = c("SITE", "TREAT", "PLOT"))

totalC <- totalC %>%
  rename("Soil" = "C_gm2") %>%
  select(!c(C_gm2_M, C_gm2_O, SD_M, SD_O, TAB))

totalC$totalC <- totalC$Overstory + totalC$Understory + totalC$Soil + totalC$CWD

totalC$tot_Overstory <- totalC$Overstory + totalC$Understory

totalC <- totalC %>%
  select(!c(Understory, Overstory)) %>%
  rename( "Overstory" = "tot_Overstory")

totalC_piv <- totalC %>%
  pivot_longer(cols = c(Overstory,   CWD, Soil, totalC),
               names_to = "Type", values_to = "Carbon_m2")

totalC_piv$Type <- factor(totalC_piv$Type, levels = c("totalC", "Soil", "CWD", "Overstory", "Understory" ))

# export
write.csv(totalC_piv, here("data/output/totalC_piv.csv"), row.names = F)

# Plot of all ########################################
# ggplot(totalC_piv, aes(x = as.factor(TREAT), y = Carbon_m2, fill = Type)) +
#   geom_boxplot() +
#   labs(x = "Number of Fires", y = "Carbon (g/m2)", title = "Total Carbon")
  #scale_fill_manual(name = "Carbon Pool",
   #                 label = c("Total Carbon", "Soil",
    #                          "Coarse Woody Debris", "Overstory Biomass",
     #                         "Understory Biomass"),
      #              values = c("#e34a33","#d8b365", "#4292c6","#9ecae1", "#deebf7"))

# Plot seperating 0 and burns #######################################
# plot0x <- totalC_piv %>%
#   filter(TREAT == 0) %>%
#   ggplot(aes(x = as.factor(TREAT), y = Carbon_m2, fill = Type)) + geom_boxplot() +
#   labs(title = "Total Carbon across pools and fire history", x = " ", y = "Biomass (grams/m2)") +
#   theme(legend.position = "none") +
#   geom_hline(yintercept = 15000, linetype = "dashed", color = "grey") +
#   scale_fill_manual(name = "Carbon Pool",
#                     label = c("Total Carbon", "Soil",
#                               "Coarse Woody Debris", "Overstory Biomass",
#                               "Understory Biomass"),
#                     values = c("#e34a33","#d8b365", "#4292c6","#9ecae1", "#deebf7"))
# plot0x
#
# plot123x <- totalC_piv %>%
#   filter(TREAT != 0) %>%
#   ggplot(aes(x = as.factor(TREAT), y = Carbon_m2, fill = Type)) + geom_boxplot() +
#   labs(title = "", x = "Number of Fires", y = "")  +
#   geom_hline(yintercept = 15000, linetype = "dashed", color = "grey") +
#   scale_fill_manual(name = "Carbon Pool",
#                     label = c("Total Carbon", "Soil",
#                               "Coarse Woody Debris", "Overstory Biomass",
#                               "Understory Biomass"),
#                     values = c("#e34a33","#d8b365", "#4292c6","#9ecae1", "#deebf7"))
# plot123x

totalC_piv$SITENAME[totalC_piv$SITE == "DALTON"] <- "UPLAND"
totalC_piv$SITENAME[totalC_piv$SITE == "STEESE"] <- "LOWLAND"

totalC_piv %>%
  ggplot(aes(x = as.factor(TREAT), y = Carbon_m2, fill = Type)) + geom_boxplot() +
  labs(title = "Total Carbon across pools and fire history", x = " ", y = "Total Carbon (grams/m2)") +
  scale_fill_manual(name = "Carbon Pool",
                    label = c("Total Carbon", "Soil",
                              "Coarse Woody Debris", "Aboveground Biomass",
                              "Understory Biomass"),
                    values = c("#e34a33","#d8b365", "#4292c6","#9ecae1", "#deebf7")) +
  facet_wrap(~SITENAME)

# plot_grid(plot0x, plot123x, rel_widths = c(0.75, 1.5)) # 900 x 400

# summary %s ##################################
cper <- totalC_piv %>%
  filter(Type == "totalC") %>%
  group_by(SITE,TREAT) %>%
  summarise(mean = mean(Carbon_m2, na.rm = T) )

cper <- totalC_piv %>%
  filter(Type != "totalC") %>%
  group_by(SITE,TREAT, Type) %>%
  summarise(mean = mean(Carbon_m2, na.rm = T) ) %>%
  mutate(total = sum(mean))

cper$per <- round(cper$mean / cper$total  * 100, digits = 2)

# pulling averages for draft
tot <- totalC %>%
  group_by(SITE, TREAT) %>%
  summarise(SD = sd(totalC), AV = mean(totalC))
