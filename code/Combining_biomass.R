# Start #########

# input:
  # understory_biomass_plot.csv
  # overstory_biomass_plot.csv
  # CWD_output_total.csv
  # veg_weight_plot.csv
# output: output/biomass_plot.csv

library(tidyverse)
library(cowplot); theme_set(theme_cowplot())
library(here)
options(scipen = 999)

'%!in%' <- Negate('%in%')

# overstory and understory ############################################

   und <- read.csv(here("data/output/understory_biomass_plot.csv"))
   und <- und %>%
     rename("Biomass_m2" = "g.m2") %>%
     dplyr::select(c(SITE, TREAT, PLOT, Biomass_m2))
   und$TYPE <- "Understory"

   over <- read.csv(here("data/output/overstory_biomass_plot.csv"))
   over <- over %>%
     dplyr::select(c(SITE, TREAT, PLOT, Biomass_m2))
   over$TYPE <- "Overstory"

   which(over$PLOT %!in% und$PLOT)

  # ggplot(over, aes(x = as.factor(TREAT), y = Biomass_m2, fill = SITE)) + geom_boxplot() +
  # labs(x = "Number of Fires", y = "Biomass (g/m2)", title = "Understory biomass")

  both <- merge(over, und, by = c("SITE", "TREAT", "PLOT"))

   both <- both %>%
    dplyr::select(!c("TYPE.y", "TYPE.x")) %>%
    rename("Overstory" = "Biomass_m2.x",
           "Understory" = "Biomass_m2.y")


# Adding CWD ####################################
cwd_total <- read.csv("data/CWD/CWD_output_total.csv")

cwd_total$TOTAL_G.m2 <- cwd_total$TOTAL_MG.H * 100

cwd <- cwd_total %>%
  dplyr::select(c("PLOT", "TOTAL_G.m2"))

plot.names <- both$PLOT
plot.cwd <- cwd$TOTAL_G.m2

for(i in 1:length(plot.names)) {
  plot.cwd[i] <- cwd$TOTAL_MG.H[cwd$PLOT == plot.names[i]]
}
plot.cwd

both$CWD <- plot.cwd

tab <- both
rm(both, over, und, cwd_total, cwd)

tab$TAB <- tab$Overstory + tab$Understory + tab$CWD

# litter (veg) weight ###################333
veg <- read.csv(here("data/output/veg_weight_plot.csv"))

veg %>%
  group_by(SITE, TREAT) %>%
  summarise(mean(Biomass_g), sd(Biomass_g))

tab %>%
  group_by(TREAT) %>%
  summarise(n())

  test <- c(rnorm(8, 16,1),
            rnorm(15, 64.9,19.2),
            rnorm(14, 45.7,39.2),
            rnorm(13, 25,22.4))

tab <- test %>%
  select(!c(PLOT.y, SITE.y)) %>%
  rename("veg_weight_gm2" = "Biomass_g", "PLOT" = "PLOT.x",
         "SITE" = "SITE.x")
rm(test)

tab$TAB <- tab$Overstory + tab$Understory + tab$CWD + tab$veg_weight_gm2

write.csv(tab, here("data/output/biomass_plot.csv"), row.names = FALSE)
