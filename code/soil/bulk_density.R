# bulk density

# input:
    # soil_samples_format.csv
    # EA_processed.csv
# output:

# set up #############################################
library(tidyverse)
library(here)
library(cowplot) ; theme_set(theme_cowplot())
# Creating a not in operator:
`%!in%` <- Negate(`%in%`)

# bringing in weights of soil samples bagged
soil <- read.csv(here("data/soil/soil_samples_format.csv")) # 988

  # correcting NAs for missing samples
    soil$DRY_WEIGHT[soil$VIAL == "NO_SAMPLE"] <- 0

  # which ones are done
soil <- soil %>% # 974
  filter(VIAL != "NO_SAMPLE") %>%
  filter(NOTES != "REDONE_DROP")

# EA results ######################
ea <- read.csv(here("data/output/EA_processed.csv"), stringsAsFactors = F) # 1156 total

ea <- ea %>%
  filter(Sample_type == "unknown") %>%
  filter(C_weight_per != "") # 1103 samples (unknown)

ea <- ea %>%
 replace_na(list(C_weight_per = "N/A")) %>%
  replace_na(list(C_weight_per = ""))

ea$C_weight_per <- as.numeric(ea$C_weight_per)

c_per <- ea %>%
  group_by(VIAL) %>%
  summarise(C_per = mean(C_weight_per))  # 910

c_per <- c_per[!is.na(c_per$C_per),]

soil[soil$VIAL %!in% c_per$VIAL,]

done_ea <- soil %>%
  filter(VIAL %in% c_per$VIAL) %>% # 894
  select(!c(DRY_WEIGHT_BAG, VIAL_WEIGHT, BAG, NOTES))

done <- merge(done_ea, c_per, by = "VIAL") # 894

rm(ea, done_ea, soil, c_per)

# volume of soil corer # volume = pie * radius2 * height
# soil corer is 15 cm long, 4.7 cm wide (radius = 2.35 cm)
# so 38.1 cm by
done$vol <- pi*(2.35^2)*done$DEPTH_CM # in grams/cm3

# Bulk density ################################
## calculate ####
done$BD <- done$DRY_WEIGHT / done$vol # grams/cm3

done$C_density <- done$BD * done$C_per

done$C_section <- done$C_density * done$DEPTH_CM
# https://ro.ecu.edu.au/cgi/viewcontent.cgi?article=1990&context=ecuworks2013 # page 65

done %>%
  filter(ORG_MIN == "M") %>%
  ggplot(aes(x = as.factor(DEPTH), y = C_section)) + geom_boxplot() +
  ylim(c(0,1))

core <- done %>%
  group_by(SITE, TREAT, PLOT, CORNER, ORG_MIN, COARSE_FINE, DIVID) %>%
  summarise(core_sum = sum(C_section) ) # in grams / cm2

core$C_gm2 <- core$core_sum * 100 # from cm to m

# # averaging duplicate samples
# done_dup <- done %>%
#   group_by(SITE, TREAT, PLOT, CORNER, ORG_MIN, COARSE_FINE,DEPTH, DUP) %>%
#   summarise(C_gm2 = mean(C_gm2, na.rm = T))

# combining coarse / fine
core <- core %>%
  group_by(SITE, TREAT, PLOT, CORNER, ORG_MIN, COARSE_FINE) %>%
  summarise(C_gm2 = sum(C_gm2, na.rm = T))

# averaging per plot
plot_soilC <- core %>%
  group_by(SITE, TREAT, PLOT, ORG_MIN) %>%
  summarise("SD" = sd(C_gm2, na.rm = T), "C_gm2" = mean(C_gm2, na.rm = T))

plot_soilC %>%
  filter(ORG_MIN == "M") %>%
  ggplot( aes(x = as.factor(TREAT), y = C_gm2, fill = SITE)) +
  geom_boxplot()

plot_soilC %>%
  filter(ORG_MIN == "O") %>%
  ggplot( aes(x = as.factor(TREAT), y = C_gm2, fill = SITE)) +
  geom_boxplot()

write.csv(plot_soilC, here("data/output/soil_c.csv"), row.names = F)

