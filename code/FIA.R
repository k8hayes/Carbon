# comparing FIA biomass estiates to my numbers

library(ggplot2)
library(cowplot); theme_set(theme_cowplot())
library(tidyverse)
library(here)
options(scipen = 999)

fia <- read.csv(here("data/FIA/FIA_Spp_Biomass_by_standage.csv"))

fia <- fia %>%
  select(!c(X, X.1))

fia %>%
  ggplot(aes(x = STDAGE, y = PicMar_biomass)) + geom_point() + xlim(c(0,50))

fia %>%
  ggplot(aes(x = STDAGE, y = BetNeo_biomass)) + geom_point()+ xlim(c(0,50))
