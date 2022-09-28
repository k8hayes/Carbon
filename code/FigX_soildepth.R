# data exploration - depth

library(tidyverse)
library(cowplot)
library(here)
theme_set(theme_cowplot())


soil <- read.csv(here("data/output/soil_depth.csv"))

soil <- soil %>%
  filter(ORG_MIN == "M")

soil %>%
  ggplot(aes(x = as.factor(TREAT), y = C_gm2, fill = DEPTH)) + geom_boxplot()
