
library(tidyverse)
library(here)

data <- read.csv(here("data/Site_age_all.csv"))

test <- data %>%
  group_by(SITE, TREAT, PLOT) %>%
  summarise(n = n(), MEDIAN = median(INTERVAL),
            MAX = max(INTERVAL))

write.csv(test, "data/site_age.csv", row.names = F)
