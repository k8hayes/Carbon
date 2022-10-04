library(here)
library(tidyverse)

data <- read.csv(here("data/dbh.csv"))

dalt <- data %>%
  filter(TREAT != 0) %>%
  filter(SITE == "DALTON") %>%
  group_by(SITE, TREAT, PLOT, SPP) %>%
  summarise(n())


stee <- data %>%
  filter(TREAT != 0) %>%
  filter(SITE == "STEESE") %>%
  group_by(SITE, TREAT, PLOT, SPP) %>%
  summarise(n())


x <- 0:20

sample(x)
 sample(x)

