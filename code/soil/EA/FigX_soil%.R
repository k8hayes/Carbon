# Figure X - soil percent carbon

library(tidyverse)
library(here)
library(cowplot)
theme_set(theme_cowplot())

data <- read.csv(here("data/output/soil_per.csv"))

# averaging duplicate samples
data <- data %>%
  group_by(SITE, TREAT, PLOT, ORG_MIN) %>%
  summarise(C_per = mean(C_per, na.rm = T))

minplot <- data %>%
  filter(ORG_MIN == "M") %>%
  ggplot(aes(as.factor(TREAT), as.numeric(C_per), fill = SITE)) + geom_boxplot() +
  labs(x = "Number of Fires", y = "Carbon estimate (%)", title = "Mineral Soil") +
  scale_fill_manual(name = "Site",
                    labels = c("Upland", "Lowland"),
                    values = c("#e34a33","#fdbb84"))
minplot

orgplot <- data %>%
  filter(ORG_MIN == "O") %>%
  ggplot(aes(as.factor(TREAT), C_per, fill = SITE)) + geom_boxplot() +
  labs(x = "Number of Fires", y = "Carbon estimate (%)", title = "Organic Soil") +
  scale_fill_manual(name = "Site",
                    labels = c("Upland", "Lowland"),
                    values = c("#e34a33","#fdbb84")) +
  theme(legend.position =  "none")
orgplot

plot_grid(orgplot, minplot, rel_widths = c(1,1.25), labels = c("A.", "B."))



