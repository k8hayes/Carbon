# working with coarse woody debris biomass

library(tidyverse)
library(cowplot); theme_set(theme_cowplot())
library(here)
options(scipen = 9999)

cwd_total <- read.csv(here("data/CWD/CWD_output_total.csv"))

cwd_total$TOTAL_G.m2 <- cwd_total$TOTAL_MG.H / 0.01



plot0x <- cwd_total %>%
  filter(TREAT == 0) %>%
  ggplot( aes(x = as.factor(TREAT), y = TOTAL_G.m2, fill = SITE)) + geom_boxplot() +
  labs(x = "", y = "Total Biomass (g/m2)", title = "Downed Woody Debris Biomass") +
  scale_fill_manual(name = "Site",
                    labels = c("Upland", "Lowland"),
                    values = c("#bf812d", "#dfc27d")) +
  theme(legend.position = "none") + geom_hline(yintercept = 1500, linetype = "dashed", color = "grey")
plot0x

plot123x <- cwd_total %>%
  filter(TREAT != 0) %>%
  ggplot( aes(x = as.factor(TREAT), y = TOTAL_G.m2, fill = SITE)) + geom_boxplot() +
  labs(x = "Number of Fires", y = "", title = "") +
  scale_fill_manual(name = "Site",
                    labels = c("Upland", "Lowland"),
                    values = c("#bf812d", "#dfc27d")) +
  geom_hline(yintercept = 1500, linetype = "dashed", color = "grey")
plot123x

plot_grid(plot0x, plot123x, rel_widths = c(0.75, 1.5), labels = "Number of Fires",
          label_x = 0, label_y = 0)


cwd_char <- read.csv(here("data/CWD/CWD_output_total.csv"))

cwd <- cwd_total
cwd$CHAR <- cwd_total$TOTAL_MG.H - cwd_char$TOTAL_MG.H
cwd$CHAR_fract <- cwd$CHAR / cwd$TOTAL_MG.H * 100

ggplot(cwd, aes(x = as.factor(TREAT), y = CHAR_fract, fill = SITE)) + geom_boxplot() +
  labs(x = "Number of Fires", y = "Percent char", title = "Charred downed biomass") +
  scale_fill_manual(name = "Site",
                    labels = c("Upland", "Lowland"),
                    values = c("#bf812d", "#dfc27d"))
