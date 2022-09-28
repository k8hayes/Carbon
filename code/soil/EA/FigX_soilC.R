# Figure X - soil percent carbon

library(tidyverse)
library(here)
library(cowplot)
theme_set(theme_cowplot())

data <- read.csv(here("data/output/soil_c.csv"))

orgplot <- data %>%
  filter(ORG_MIN == "O") %>%
  ggplot(aes(as.factor(TREAT), y = C_gm2, fill = SITE)) + geom_boxplot() +
  labs(x = "Number of Fires", y = "Carbon (g/m2)", title = "Organic Soil") +
  scale_fill_manual(name = "Site",
                    labels = c("Upland", "Lowland"),
                    values = c("#deebf7","#3182bd")) +
  theme(legend.position =  "none")
orgplot

minplot <- data %>%
  filter(ORG_MIN == "M") %>%
  ggplot(aes(as.factor(TREAT), y = C_gm2, fill = SITE)) + geom_boxplot() +
  labs(x = "Number of Fires", y = "Carbon (g/m2)", title = "Mineral Soil") +
  scale_fill_manual(name = "Site",
                    labels = c("Upland", "Lowland"),
                    values = c("#deebf7","#3182bd"))

plot_grid(orgplot,minplot, rel_widths = c(1,1.25), labels = c("A.", "B.")) # 850 x 350

# Splitting by 0 and 123

data %>%
  filter(ORG_MIN == "ORG") %>%
  filter(TREAT !=0) %>%
  ggplot(aes(as.factor(TREAT), y = C_gm2_AV, fill = SITE)) + geom_boxplot() +
  labs(x = "Number of Fires", y = "Carbon (g/m2)", title = "Organic Soil") +
  scale_fill_manual(name = "Site",
                    labels = c("Upland", "Lowland"),
                    values = c("#deebf7","#3182bd")) +
  theme(legend.position =  "none")


# Scaling up by active layer

thaw <- read.csv(here("data/soil/thaw_depth.csv"))

thaw <- thaw %>%
  group_by(SITE, TREAT) %>%
  summarise(thaw_cm = mean(DEPTH_CM, na.rm = T))
thaw$DEPTH_cm <- thaw$thaw_cm - 15
thaw$DEPTH <- "AL"

depth <- read.csv(here("data/output/soil_depth.csv"))

min <- depth %>%
  filter(ORG_MIN == "M")

depth_1015 <- min %>%
  filter(DEPTH == "10_15")

depth_1015$percm <- depth_1015$C_gm2 / 5

bottomC <- merge(depth_1015, active, by = c("SITE", "TREAT"))

bottomC$AL_C <- bottomC$percm * bottomC$AL_cm

bottomC <- bottomC %>%
  group_by(SITE, TREAT, PLOT) %>%
  summarise(C_gm2 = mean(AL_C, na.rm = T))

# combining back
test <- merge(min, bottomC, by = c("SITE", "TREAT", "PLOT"))


# pulling numbers out for draft
orgAV <- data %>%
  filter(ORG_MIN == "O") %>%
  group_by(SITE, TREAT) %>%
  summarise(SD = sd(C_gm2), AV = mean(C_gm2))
