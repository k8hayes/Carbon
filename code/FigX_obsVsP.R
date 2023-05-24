# 

library(tidyverse)
library(here)
library(cowplot)
library(ggpubr)
theme_set(theme_cowplot())

# Start ##############################

data <- read.csv(here("data/output/soil_c.csv"))

# adding age
age <- read.csv(here("data/site_age.csv"))

age <- age %>%
  select(c(SITE, TREAT, PLOT, USE)) %>%
  rename(AGE = USE)

data <- merge(data, age, by = c("SITE", "TREAT", "PLOT"))

data <- data %>%
  filter(ORG_MIN == "O") %>%
  filter(TREAT != 0)

data$Site[data$SITE == "DALTON"] <- "Upland"
data$Site[data$SITE == "STEESE"] <- "Lowland"

## adding equations ###############
lowC <- function(x) 2500 + 20*x
highC <- function(x) 2500 + 40*x

test <- data.frame(c(11.5:16),
                   lowC(c(11.5:16)),
                   highC(c(11.5:16)))
colnames(test) <- c("AGE", "LowC", "HighC")
test <- test %>%
  pivot_longer(c(LowC, HighC), names_to = "EQ",
               values_to = "C_gm2")

# 1x fire ######################
data1 <- data %>%
  filter(TREAT == 1)

dataAVdalt1 <- data1 %>%
  filter(SITE == "DALTON") %>%
  group_by(AGE) %>%
  summarise(AV = mean(C_gm2))

dataAVstee1 <- data1 %>%
  filter(SITE == "STEESE") %>%
  group_by(AGE) %>%
  summarise(AV = mean(C_gm2))

## plot ###################
plot1 <-ggplot() + 
  geom_jitter(data1, mapping = aes(x = AGE, y = C_gm2, col = Site),
              alpha = 0.5, width = 0.1) +
  geom_crossbar(dataAVdalt1, mapping = aes(x = AGE, xmin = AGE, xmax = AGE,
                                      y = AV,  ymin = AV, ymax = AV),
                size = 0.25, col = "#2da3a7", width = .5) +
  geom_crossbar(dataAVstee1, mapping = aes(x = AGE, xmin = AGE, xmax = AGE,
                                          y = AV, ymin = AV, ymax = AV),
                size = 0.25, col = "#f76d67", width = .5) +
  geom_line(test, mapping =  aes(x = AGE, y = C_gm2, 
                                group = EQ), linetype = "dashed") + 
  labs(x = "Age (Years)", y = "Carbon (g/m2)", title = "Once-burned") + 
  theme(legend.position = "none")

# 2x fire ######################
data2 <- data %>%
  filter(TREAT == 2)

dataAVdalt2 <- data2 %>%
  filter(SITE == "DALTON") %>%
  group_by(AGE) %>%
  summarise(AV = mean(C_gm2))

dataAVstee2 <- data2 %>%
  filter(SITE == "STEESE") %>%
  group_by(AGE) %>%
  summarise(AV = mean(C_gm2))

## plot #####
plot2 <- ggplot() + 
  geom_jitter(data2, mapping = aes(x = AGE, y = C_gm2, col = Site),
              alpha = 0.5, width = 0.1) +
  geom_crossbar(dataAVdalt2, mapping = aes(x = AGE, xmin = AGE,
                                           y = AV, xmax = AGE, ymin = AV, ymax = AV),
                size = 0.5, col = "#2da3a7", width = .5) +
  geom_crossbar(dataAVstee2, mapping = aes(x = AGE, xmin = AGE,
                                           y = AV, xmax = AGE, ymin = AV, ymax = AV),
                size = 0.5, col = "#f76d67", width = .5) +
  geom_line(test, mapping =  aes(x = AGE, y = C_gm2, 
                                 group = EQ), linetype = "dashed") + 
  labs(x = "Age (Years)", y = "Carbon (g/m2)", title = "Twice-burned") +
  theme(legend.position = "none")

# 3x fire ######################
data3 <- data %>%
  filter(TREAT == 3)

dataAVdalt3 <- data3 %>%
  filter(SITE == "DALTON") %>%
  group_by(AGE) %>%
  summarise(AV = mean(C_gm2))

dataAVstee3 <- data3 %>%
  filter(SITE == "STEESE") %>%
  group_by(AGE) %>%
  summarise(AV = mean(C_gm2))

## plot ##########
plot3 <- ggplot() + 
  geom_jitter(data3, mapping = aes(x = AGE, y = C_gm2, col = Site),
              alpha = 0.5, width = 0.1) +
  geom_crossbar(dataAVdalt3, mapping = aes(x = AGE, xmin = AGE,
                                           y = AV, xmax = AGE, ymin = AV, ymax = AV),
                size = 0.5, col = "#2da3a7", width = .5) +
  geom_crossbar(dataAVstee3, mapping = aes(x = AGE, xmin = AGE,
                                           y = AV, xmax = AGE, ymin = AV, ymax = AV),
                size = 0.5, col="#f76d67", width = .5) +
  geom_line(test, mapping =  aes(x = AGE, y = C_gm2, 
                                 group = EQ), linetype = "dashed") + 
  labs(x = "Age (Years)", y = "Carbon (g/m2)", title = "Thrice-burned")

# Combine #############################

plot_grid(plot1, plot2, plot3, ncol = 3, rel_widths = c(1,1,1.35)) # 1000 by 300

  
# TEST of 20% loss ############################################

## 1x fire ######################
data1 <- data %>%
  filter(TREAT == 1)

dataAVdalt1 <- data1 %>%
  filter(SITE == "DALTON") %>%
  group_by(AGE) %>%
  summarise(AV = mean(C_gm2))

dataAVstee1 <- data1 %>%
  filter(SITE == "STEESE") %>%
  group_by(AGE) %>%
  summarise(AV = mean(C_gm2))

### plot ###################
plot1 <-ggplot() + 
  geom_jitter(data1, mapping = aes(x = AGE, y = C_gm2, col = Site),
              alpha = 0.5, width = 0.1) +
  geom_crossbar(dataAVdalt1, mapping = aes(x = AGE, xmin = AGE, xmax = AGE,
                                           y = AV,  ymin = AV, ymax = AV),
                size = 0.25, col = "#2da3a7", width = .5) +
  geom_crossbar(dataAVstee1, mapping = aes(x = AGE, xmin = AGE, xmax = AGE,
                                           y = AV, ymin = AV, ymax = AV),
                size = 0.25, col = "#f76d67", width = .5) +
  geom_line(test, mapping =  aes(x = AGE, y = C_gm2, 
                                 group = EQ), linetype = "dashed") + 
  labs(x = "Age (Years)", y = "Carbon (g/m2)", title = "Once-burned") + 
  theme(legend.position = "none")

## 2x fire ######################


lowC2 <- function(x) 2000 + 20*x
highC2 <- function(x) 2000 + 40*x

test2 <- data.frame(c(11.5:16),
                    lowC2(c(11.5:16)),
                    highC2(c(11.5:16)))
colnames(test2) <- c("AGE", "LowC", "HighC")
test2 <- test2 %>%
  pivot_longer(c(LowC, HighC), names_to = "EQ",
               values_to = "C_gm2")
data2 <- data %>%
  filter(TREAT == 2)

dataAVdalt2 <- data2 %>%
  filter(SITE == "DALTON") %>%
  group_by(AGE) %>%
  summarise(AV = mean(C_gm2))

dataAVstee2 <- data2 %>%
  filter(SITE == "STEESE") %>%
  group_by(AGE) %>%
  summarise(AV = mean(C_gm2))

### plot #####
plot2 <- ggplot() + 
  geom_jitter(data2, mapping = aes(x = AGE, y = C_gm2, col = Site),
              alpha = 0.5, width = 0.1) +
  geom_crossbar(dataAVdalt2, mapping = aes(x = AGE, xmin = AGE,
                                           y = AV, xmax = AGE, ymin = AV, ymax = AV),
                size = 0.5, col = "#2da3a7", width = .5) +
  geom_crossbar(dataAVstee2, mapping = aes(x = AGE, xmin = AGE,
                                           y = AV, xmax = AGE, ymin = AV, ymax = AV),
                size = 0.5, col = "#f76d67", width = .5) +
  geom_line(test2, mapping =  aes(x = AGE, y = C_gm2, 
                                  group = EQ), linetype = "dashed") + 
  labs(x = "Age (Years)", y = "Carbon (g/m2)", title = "Twice-burned") +
  theme(legend.position = "none")

## 3x fire ######################

lowC3 <- function(x) 1600 + 20*x
highC3 <- function(x) 1600 + 40*x

test3 <- data.frame(c(11.5:16),
                    lowC3(c(11.5:16)),
                    highC3(c(11.5:16)))
colnames(test3) <- c("AGE", "LowC", "HighC")
test3 <- test3 %>%
  pivot_longer(c(LowC, HighC), names_to = "EQ",
               values_to = "C_gm2")
data3 <- data %>%
  filter(TREAT == 3)

dataAVdalt3 <- data3 %>%
  filter(SITE == "DALTON") %>%
  group_by(AGE) %>%
  summarise(AV = mean(C_gm2))

dataAVstee3 <- data3 %>%
  filter(SITE == "STEESE") %>%
  group_by(AGE) %>%
  summarise(AV = mean(C_gm2))

### plot ##########
plot3 <- ggplot() + 
  geom_jitter(data3, mapping = aes(x = AGE, y = C_gm2, col = Site),
              alpha = 0.5, width = 0.1) +
  geom_crossbar(dataAVdalt3, mapping = aes(x = AGE, xmin = AGE,
                                           y = AV, xmax = AGE, ymin = AV, ymax = AV),
                size = 0.5, col = "#2da3a7", width = .5) +
  geom_crossbar(dataAVstee3, mapping = aes(x = AGE, xmin = AGE,
                                           y = AV, xmax = AGE, ymin = AV, ymax = AV),
                size = 0.5, col="#f76d67", width = .5) +
  geom_line(test3, mapping =  aes(x = AGE, y = C_gm2, 
                                  group = EQ), linetype = "dashed") + 
  labs(x = "Age (Years)", y = "Carbon (g/m2)", title = "Thrice-burned")

## Combine #############################

plot_grid(plot1, plot2, plot3, ncol = 3, rel_widths = c(1,1,1.35)) # 1000 by 300



