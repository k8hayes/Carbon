# Working with pilot fuel data

library(tidyverse)
library(here)
library(cowplot)
theme_set(theme_cowplot())

# steese_cwd <- read.csv("data/Steese_Cover.csv")
# steese boxplot
#ggplot(steese_cwd, aes(x = as.factor(steese_cwd$TREATMENT), y = steese_cwd$CWD)) + geom_boxplot()

# dalton_cwd <- read.csv("data/Dalton_Species_coverage.csv")
# ggplot(dalton_cwd, aes(x = as.factor(dalton_cwd$TREATMENT), y = dalton_cwd$CWD)) + geom_boxplot()

# combining the two into one file
# cwd_cover <- steese_cwd[1:9]
# cwd_cover_dalton <- dalton_cwd[1:9]
# colnames(cwd_cover_dalton)<- colnames(cwd_cover)
# cwd_cover <- rbind(cwd_cover, cwd_cover_dalton)
# rm(cwd_cover_dalton)
# write.csv(cwd_cover, "data/cwd_cover.csv")

cwd_cover <- read.csv(here("data/CWD/cwd_cover.csv"))

ggplot(cwd_cover, aes( x = as.factor(cwd_cover$TREATMENT), y = cwd_cover$CWD, fill = cwd_cover$SITE)) +
  geom_boxplot() +  labs(title = "Coarse Woody Debris Cover",
                         x = "Number of Fires", y = "Percent cover") +
  scale_fill_manual(name = "Site Type",
                    values = c("#dfc27d", "#018571"),
                    labels = c("Upland", "Lowland")) +
  coord_cartesian(ylim = c(0, 100)) + panel_border() + background_grid()


# COARSE WOODY DEBRIS COUNT
# steese_count <- read.csv("data/steese_cwd_count.csv")
# dalton_count <- read.csv("data/Dalton_CWD_COUNT.csv")
# dalton_count <- dalton_count[-9]
# steese_count <- steese_count[which(steese_count$LINE == "SUM"),]
# dalton_count <- dalton_count[which(dalton_count$LINE == "SUM"),]
# colnames(dalton_count) <- colnames(steese_count)
# cwd_count <- rbind(steese_count, dalton_count)
# write.csv(cwd_count, "data/cwd_count.csv")

cwd_count <- read.csv("data/cwd_count.csv")
# cwd_count <- cwd_count[-1]
# cwd_count <- cwd_count[-4]

oneplot <- ggplot(cwd_count, aes(x = as.factor(cwd_count$TREAT), y = cwd_count$X1.HOUR, fill = cwd_count$SITE)) +
  geom_boxplot() + labs(title = "1-hour",
                        x = "Number of Fires", y = "Fuel counts") +
  panel_border() + background_grid() + scale_fill_manual(name = "Site Type",
                                                         values = c("#636363", "#252525"),
                                                         labels = c("Upland", "Lowland")) +
  theme(plot.title = element_text(hjust = 0))


tenplot<- ggplot(cwd_count, aes(x = as.factor(cwd_count$TREAT), y = cwd_count$X10.HOUR, fill = cwd_count$SITE)) +
  geom_boxplot() + labs(title = "10-hour",
                        x = "Number of Fires", y = "") +
  panel_border() + background_grid() + scale_fill_manual(name = "Site Type",
                                                         values = c("#f0f0f0", "#bdbdbd"),
                                                         labels = c("Upland", "Lowland")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0)) + coord_cartesian(y = c(0,40))


hundplot<- ggplot(cwd_count, aes(x = as.factor(cwd_count$TREAT), y = cwd_count$X100.HOUR, fill = cwd_count$SITE)) +
  geom_boxplot() +labs(title = "100-hour",
                         x = "Number of Fires", y = "") +
  panel_border() + background_grid() + scale_fill_manual(name = "Site Type",
                                                         values = c("#f0f0f0", "#bdbdbd"),
                                                         labels = c("Upland", "Lowland")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0)) + coord_cartesian(y = c(0,40))


thousand <- ggplot(cwd_count, aes(x = as.factor(cwd_count$TREAT), y = cwd_count$X1000.HOUR, fill = cwd_count$SITE)) +
  geom_boxplot() + labs(title = "1000-hour",
                          x = "Number of Fires", y = " ") +
  panel_border() + background_grid() + scale_fill_manual(name = "Site Type",
                                                         values = c("#f0f0f0", "#bdbdbd"),
                                                         labels = c("Upland", "Lowland")) +
  theme(plot.title = element_text(hjust = 0)) + coord_cartesian(y = c(0,40))
hrgrid1 <- plot_grid(tenplot, hundplot, thousand, nrow = 1, rel_heights = c(0.75, 0.75, 0.75),
          rel_widths = c(1,1,1.3), align = "h")
save_plot("test.png", hrgrid1, nrow = 1, ncol = 3, base_aspect_ratio = 1.5)

test1<- plot_grid(oneplot, tenplot, hundplot, thousand, labels = c("A.", "B.", "C.", "D."),
          rel_widths = c(1.3, 1, 1, 1.3))
save_plot("test.png", test1, nrow = 2, ncol = 2)

toptest <- plot_grid(oneplot, tenplot,labels = c("A.", "B."), rel_widths = c(1.3, 1))
bottest <- plot_grid(hundplot, thousand, labels = c("C.", "D."), rel_widths = c(1, 1.3))
test <- plot_grid(toptest, bottest, nrow = 2, ncol = 1)
save_plot("test.png", test, nrow = 2, ncol = 1)
