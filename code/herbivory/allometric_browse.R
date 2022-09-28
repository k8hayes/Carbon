# Allometrics ################################
# building our own equations to evaluate how much biomass might be removed by herbivores

library(tidyverse)
library(here)
# library(msm) # used for truncated normal distribution
library(TruncatedDistributions) # used for beta distributions
  # NOTE: had to use set.repositories() and make sure Rstudio was looking for r-forge
  # https://rdrr.io/rforge/TruncatedDistributions/man/tbeta.html
  # install.packages("TruncatedDistributions", repos="http://R-Forge.R-project.org")

library(cowplot); theme_set(theme_cowplot())
options(scipen = 999)

allom <- read.csv(here("data/allometrics_raw.csv"))

# Data clean up ##########################################################

  # taking out NAs
  ok <- complete.cases(allom$DRY_WEIGHT)
  allom <- allom[ok,]
  ok <- complete.cases(allom$WIDTH_IN)
  allom <- allom[ok,]

  # width measured with calipers, so should be x/32
  allom$WIDTH_IN <- allom$WIDTH_IN / 32
  allom$WIDTH_IN <- round(allom$WIDTH_IN, digits = 2)

hist(allom$WIDTH_IN) # not an even measurement of branches of all widths

  # getting rid of blank rows (used to record DBH / height in data entry)
    allom <- allom %>%
      filter(STEM != "S0")
    allom <- subset(allom, select = -c(DBH, HEIGHT_M))

# testing distributions
  # gamma distribution
    plot(allom$WIDTH_IN, allom$DRY_WEIGHT)

    allom_plot <- ggplot(allom, aes(x = WIDTH_IN, y = DRY_WEIGHT)) + geom_jitter(shape = 1) +
      labs( x = "Width of browse (inch)", y = "Aboveground Carbon (grams)", title = "Allometric equation of Aboveground Carbon removal")
    allom_plot

    gamma <- glm(DRY_WEIGHT ~ WIDTH_IN, family = Gamma(link = "log"), data = allom)
    summary(gamma) # AIC 1049

  # inverse guassian
    inv.gauss <- glm(DRY_WEIGHT ~ WIDTH_IN, family = inverse.gaussian(link = "log"), data = allom)
    summary(inv.gauss) # AIC 996

par(mfrow = c(4,4))
plot(lm(allom$DRY_W ~ allom$WIDTH_IN), main = "Not logged")
plot(lm(log(allom$DRY_W) ~ allom$WIDTH_IN), main = "Logged")
plot(gamma)
plot(inv.gauss)
dev.off()

# clean up
  rm(gamma, ok)

# Creating equation #################################################

out <- predict(inv.gauss, interval="prediction")
preds <- as.vector(out)
plot(preds ~ allom$DRY_WEIGHT, ylab="Predicted", xlab="Observed", pch=16)

# make new prediction object

  # bring in herb data
  herb <- read.csv(here("data/herbivory/herb.csv"))

  # counting how many are clumped
    herb %>%
      group_by(STEM_TYPE) %>%
      summarise(count = n())

  new <- data.frame(x = herb$MAX_B_WIDTH)
  new <- new %>%
    rename("WIDTH_IN" = "x") # so predict() knows where to go

  # Run
  herb$MASS_REMOVED <- predict(inv.gauss, newdata = new, type="response", interval="prediction")

  # clean up
  rm(new)

# correcting for intercept on non-browsed entries
  herb$MASS_REMOVED[herb$B_TYPE == "N"] <- 0

  herb <- herb %>%
    filter(MASS_REMOVED != 0)

hist(herb$MASS_REMOVED)
plot(herb$MASS_REMOVED ~ herb$MAX_B_WIDTH)

# selecting out regen FOR NOW
herb <- herb %>%
  filter(HEIGHT_M > 1.37)

# still plots each tree data point
  ggplot(herb, aes(x = as.factor(TREAT), y = MASS_REMOVED, col = B_TYPE)) +
    geom_boxplot()  +
    labs( x = "Fire", y = "Dry mass removed (g)") +
    scale_color_discrete(name = "Browse Type ",
                         labels = c("B" = "Both", "H" = "Hare",
                                  "M" = "Moose"))
# grouping at plot level
  plot_brow <- herb %>%
    group_by(SITE, TREAT, PLOT) %>%
    summarise(MASS_REMOVED = sum(MASS_REMOVED))

  ggplot(plot_brow, aes(x = as.factor(TREAT), y = MASS_REMOVED, col = SITE)) +
    geom_boxplot()  +
    labs( x = "Fire", y = "Dry mass removed (g)",
          title = "Estimated mass removed by herbivores")

# early estimate of average according to site / treat
    # just to compare to yarie 2007
  # b_av <- herb %>%
    #group_by(SITE, TREAT, PLOT) %>%
    #summarise(AV = mean(MASS_REMOVED))


# Monte Carlo ################################################

  # definitely not a normal distribution
    hist(herb$BROW_INDEX)

  # just selecting for individuals with browse counts
    # first, select for individuals
    herb_ind <- herb %>%
      filter(STEM_TYPE == "IND")
    # then selecting for those who actually have browse countsrver
     ok <- complete.cases(herb_ind$BROW_INDEX)
     herb_ind <- herb_ind[ok,]
    # clean up
     rm(ok, preds)

     hist(herb_ind$BROW_INDEX)

     summary(herb_ind$BROW_INDEX) # mean 0.4458 # min 0 # max 1
     sd(herb_ind$BROW_INDEX) # sd 0.2915604

     # truncated normal distribution
        # 1291 samples are clumped
        # test <- rtnorm(1291, mean = 0.4458, sd = 0.2915604, upper = 1.0, lower = 0.0)
        # test <- round(test, digits = 2)
        # hist(test)

     # beta distribution
     # function from:
     # https://stats.stackexchange.com/questions/12232/calculating-the-parameters-of-a-beta-distribution-using-the-mean-and-variance
     estBetaParams <- function(mu, var) {
       alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
       beta <- alpha * (1 / mu - 1)
       return(params = list(alpha = alpha, beta = beta))
     }

     estBetaParams(0.4458, (0.2915604^2))

    test <- rtbeta(519, 0.8498556, 1.056505, a = 0, b = 1)

    hist(test)

     # building distribution
     mc_data <- herb %>%
       filter(STEM_TYPE == "CLUMP")
     mc_data$BROW_INDEX <- test

     mc_data <- rbind(mc_data, herb_ind)

  # scaling up by browsing index
     mc_data <- mc_data %>%
       rename(MASS_REMOVE_STEM = MASS_REMOVED) # renamed to avoid confusion
     mc_data$MASS_REMOVE_TREE <- mc_data$MASS_REMOVE_STEM * (mc_data$BROW_INDEX*100)


     hist(mc_data$MASS_REMOVE_TREE)

# Plots ########################################
     # plotting each tree as a data point
     ggplot(mc_data, aes(x = as.factor(TREAT), y = MASS_REMOVE_TREE, col = B_TYPE)) +
       geom_boxplot()  +
       labs( x = "Fire", y = "Dry biomass (g)", title = "Mass Removed by browsing") +
       scale_color_discrete(name = "Browse Type ",
                            labels = c("B" = "Both", "H" = "Hare",
                                       "M" = "Moose"))
     # grouping at plot level
     mc_plot <- mc_data %>%
       group_by(SITE, TREAT, PLOT) %>%
       summarise(MASS_REMOVED = sum(MASS_REMOVE_TREE))

     # adding in quandrants/m2
     mc_plot$m2 <- 100 # default
         # exceptions:
         mc_plot$m2[mc_plot$PLOT == "15_3"] <- 25
         mc_plot$m2[mc_plot$PLOT == "41_1"] <- 200
         mc_plot$m2[mc_plot$PLOT == "20_1"] <- 25

      # scaling to g/m2
         mc_plot$g.m2 <- mc_plot$MASS_REMOVED / mc_plot$m2

       ggplot(mc_plot, aes(x = as.factor(TREAT), y = g.m2, col = SITE)) +
       geom_boxplot()  +
       labs( x = "Fire", y = "Dry mass removed (g/m2)")


    # bringing in biomass
       biomass <- read.csv(here("data/output/spp_biomass_plot.csv"))

       biomass_plot <- biomass %>%
         group_by(SITE, TREAT, QUAD,PLOT) %>%
         summarise(Biomass_sum = sum(Biomass_sum))

       biomass_plot$Biomass_sum_plot <- NA

       biomass_plot$Biomass_sum_plot[biomass_plot$QUAD == "1"] <- biomass_plot$Biomass_sum[biomass_plot$QUAD == "1"] * 4
       biomass_plot$Biomass_sum_plot[biomass_plot$QUAD == "2"] <- biomass_plot$Biomass_sum[biomass_plot$QUAD == "2"] * 2
       biomass_plot$Biomass_sum_plot[biomass_plot$QUAD == "0.2"] <- biomass_plot$Biomass_sum[biomass_plot$QUAD == "0.2"] * 20

       biomass_plot <- as.data.frame(biomass_plot)

     biomass_plot <- biomass_plot %>%
       select(!c(QUAD, Biomass_sum))

       test <- merge(mc_plot, biomass_plot, by = c("PLOT", "TREAT", "SITE"))

       test$Biomass.gm2 <- test$Biomass_sum_plot/400

       test <- test %>%
         rename("browse.gm2" = "g.m2") %>%
         select(!c(m2, Biomass_sum_plot, MASS_REMOVED))

       test$diff <- test$browse.gm2 / test$Biomass.gm2* 100



       allom_plot <- ggplot(allom, aes(x = WIDTH_IN, y = DRY_WEIGHT)) + geom_jitter(shape = 1) +
         labs( x = "Width of browse (inch)",
               y = "Aboveground Carbon (grams)",
               title = "Allometric equation of Aboveground Carbon removal") +
         theme(text = element_text(family = "Times New Roman"))


       allom_plot

       biomass_plot <- ggplot(test, aes(x = as.factor(TREAT), y = diff, fill = SITE)) +
         geom_boxplot() + ylim(c(0,100)) +
         labs(x = "Number of fires", y = "100% carbon removed",
              title = "Estimated Aboveground Carbon removed by herbivores") +
         scale_fill_manual(name = "Site Type",
                           values = c("#bababa", "#404040"),
                           labels = c("Upland", "Lowland")) +
         panel_border() + background_grid() +
         theme(text = element_text(family = "Times New Roman"))


       # for proposal
       plot_grid(allom_plot, biomass_plot, nrow = 2, ncol = 1,
                 rel_widths = c(1,2),
                 labels = c("A.", "B.")) # 550 x 640
