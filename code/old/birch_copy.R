#############################################################################
# starting off just with birch

birch <- dbh %>%
  filter(SPP == "BENE")
hist(birch$DBH) # big stuff (dbh > 18) is all unburned sites, all dead
summary(birch$DBH)

##########################
# testing total aboveground biomass equations
# only testing those that only require dbh

# first equation in table
EQ_tab1_bene <- function(x) -16809 + (2656.482*x) + (284.416*(x^2))
# n = 19, r2 = 0.98
# SE
# intercept = 10934
# dbh = 1462.65
# dbh2 = 42.72
tab1_beneSE <- sqrt((10934^2) + (1462.65^2) + (42.72^2))

# second equation in table
# tab2_bene <- function(x) 361.738*(x^2)
# n = 19, r2 = 0.987
# SE
# dbh2 = 9.89
# tab2_beneSE <- sqrt((9.89)^2)

birch$EQ_tab1 <- EQ_tab1_bene(birch$DBH)
# birch$tab2 <- tab2_bene(birch$DBH)

# birch$SE_tab1 <- tab1_beneSE
# birch$SE_tab2 <- tab2_beneSE

plot(birch$EQ_tab1 ~ birch$DBH)
# points(birch$tab2 ~ birch$DBH, col = "red")
abline(v = c(0.1,23.5), col = "red") # my ranges
abline(v = c(3.7, 32.7), col = "blue", lty = 2) # equation's ranges

# testing out how to do this in ggplot
# ggplot(birch, aes(x = DBH)) +
#   geom_point(aes(y = tab1)) +
#   geom_point(aes(y = tab2, col = "red")) +
#   geom_ribbon(aes(ymin = tab1 -tab1_beneSE,
#                   ymax = tab1 + tab1_beneSE,
#                   alpha = 0.3)) +
#   geom_vline(xintercept = c(0.1, 23.5), linetype = "dashed") +
#   geom_vline(xintercept = c(3.7, 32.7), linetype = "dashed", col = "blue") +
#   geom_ribbon(aes(ymin = tab2 - tab2_beneSE,
#                   ymax = tab2 + tab2_beneSE,
#                   alpha = 0.3, col = "red")) +
#   theme(legend.position = "none") +
#   labs(x = "DBH (cm)", y = "Dry Biomass (g)", title = "Alaskan Birch (Total Above. Biomass)")

# seems like second equation (red) might over-estimate smaller trees

########################
# testing out calculating each component

# FOLIAGE
#fol1_bene <- function(x) -1494.788 + (331.746*x) - 4.784*(x^2)
# n = 19, r2 = 0.776
# SE
# intercept = 612.75
# dbh = 81.96
# dbh2 = 2.39
# fol1_beneSE <- sqrt((612.76)^2 + (81.96)^2 + (2.39)^2)

fol2_bene  <- function(x) 145.892*x
# n = 19, r2 = 0.877
# SE
# dbh = 12.89
fol2_beneSE <- sqrt(12.89^2)

#birch$fol1 <- fol1_bene(birch$DBH)
birch$fol2 <- fol2_bene(birch$DBH)

# LIVE BRANCHES
# branch1_bene <- function(x) 3322.457 - 883.631*x + 92.016*(x^2)
# n = 19, r2 = 0.927
# SE
# intercept = 4050.06
# dbh = 541.76
# dbh2 = 15.83
# branch1_beneSE <- sqrt((4050.06)^2 + (541.76)^2 + (15.83)^2)

branch2_bene <- function(x) -472.032*x + 81.536*(x^2)
# n = 19, r2 = 0.949
# SE
# dbh = 202.38
# dbh2 = 9.25
branch2_beneSE <- sqrt((202.38)^2  + (9.25)^2)

# birch$branch1 <- branch1_bene(birch$DBH)
birch$branch2 <- branch2_bene(birch$DBH)

# STEMWOOD
# stemw1_bene <- function(x) -30966 + 5360.216*x + 90.984*(x^2)
# n = 11, r2 = 0.967
# SE
# intercept = 16521
# dbh = 2054.69
# dbh2 = 55.98
# stemw1_beneSE <- sqrt((16521)^2 + (2054.69)^2 + (55.98)^2)

stemw2_bene <- function(x) 1783.003*x + 177.304*(x^2)
# n = 11, r2 = 0.978
# SE
# dbh = 860.5
# dbh2 = 35.99
stemw2_beneSE <- sqrt(860.5^2 + 35.99^2)

# birch$stem1 <- stemw1_bene(birch$DBH)
birch$stem2 <- stemw2_bene(birch$DBH)

# BARK
# bark1_bene <- function(x) -1044.436 + 50.747*(x^2)
# n = 10, r2 = 0.989
# SE
# intercept = 753.22
# dbh2 = 1.86
# bark1_beneSE <- sqrt(753.22^2 + 1.86^2)

bark2_bene <- function(x) 49.01*(x^2)
# n = 10, r2 = 0.992
# SE
# dbh2 = 1.44
bark2_beneSE <- sqrt(1.44^2)

# birch$bark1 <- bark1_bene(birch$DBH)
birch$bark2 <- bark2_bene(birch$DBH)

# summing up components
# birch$sum1 <- birch$fol1 + birch$branch1 + birch$stem1 + birch$bark1
# birch$sum2 <- birch$fol2 + birch$branch2 + birch$stem2 + birch$bark2
birch$EQ_sumR <- birch$fol2 + birch$branch2 + birch$stem2 + birch$bark2

# SE_sum1 <- sqrt(fol1_beneSE^2 + branch1_beneSE^2 + stemw1_beneSE^2 + bark1_beneSE^2)
sumR_SE <- sqrt(fol2_beneSE^2 + branch2_beneSE^2 + stemw2_beneSE^2 + bark2_beneSE^2)

# plot(birch$sum1 ~ birch$DBH)
# points(birch$sum2 ~ birch$DBH, col = "green")
# points(birch$tab1 ~ birch$DBH, col = "red")
# points(birch$tab2 ~ birch$DBH, col = "blue")

birch <- birch %>%
  pivot_longer(
    cols = starts_with("EQ"),
    names_to = "EQ",
    values_to = "Biomass")

# clean up variable name
birch$EQ[birch$EQ == "EQ_tab1"] <- "tab1"
birch$EQ[birch$EQ == "EQ_sumR"] <- "sumR"

# adding in standard errors
birch$SE <- NA
birch$SE[birch$EQ == "tab1"] <- tab1_beneSE
birch$SE[birch$EQ == "sumR"] <- sumR_SE

# birch <- subset(birch, select = -c(fol1, fol2, bark1, bark2, stem1, stem2, branch1, branch2))
birch <- subset(birch, select = -c(fol2, bark2, stem2, branch2))

labs <- c("Observed Range", "Equation Range")
ggplot(birch, aes(x = DBH)) +
  geom_point(aes(y = Biomass, col = EQ)) +
  geom_ribbon(aes(ymax = Biomass + SE, ymin = Biomass - SE, fill = EQ), alpha = .3) +
  geom_vline(xintercept = c(0.1, 23.5), linetype = "dashed") +
  geom_vline(xintercept = c(3.7, 32.7), linetype = "dashed", col = "blue")  +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Sum of Components", "Tot. Above. Biomass"),
                     name = "Equation") +
  scale_fill_manual(values = c("red", "blue"),
                    labels = c("Sum of Components", "Tot. Above. Biomass"),
                    name = "Equation") +
  labs(x = "DBH (cm)", y = "Dry Biomass (g)", title = "Alaskan Birch (Total Above. Biomass)") +
  geom_text(mapping = aes(x = 1.5, y = 185000, label = "Observed Range"),
            angle = 90, size = 3, check_overlap = TRUE) +
  geom_text(mapping = aes(x = 5, y = 185000, label = "Equation Range"),
            angle = 90, size = 3, color = "Blue", check_overlap = TRUE) +
  ylim(c(0, 225000))

# seems to show total aboveground biomass estimates actually calculate negative biomoass
# (at small DBHs)
# should stick with sum of components for the first pass
