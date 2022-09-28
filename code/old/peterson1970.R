# Peterson, Chan and Cragg 1970
# species: aspen

#############################################################################
# quaking aspen # peterson 1970

# NOTE: no standard errors, at least in table from Stanek and State

potr <- dbh %>%
  filter(SPP == "POTR")
hist(potr$DBH) # everything smaller than 6 cm
summary(potr$DBH) # min 0.1, max 6.5

##########################
# testing total aboveground biomass equations
# only testing those that only require dbh

# first equation in table
tab1_potr <- function(x) (-1.1115 + 2.3466*log(x))^10
# n = 49, r2 = 0.979
  # SE
    # 0.099


potr$tab1 <- tab1_potr(potr$DBH)
potr$tab2 <- tab2_potr(potr$DBH)

plot(potr$tab1 ~ potr$DBH)
points(potr$tab2 ~ potr$DBH, col = "red")
abline(v = c(0.5,6.5), col = "red") # my ranges
abline(v = c(4.5, 33), col = "blue", lty = 2) # equation's ranges

# doesn't work for DBH < 1 cm
# which is unfortunately most of mine...

########################
# testing out calculating each component

# FOLIAGE
fol1_potr <- function(x) (-2.3011 + 1.9742*log(x))^10
# n = 49, r2 = 0.97

fol2_potr  <- function(x) (-2.3011 + 1.9742*log(x))^10
# n = 31, r2 = 0.942
# SE
# dbh2 = 0.25

potr$fol1 <- fol1_potr(potr$DBH)
potr$fol2 <- fol2_potr(potr$DBH)

# LIVE BRANCHES
branch1_potr <- function(x) 2102.347 - 722.293*x + 68.792*(x^2)
# n = 23, r2 = 0.770
# SE
# intercept = 1680.90
# dbh = 353.52
# dbh2 = 17.13

branch2_potr <- function(x) -325.581*x + 52.188*(x^2)
# n = 23, r2 = 0.867
# SE
# dbh = 158.19
# dbh2 = 10.98

potr$branch1 <- branch1_potr(potr$DBH)
potr$branch2 <- branch2_potr(potr$DBH)

# STEMWOOD
stemw1_potr <- function(x) 3842.733 - 2388.907*x + 370.644*(x^2)
# n = 39, r2 = 0.975
# SE
# intercept = 1627.52
# dbh = 469.52
# dbh2 = 25.87

stemw2_potr <- function(x) -1445.992*x + 325.786*(x^2)
# n = 39, r2 = 0.981
# SE
# dbh = 261.73
# dbh2 = 18.62

potr$stem1 <- stemw1_potr(potr$DBH)
potr$stem2 <- stemw2_potr(potr$DBH)

# BARK
bark1_potr <- function(x) -554.585 + 47.289*(x^2)
# n = 21, r2 = 0.945
# SE
# intercept = 458.92
# dbh2 = 2.61

bark2_potr <- function(x) 44.621*(x^2)
# n = 21, r2 = 0.980
# SE
# dbh2 = 1.41

potr$bark1 <- bark1_potr(potr$DBH)
potr$bark2 <- bark2_potr(potr$DBH)

# summing up components
potr$sum1 <- potr$fol1 + potr$branch1 + potr$stem1 + potr$bark1
potr$sum2 <- potr$fol2 + potr$branch2 + potr$stem2 + potr$bark2

plot(potr$sum1 ~ potr$DBH)
points(potr$sum2 ~ potr$DBH, col = "green")
points(potr$tab1 ~ potr$DBH, col = "red")
points(potr$tab2 ~ potr$DBH, col = "blue")
abline(v = c(0.1,26), col = "red") # my ranges
abline(v = c(0, 12.8), col = "blue", lty = 2) # equation's ranges

# need to find different aspen equations
