# cleaning up dbh
# most comes from Regen_analysis from tree regen manuscript
library(tidyverse)
library(here)

dbh <- read.csv(here("data/dbh_raw.csv"))

  # merging salix # so many willows, so little time
  dbh$SPP[dbh$SPP == "SAGL"] <- "SALIX"
  dbh$SPP[dbh$SPP == "SA_3"] <- "SALIX"
  dbh$SPP[dbh$SPP == "SA_4"] <- "SALIX"
  dbh$SPP[dbh$SPP == "SA_5"] <- "SALIX"
  dbh$SPP[dbh$SPP == "SA_6"] <- "SALIX"
  dbh$SPP[dbh$SPP == "SA_7"] <- "SALIX"
  dbh$SPP[dbh$SPP == "SA_8"] <- "SALIX"
  dbh$SPP[dbh$SPP == "SA_?"] <- "SALIX"
  dbh$SPP[dbh$SPP == "SADE"] <- "SALIX"
  dbh$SPP[dbh$SPP == "SAPU"] <- "SALIX"
  dbh$SPP[dbh$SPP == "SAGL_R"] <- "SALIX"

   # dropping unknown
     dbh <- dbh %>%
       filter(SPP != "UNKNOWN") # only a few unknown species present at Dalton site

 # adding conifer and deciduous divisions
    dbh$DIV[dbh$SPP == "PIME"] <- "c"
    dbh$DIV[dbh$SPP == "BENE"] <- "d"
    dbh$DIV[dbh$SPP == "POTR"] <- "d"
    dbh$DIV[dbh$SPP == "POBA"] <- "d"
    dbh$DIV[dbh$SPP == "SALIX"] <- "d"
    dbh$DIV[dbh$SPP == "ARCTO"] <- "d"
    dbh$DIV[dbh$SPP == "ALCR"] <- "d"

# adding expansion factors
    # assuming a plot is 1/25 of a ha, so half a plot is 1/50 of a ha.
  dbh$EXP_FACT[dbh$QUAD == 2] <- 50
  dbh$EXP_FACT[dbh$QUAD == 1] <- 100
  dbh$EXP_FACT[dbh$QUAD == .1] <- 1000
  dbh$EXP_FACT[dbh$QUAD == .2] <- 500

 # adding age column
  age <- read.csv(here("data/site_age.csv"))
  
  age <- age %>%
    select(c(SITE, TREAT, PLOT, USE)) %>%
    rename(AGE = USE)
  
 dbh <- merge(dbh, age, by = c("SITE", "TREAT", "PLOT"))
 
write.csv(dbh, here("data/dbh.csv"), row.names = F)
