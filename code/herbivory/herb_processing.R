# processing herbivory data
# 9/2/20

library(tidyverse)
library(here)

herb <- read.csv(here("data/herbivory/Herb_raw.csv"), stringsAsFactors = F)

# taking out extra columns
herb <- subset(herb, select = -c(X, X.1))

# adding divisions
  unique(herb$SPP)
  herb$DIV[herb$SPP == "BENE"] <- "d"
  herb$DIV[herb$SPP == "ALCR"] <- "d"
  herb$DIV[herb$SPP == "POTR"] <- "d"
  herb$DIV[herb$SPP == "SALIX"] <- "d"
  herb$DIV[herb$SPP == "PIME"] <- "c"
  herb$DIV[herb$SPP == "PIGL"] <- "c"
  herb$DIV[herb$SPP == "ARCTO"] <- "d"
  herb$DIV[herb$SPP == "POBA"] <- "d"

# renaming columns
  herb <- herb %>%
    rename(PLOT = SITECODE)

# UNITS
  # transforming height from feet to meters
    herb$HEIGHT_M <- herb$HEIGHT_FT/3.281
    herb$HEIGHT_M <- round(herb$HEIGHT_M, digits = 1)

  # height from inches to meters
    herb$HEIGHT_M[is.na(herb$HEIGHT_M)] <- herb$HEIGHT_IN[is.na(herb$HEIGHT_M)]/39.37
    herb$HEIGHT_M <- round(herb$HEIGHT_M, digits = 1)

  # dbh from inches to cm
    herb$DBH_CM[is.na(herb$DBH_CM)] <- herb$DBH_INCH[is.na(herb$DBH_CM)] *2.54
    herb$DBH_CM <- round(herb$DBH_CM, digits = 2)

  # removing extra columns (to keep things clear)
    herb <- subset(herb, select = -c(HEIGHT_FT, HEIGHT_IN, DBH_INCH))

# filling in maximum browse width
    herb$MAX_B_WIDTH[is.na(herb$MAX_B_WIDTH)] <- 0

write.csv(herb, here("data/herbivory/herb.csv"), row.names = F)
