# scaling active layer by date of sampling

# climate data
# from akclimate.org
# https://akclimate.org/data/data-portal/

    # use Circle Hot Springs for Steese
    # using Fairbanks for Denver

# julian dates

# Start ######################################
library(tidyverse)
library(here)
library(cowplot)
theme_set(theme_cowplot())
library(lubridate)

'%!in%' <- function(x,y)!('%in%'(x,y))

## active layer #####################################
active <- read.csv(here("data/soil/thaw_depth.csv"))

  ### julian dates ######################
    # converts date column from character to date
    active$DATE <- as.Date(active$DATE, "%m/%d/%y")

    # converts to julian
    active$julian <- yday(active$DATE)

## climate ##################################################
### steese ###################################
ste_temp <- read.csv(here("data/climate/dailyTemp_Steese.csv"), skip = 5)

ste_temp <- ste_temp %>%
  rename("AvTempF" = "Mean.Temperature..degF.")
ste_temp$SITE <- "STEESE"

  ####  start date ########
  stee_Warm <- ste_temp[ste_temp$AvTempF >= 0,]

  ste_temp[ste_temp$Date %!in% stee_Warm$Date,]
  # 2019-02-25 # julian 56

  rm(stee_Warm)

  #### julian dates ####################
  str(ste_temp$Date)
  ste_temp$Date <- as.Date(ste_temp$Date, "%Y-%m-%d")

    # converts to julian
    ste_temp$julian <- yday(ste_temp$Date)

    ste_temp <- ste_temp %>%
      filter(julian >= 56)

### dalton ###############################################
dalt_temp <- read.csv(here("data/climate/dailyTemp_Dalt.csv"), skip = 5)

dalt_temp <- dalt_temp %>%
  rename("AvTempF" = "Mean.Temperature..degF.")
dalt_temp$SITE <- "DALTON"

    ####  start date ########
    dalt_Warm <- dalt_temp[dalt_temp$AvTempF >= 0,]

    dalt_temp[dalt_temp$Date %!in% dalt_Warm$Date,]
    # 2019-02-23 # julian 54

    rm(dalt_Warm)

    #### julian dates ##########################
    str(dalt_temp$Date)
    dalt_temp$Date <- as.Date(dalt_temp$Date, "%Y-%m-%d")

    # converts to julian
    dalt_temp$julian <- yday(dalt_temp$Date)

    dalt_temp <- dalt_temp %>%
      filter(julian >=56)

# ### combining ######################
# AvTemp <- rbind(ste_temp, dalt_temp)

# partial ##################################
# 1) Compute thawing degree days up to the moment of your measurements (TDD_partial).
# number of days with mean daily temperature above 32F/0C before measurement date

   dalt_active <-  active %>%
      filter(SITE == "DALTON")
    unique(dalt_active$julian)
    dalt_active$TDD_partial <- dalt_active$julian - 54

    ste_active <- active %>%
      filter(SITE == "STEESE")

    unique(ste_active$julian)
    ste_active$TDD_partial <- ste_active$julian - 54


# 2) Compute thawing degree days to the end of August, beginning of September (TDD_summer)
# number of days with mean daily temperature above 32F/0C after measurement date
# and before september / august

    dalt_active$TDD_summer <- 303 - dalt_active$julian

    ste_active$TDD_summer <- 303 - ste_active$julian

# 3) thaw depth * sqrt(TDD_summer) / sqrt(TDD_partial)

    dalt_active$extrap <- dalt_active$DEPTH_CM * sqrt(dalt_active$TDD_partial) / sqrt(dalt_active$TDD_summer)

    ste_active$extrap <- ste_active$DEPTH_CM * sqrt(ste_active$TDD_partial) / sqrt(ste_active$TDD_summer)

    active <- rbind(dalt_active, ste_active)

    active %>%
      filter(HIT == "FROZEN") %>%
      ggplot(aes(x = as.factor(TREAT), y = extrap, fill = SITE)) + geom_boxplot()
