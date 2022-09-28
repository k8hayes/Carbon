# formatting soil sample data sheet

# input: soil_samples_raw.csv
# output: soil_samples.csv

# Start ###################
library(tidyverse)
library(here)
library(naniar)
# Creating a not in operator:
`%!in%` <- Negate(`%in%`)

# Format Soil ################################################

soil <- read.csv(here("data/soil/soil_samples_raw.csv"))
soil <- soil %>%
  filter(VIAL != 970) %>%
  filter(VIAL != 550) %>%
  filter(VIAL != 98) %>%
  filter(VIAL != 99) %>%
  filter(VIAL != 419)  %>%
  mutate(TREAT = case_when( # renaming treatment
    endsWith(.$PLOT, "_0") ~ 0,
    endsWith(.$PLOT,"_1") ~ 1,
    endsWith(.$PLOT,"_2") ~ 2,
    endsWith(.$PLOT,"_3") ~ 3,)) %>%
  mutate(DIVID = case_when( # 14 SUBDIVIDED PLOTS
    PLOT == "11_0" ~ "Y", PLOT == "58_0" ~ "Y",
    PLOT == "10_0" ~ "Y", PLOT == "12_1" ~ "Y",
    PLOT == "64_1" ~ "Y", PLOT == "65_1" ~ "Y",
    PLOT == "52_1" ~ "Y", PLOT == "50_1" ~ "Y",
    PLOT == "32_2" ~ "Y", PLOT == "56_2" ~ "Y",
    PLOT == "57_2" ~ "Y", PLOT == "47_2" ~ "Y",
    PLOT == "37_3" ~ "Y", PLOT == "15_3" ~ "Y",
    TRUE ~ "N")) %>%
  mutate(SITE = case_when( # 24 dalton
    PLOT == "11_0" ~ "DALTON", PLOT == "58_0" ~ "DALTON",
    PLOT == "10_0" ~ "DALTON", PLOT == "44_0" ~ "DALTON",
    PLOT == "12_1" ~ "DALTON", PLOT == "64_1" ~ "DALTON",
    PLOT == "65_1" ~ "DALTON", PLOT == "52_1" ~ "DALTON",
    PLOT == "41_1" ~ "DALTON", PLOT == "48_1" ~ "DALTON",
    PLOT == "42_1" ~ "DALTON", PLOT == "50_1" ~ "DALTON",
    PLOT == "32_2" ~ "DALTON", PLOT == "39_2" ~ "DALTON",
    PLOT == "56_2" ~ "DALTON", PLOT == "57_2" ~ "DALTON",
    PLOT == "47_2" ~ "DALTON", PLOT == "16_2" ~ "DALTON",
    PLOT == "8_2" ~ "DALTON", PLOT == "40_2" ~ "DALTON",
    PLOT == "37_3" ~ "DALTON", PLOT == "15_3" ~ "DALTON",
    PLOT == "55_3" ~ "DALTON", PLOT == "54_3" ~ "DALTON",
    PLOT == "14_3" ~ "DALTON", PLOT == "7_3" ~ "DALTON",
    TRUE ~ "STEESE"
  ))

## bag weight #####################################

bag_w <- read.csv(here("data/soil/soil_samples_weight.csv"))
bag_w <- bag_w %>%
  mutate(TREAT = case_when(
    endsWith(.$PLOT, "_0") ~ 0,
    endsWith(.$PLOT,"_1") ~ 1,
    endsWith(.$PLOT,"_2") ~ 2,
    endsWith(.$PLOT,"_3") ~ 3,)) %>%
  mutate(DIVID = case_when( # 14 SUBDIVIDED PLOTS
    PLOT == "11_0" ~ "Y", PLOT == "58_0" ~ "Y",
    PLOT == "10_0" ~ "Y", PLOT == "12_1" ~ "Y",
    PLOT == "64_1" ~ "Y", PLOT == "65_1" ~ "Y",
    PLOT == "52_1" ~ "Y", PLOT == "50_1" ~ "Y",
    PLOT == "32_2" ~ "Y", PLOT == "56_2" ~ "Y",
    PLOT == "57_2" ~ "Y", PLOT == "47_2" ~ "Y",
    PLOT == "37_3" ~ "Y", PLOT == "15_3" ~ "Y",
    TRUE ~ "N" )) %>%
  mutate(SITE = case_when( # 24 dalton
    PLOT == "11_0" ~ "DALTON", PLOT == "58_0" ~ "DALTON",
    PLOT == "10_0" ~ "DALTON", PLOT == "44_0" ~ "DALTON",
    PLOT == "12_1" ~ "DALTON", PLOT == "64_1" ~ "DALTON",
    PLOT == "65_1" ~ "DALTON", PLOT == "52_1" ~ "DALTON",
    PLOT == "41_1" ~ "DALTON", PLOT == "48_1" ~ "DALTON",
    PLOT == "42_1" ~ "DALTON", PLOT == "50_1" ~ "DALTON",
    PLOT == "32_2" ~ "DALTON", PLOT == "39_2" ~ "DALTON",
    PLOT == "56_2" ~ "DALTON", PLOT == "57_2" ~ "DALTON",
    PLOT == "47_2" ~ "DALTON", PLOT == "16_2" ~ "DALTON",
    PLOT == "8_2" ~ "DALTON", PLOT == "40_2" ~ "DALTON",
    PLOT == "37_3" ~ "DALTON", PLOT == "15_3" ~ "DALTON",
    PLOT == "55_3" ~ "DALTON", PLOT == "54_3" ~ "DALTON",
    PLOT == "14_3" ~ "DALTON", PLOT == "7_3" ~ "DALTON",
    TRUE ~ "STEESE"))

## merge bag w w/ soil ##############################
test <- merge(soil, bag_w, by = c("SAMPLE", "VIAL", "PLOT", "CORNER",
                                  "ORG_MIN", "COARSE_FINE", "TREAT", "DIVID",
                                  "SITE"))

soil[soil$SAMPLE %!in% test$SAMPLE,]

test[soil$SAMPLE %!in% test$SAMPLE,]

bag_w[bag_w$SAMPLE  %!in% test$SAMPLE,  ]

soil <- test %>%
  select(!c( NOTES.x, DEPTH.y, BAG.x,)) %>%
  rename("DUP" = "DUPLICATES", "NOTES" = "NOTES.y",
         "BAG" = "BAG.y", "DEPTH" = "DEPTH.x")
rm(test, bag_w)

## Soil Depths #############################################################
# renaming to call depth section, and to give actual depth to depth
soil$DEPTH_CM[soil$DIVID == "N" & soil$ORG_MIN == "M"] <- 15
soil$DEPTH_CM[soil$DEPTH == "0_5" | soil$DEPTH == "10_15" | soil$DEPTH == "5_10" ] <- 5
soil$DEPTH[soil$DEPTH != "0_5" & soil$DEPTH != "10_15" & soil$DEPTH != "5_10" ] <- "FULL"


  # bringing in organic layer depths
  org_Depth <- read.csv(here("data/soil/org_depth.csv"))

  org_Depth$DEPTH <- "FULL"

  # adding sample labels to org_depth
  for(i in 1:length(org_Depth$PLOT)) {
    org_Depth$SAMPLE[i] <- paste(org_Depth$PLOT[i],org_Depth$CORNER[i], "_O", sep="")
  }

  length(unique(org_Depth$SAMPLE))

  org_soil <- soil %>%
    filter(ORG_MIN == "O")
  length(unique(org_soil$SAMPLE))

  # matching org depths with full file depth
test <- merge(org_soil, org_Depth, by = c("SAMPLE", "PLOT",
                                          "TREAT", "CORNER"))
minsoil <- soil %>%
  filter(ORG_MIN == "M")

test <- test %>%
  select(!c("SITE.x", "DEPTH.x", "DEPTH_CM")) %>%
  rename("SITE" = "SITE.y", "DEPTH_CM" = "ORG_DEPTH",
         "DEPTH" = "DEPTH.y")

soil <- rbind(test, minsoil)
rm(minsoil, org_Depth, org_soil, test)

# dealing with 39_2
        # 39_2 is the one we redid
        # first attempt = 6/17, second 6/21
        # did not subdivid during 6/21 attempt
        # 0-5 measurements might just be the amount recovered on 6/17
    # taking out clear 6/17 measurements # leaving clear 6/21 + everything else
    soil <- soil[soil$VIAL != "866",]
    soil <- soil[soil$VIAL != "98",]
    soil <- soil[soil$VIAL != "99",]
    soil <- soil[soil$VIAL != "419",]

   # fixing errors found later
    # soil[soil$VIAL == 811,] # wrong plot number
    # soil$PLOT[soil$VIAL == 811] <- "28_1"

    # soil[soil$VIAL == 15,] # wrong plot number
    # soil$PLOT[soil$VIAL == 15] <- "47_2"

    # soil[soil$SAMPLE == "11_0SE_MF",]
    # soil$DEPTH[108] <- "5_10"

    # soil[soil$VIAL == 627,] # wrong depth
    # soil$DEPTH[soil$VIAL == 627] <- "5_10"

    # soil[soil$VIAL == 580,] # wrong corner
    # soil$CORNER[soil$VIAL == 580] <- "SE"
    # soil$SAMPLE[soil$VIAL == 580] <- "10_0SE_O"

    # soil[soil$VIAL == 734,]
    # soil$SAMPLE[soil$VIAL == 734] <- "34_2NE_MC"
    # soil$PLOT[soil$VIAL == 734] <- "34_2"
    # soil$CORNER[soil$VIAL == 734] <- "NE"
    # soil$DIVID[soil$VIAL == 734] <- "N"

    # soil[soil$VIAL == 812,]
    # soil$SAMPLE[soil$VIAL == 812] <- "56_2NW_MC"
    # soil$PLOT[soil$VIAL == 812] <- "56_2"
    # soil$TREAT[soil$VIAL == 812] <- 2

    # soil[soil$VIAL == 773,]
    # soil$SAMPLE[soil$VIAL == 773] <- "56_2NW_O"
    # soil$PLOT[soil$VIAL == 773] <- "56_2"
    # soil$TREAT[soil$VIAL == 773] <- 2
    #
    # soil[soil$VIAL == 339,] # missing corner
    # soil$SAMPLE[soil$VIAL == 339] <- "32_2C_MC"
    # soil$CORNER[soil$VIAL == 339] <- "C"
    #
    # soil[soil$VIAL == 313,]
    # soil$DEPTH[soil$VIAL == 313] <- ""
    #
    # soil[soil$VIAL ==  304,] # should be SW
    # soil$SAMPLE[soil$VIAL ==  304] <- "24_3SW_MF"
    # soil$CORNER[soil$VIAL ==  304] <- "SW"
    #
    # soil[soil$VIAL == 803,] # should be coarse
    # soil$COARSE_FINE[soil$VIAL == 803] <- "C"
    # soil$NOTES[soil$VIAL == 803] <- "COARSE - FIXED"
    #
    # soil[soil$VIAL == 121,]
    # soil$BAG[soil$VIAL == 121] <- 121
    # soil$NOTES[soil$VIAL == 121] <- "NO COARSE"
    #
    # soil[soil$VIAL == 134,]
    # soil$NOTES[soil$VIAL == 134] <- "NO COARSE"

    # soil[soil$VIAL == 591,]

    # soil[soil$VIAL == 577,] # should be SE
    # soil$SAMPLE[soil$VIAL == 577] <- "41_1SE_MC"
    # soil$CORNER[soil$VIAL == 577] <- "SE"

    # soil[soil$VIAL == 430,]


# one thing to fix: missing depth values
    # soil[soil$DEPTH == "",] # should be missing for organic layers, but not for mineral

    # # adding in no coarse lines
    #  no_coarse <- soil[soil$VIAL == "NO",]
    #  no_coarse$VIAL <- "NA"
    #  no_coarse$COARSE_FINE <- "C"
    #  no_coarse$SAMPLE
    #  no_coarse$SAMPLE <- c("47_2NW_MC",  "64_1SW_MC",  "37_3SE_MC",
    #                        "50_1SE_MC",  "50_1SE_MC",  "11_0C_MC",
    #                        "12_1SW_MC",  "12_1NW_MC", "65_1SE_MC")
    # # soil <- rbind(soil, no_coarse)
    # rm(no_coarse)

# Ordering ###############################################
# ordering soil data within corners and according to depth
order <- c("C", "SW", "SE", "NW", "NE")
depth_ord <- c("0_5", "5_10", "10_15")

test <- soil %>%
  group_by(PLOT, CORNER, COARSE_FINE, ORG_MIN) %>%
  arrange(match(CORNER, order), .by_group = T) %>%
  arrange(match(COARSE_FINE, depth_ord), .by_group = T)

write.csv(test, here("data/soil/soil_samples_format.csv"),
          row.names = FALSE)

soil <- read.csv(here("data/soil/soil_samples_format.csv"))

    rm(order, depth_ord)

# Closer looks ##################################
    dup <- soil %>%
      group_by(SAMPLE) %>%
      summarise(n = n()) %>%
      filter(n >1)
  check <- soil %>%
      filter(SAMPLE  %in% dup$SAMPLE)

  dup_vials <- check %>%
    group_by(SAMPLE, DEPTH) %>%
    summarise(n = n(), vial = min(VIAL), vial2 = max(VIAL)) %>%
    filter(n >1)

  dup <- soil %>%
    filter(VIAL %in% dup_vials$vial | VIAL %in% dup_vials$vial2)
  dup$DUP <- "DUP"
  dup_value <- as.vector(dup$VIAL)

  soil$DUP[soil$VIAL %in% dup_value] <- "DUP"

  rm(check, dup, dup_value)

  write.csv(soil, here("data/soil/soil_samples_format.csv"),
            row.names = FALSE)

  soil <- read.csv(here("data/soil/soil_samples_format.csv"))

  ## Mineral  ##############################
  ### Undivided  ##################
  # 1 coarse + 1 fine = 2 for each corner
  # 2 x 5 corners = 10 samples
  undivid <- soil %>%
    filter(ORG_MIN == "M") %>%
    filter(DIVID == "N") %>%
    filter(NOTES != "REDONE_DROP")
  unique(undivid$PLOT)
  length(unique(undivid$PLOT)) # 36
  # so 360 undivided mineral soil samples

  undivid_count <-  undivid %>%
    group_by(PLOT) %>%
    summarise(n())

  # checking sites with more than 10 samples # ALL FIXED
   undivid_count[undivid_count$`n()` > 10,]

         undivid[undivid$PLOT == "22_3",]
          soil$DUP[soil$VIAL == 350] <- "DUP"
          soil$DUP[soil$VIAL == 557] <- "DUP"

          undivid[undivid$PLOT == "23_3",]

          undivid[undivid$PLOT == "35_3",] # duplicate SE mineral fine layer # both say bag 133

          undivid[undivid$PLOT == "48_1",]
            soil$DUP[soil$VIAL == 375] <- "DUP"
            soil$DUP[soil$VIAL == 598] <- "DUP"

          undivid[undivid$PLOT == "55_3",]
            soil$DUP[soil$VIAL == 408] <- "DUP"
            soil$DUP[soil$VIAL == 417] <- "DUP"

        undivid[undivid$PLOT == "7_3",] # duplicate NE coarse, bag 117

        undivid[undivid$PLOT == "8_2",] # think the SW duplicates may be


  # checking the sites with less than 10 samples
  undivid_count[undivid_count$`n()` < 10,]

      undivid[undivid$PLOT == "1_0",] # missing all NE
      undivid[undivid$PLOT == "18_1",] # missing all SW
        soil$SAMPLE[soil$VIAL == 591] <- "18_1SW_MC"
        soil$SITE[soil$VIAL == 591] <- "STEESE"
        soil$PLOT[soil$VIAL == 591] <- "18_1"
        soil$DUP[soil$VIAL == 591] <- ""

        soil$SAMPLE[soil$VIAL == 215] <- "18_1SW_MF"
        soil$PLOT[soil$VIAL == 215] <- "18_1"
        soil$DUP[soil$VIAL == 215] <- "STEESE"
        soil$DUP[soil$VIAL == 215] <- ""

      undivid[undivid$PLOT == "31_0",] # missing NE and SE
      undivid[undivid$PLOT == "34_2",] # missing coarse NE

      undivid[undivid$PLOT == "39_2",] # missing NW

      undivid[undivid$PLOT == "4_2",] # missing center coarse and fine # bag 128 # no coarse, can't find fine

      undivid[undivid$PLOT == "44_0",] # missing coarse/fine 44_0NW

      undivid[undivid$PLOT == "5_1",] # missing coarse/fine 5_1center

      undivid[undivid$PLOT == "6_0",]

      undivid[undivid$PLOT == "9_0",]
  # double checking ones that should be correct
  undivid_count[undivid_count$`n()` == 10,]

      # undivid[undivid$PLOT == "16_2",]
      # undivid[undivid$PLOT == "17_3",]

       undivid[undivid$PLOT == "18_1",] # double NE, missing SW

      # undivid[undivid$PLOT == "2_3",]
      # undivid[undivid$PLOT == "20_1",]

       undivid[undivid$PLOT == "23_3",] # extra NE fine, missing SW organic

       # undivid[undivid$PLOT == "25_3",]
       # undivid[undivid$PLOT == "26_2",]
       # undivid[undivid$PLOT == "27_2",]
       # undivid[undivid$PLOT == "28_1",]
       # undivid[undivid$PLOT == "29_1",]
       # undivid[undivid$PLOT == "3_2",]
       # undivid[undivid$PLOT == "33_1",]
       # undivid[undivid$PLOT == "34_2",]
       # undivid[undivid$PLOT == "36_1",]
       # undivid[undivid$PLOT == "40_2",]
       # undivid[undivid$PLOT == "41_1",] # extra SW C, missing SE C # fixed above
       # undivid[undivid$PLOT == "42_1",]
       # undivid[undivid$PLOT == "54_3",]

  ### Divided #################################################
   # 3 each of coarse and fine so 6 for each corner
   # so, 6 x 5 corners = 35 samples
        divid <- soil %>%
          filter(DIVID == "Y") %>%
          filter(NOTES != "REDONE_DROP")

        divid_count <-  divid %>%
           group_by(PLOT) %>%
           summarise(n())

        divid_count[divid_count$`n()` < 35,]

        divid[divid$PLOT == "47_2",]
        divid[divid$PLOT == "50_1",]
        divid[divid$PLOT == "56_2",]

        divid_count[divid_count$`n()` > 35,]
        divid[divid$PLOT == "10_0",] # dup marked
        divid[divid$PLOT == "52_1",]# dup marked

        divid_count[divid_count$`n()` == 35,]
        divid[divid$PLOT == "15_3",]
        divid[divid$PLOT == "57_2",] # missing NW organic

  ## Organic ####################################
        orgsoil <- soil %>%
          filter(ORG_MIN == "O") %>%
          filter(NOTES != "REDONE_DROP")

        org_count <-  soil %>%
          filter(ORG_MIN == "O") %>%
          filter(NOTES != "REDONE_DROP") %>%
          group_by(PLOT) %>%
          summarise(n())

        org_count[org_count$`n()` < 5,]

        org_count[org_count$`n()` > 5,]

        orgsoil[orgsoil$PLOT == "11_0",] # 2 NE # same bag, can drop 1
        orgsoil[orgsoil$PLOT == "14_3",] # two SW, different bag number, 62 and 63
        orgsoil[orgsoil$PLOT == "19_2",]
        orgsoil[orgsoil$PLOT == "22_3",] # two SW, no bag number
        orgsoil[orgsoil$PLOT == "24_3",] # two SE, no bag

        orgsoil[orgsoil$PLOT == "55_3",] # two SW, same bag
        orgsoil[orgsoil$PLOT == "56_2",] # 3 center, one repeat, one double bag
          soil$NOTES[soil$VIAL == 648] <- "REDONE_DROP"
        orgsoil[orgsoil$PLOT == "8_2",] # double NE, same bag #

# Export ##################################
soil$DRY_WEIGHT <- soil$DRY_WEIGHT_BAG + soil$VIAL_WEIGHT

write.csv(soil, here("data/soil/soil_samples_format.csv"),
                  row.names = FALSE)




