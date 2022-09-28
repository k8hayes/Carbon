# data clean up - raw EA data

# input: EA_abb_raw.csv
# output: output/soil.csv

# Set up #####################################
library(tidyverse)
library(here)
library(cowplot)
library(janitor)
library(naniar) # using for NAs
theme_set(theme_cowplot())

# Creating a not in operator:
 `%notin%` <- Negate(`%in%`)

ea_raw <- read.csv(here("data/EA/EA_abb_raw.csv"))
colnames(ea_raw)

unk <- ea_raw %>% # taking out standards # doesn't take out duplicates
  filter(Sample_type == "unknown")

# Adding soil ###############################################
soil <- read.csv(here("data/soil/soil_samples_format_weight.csv"))

# which of the vials in the total soil dataframe have been run
done <- soil[which(soil$VIAL %in% unk$Sample == TRUE),]

  # out of those, which of them aren't in the soil datasheet
  unk$Sample[which(unk$Sample %notin% done$VIAL == TRUE)]

  soil$VIAL <- as.numeric(soil$VIAL)

    # missing values are the 4 39_2 samples I took out, plus the mystery 550 sample

  # CHECK 970 and 436

    # taking them out of the unknown dataframe for now
    unk <- unk[unk$Sample != "436",]
    unk <- unk[unk$Sample != "970",]

    unk <- unk %>%
      rename("VIAL" = "Sample")

    data <- merge(unk, done, by = "VIAL")
    rm(unk,done,ea_raw)

    data <- data %>%
      select(!c(BAG, NOTES))

# assigning NAs
data <- data %>%
      replace_with_na_all(condition = ~.x %in% common_na_strings)

dup <- data %>% # looking for duplicates in dataset
  group_by(VIAL) %>%
  summarise(n = n()) %>%
  filter(n > 1)

    # want to look for which duplicates have NAs so I can drop them

    data$dup <- ifelse(data$VIAL %in% dup$VIAL, 1, 0) # creates vector of 1s and 0s
                                                      # 1 if it's a match # adding as column to data so i can index
    check_Dup <- data[data$dup == 1,]
    check_Dup

    missing <- check_Dup[is.na(check_Dup$C_weight_mg),]
    # can take out duplicates where 1 is missing
      # 251, 309, 409, 420, 700, 94

  # adding row numbers to data so I can use them to index for missing values
    data$ID <- seq.int(nrow(data))

    # taking out

              data$ID[data$VIAL== 251] # one to take out is 178
              data <- data[data$ID != 178,]

              data$ID[data$VIAL == 309] # take out 248
              data <- data[data$ID != 248,]

              data[data$VIAL == 409,] # first
              data$ID[data$VIAL == 409] # take out 359
              data <- data[data$ID != 359,]

              data[data$VIAL == 420,] # first
              data$ID[data$VIAL == 420] # take out 372
              data <- data[data$ID != 372,]

              data[data$VIAL == 700,] # first
              data$ID[data$VIAL == 700] # take out 711
              data <- data[data$ID != 711,]

              data[data$VIAL == 94,] # second
              data$ID[data$VIAL == 94] # take out 1007
              data <- data[data$ID != 1007,]


              rm(missing)

      # now looking at duplicates that aren't NAs
      check_Dup <- check_Dup[is.na(check_Dup$C_weight_mg) == FALSE,] # dropping NAs

      check_Dup$C_weight_mg <- as.numeric(check_Dup$C_weight_mg)

      diff <- check_Dup %>%
        group_by(VIAL) %>%
        summarise(AV = mean(C_weight_mg), SD = sd(C_weight_mg))

      diff$relSD <- diff$SD / diff$AV

      diff %>%
        filter(relSD > 0.1) %>%
        ggplot( aes(x = VIAL, y = relSD)) +
               geom_point()

      big_diff <- diff %>%
        filter(relSD > 0.1)

      redo <- big_diff$VIAL
      redo <- as.numeric(redo)
      redo <- c(redo, 876, 909, 429, 440, 529)
      redo

# plotting
  # NOTE: haven't taken duplicates out
      # may do that / average them for the final graph?

write.csv(data, here("data/output/EA_processed.csv"), row.names = F)
