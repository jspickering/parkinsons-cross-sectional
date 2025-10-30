############################################################################################################
# Script for analysis of the Go/No-Go task according to the pre-registration on the Open Science Framework #
# Link:                                                                                                    #
# Authors: Jade Pickering & Marta Majewska, 2020                                                           #
############################################################################################################

##########
# SET-UP #
##########

# load all packages and install if necessary
requiredPackages = c('tidyverse', 'janitor', 'trimr')
# requiredPackages = c('ggplot2','broom','janitor', 'gdata', 'beepr', 'cowplot', 'viridis', 'patchwork', 'tidyverse')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

# task info
expected_trials = 360
trial_threshold = 0.8
needed_trials = expected_trials * trial_threshold

# empty exclusions table to fill in later
exclusions <- tibble(
  participant = character(),
  group = character(),
  reason = character()
  )

################
# READ IN DATA #
################

### read in and tidy data
go_no_go_raw <- read_csv("raw_data/go_no_go_dummy_data.csv") %>%
  # put variables names in snake_case
  clean_names() %>%
  # remove any rows without a participant
  drop_na(group) %>%
  # convert data types for all columns to make sure they're correct
  mutate(
    across(c(x1,
             group,
             participant,
             condition,
             trial_acc
    ), as.character),
    across(c(trial_num,
             trial_rt
    ), as.numeric)
  ) %>%
  # remove YC participants as this was collected for MM's project
  subset(group != "YC")

go_no_go_tidy <- go_no_go_raw %>%
  # recode trial accuracy - CHECK this as I can't test it with this dummy data
  mutate(trial_acc = if_else(trial_acc == "miss", "0",
                             if_else(trial_acc == "1", "1",
                                     if_else(trial_acc == "0", "0",
                                             "NA"))))

# Exclude participants who completed less than 80% of expected trials
exc_expected_trials <- go_no_go_tidy %>%
  group_by(group, participant) %>%
  summarise(n_trials = n()) %>%
  filter(n_trials < needed_trials) %>%
  transmute(participant,
            group,
            reason = "not enough trials")

# put exclusions into the main exclusions table
exclusions <- bind_rows(exclusions,
                        exc_expected_trials)

# and filter these exclusions out of the main data going forwards
go_no_go_tidy <- go_no_go_tidy %>%
  filter(!participant %in% exclusions$participant)


