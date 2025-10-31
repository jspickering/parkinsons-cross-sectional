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
anticipatory_rt = 150

# empty exclusions table to fill in later
exclusions <- tibble(
  participant = character(),
  group = character(),
  reason = character()
  )

################
# READ IN DATA #
################

##### Access data from network drive
data_dir <- "Z:/Study 2_cross sectional/DATA/GNG/data" # file path to network drive
groups <- c("PWP", "ICD", "OC", "YC") # specify groups
group_dfs <- list() # create empty list to hold data for each group

for (g in groups) {
  # find all csv files in that group's folder
  files <- list.files(
    path = file.path(data_dir, g),
    pattern = "\\.csv$",
    full.names = TRUE
  )
  # read all the files in and combine into one df
  group_data <- map_df(files, ~ read_csv(.x, col_types = cols(.default = "c")))
  # clean column names and add a 'group' column
  group_data <- group_data %>%
    clean_names() %>%
    mutate(group = g)
  # store the group's df into a list
  group_dfs[[g]] <- group_data
}

# combine all group dfs into one big df
gng_raw <- bind_rows(group_dfs) #%>%
  # # testing which participant didn't have their ID in the participant  column - it was 072
  # select(participant, group) %>%
  # distinct(participant, group) %>%
  # arrange(participant)


##### Tidy data
gng_data <- gng_raw %>%
  # participant 082 was mislabelled as 083 (see researcher notes on participant pack)
  mutate(participant = if_else((participant == "083" & group == "PWP"), "082",
                               participant)) %>%
  # only keep the columns we're interested in
  select(participant,
         group,
         condition,
         trial_rt,
         trial_acc
         ) %>%
  # convert data types for all columns to make sure they're correct
  mutate(
    across(c(participant,
             group,
             condition,
             trial_acc
    ), as.character),
    across(c(trial_rt
    ), as.numeric)
  ) %>%
  # remove participats as this was collected for MM's project
  filter(group != "YC") %>%
  # recode misses as an accuracy of 0
  mutate(trial_acc = as.numeric(if_else(trial_acc == "miss", "0",
                              trial_acc)))

##### Look for exclusions

# Exclude participants who completed less than 80% of expected trials
exc_expected_trials <- gng_data %>%
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
gng_data <- gng_data %>%
  filter(!participant %in% exclusions$participant)

##### Accuracy: participant level

gng_accuracy <- gng_data %>%
  group_by(participant, group) %>%
  summarise(
    commission_error_perc = mean(trial_acc[condition == "0"] == 0, na.rm = TRUE) * 100,
    omission_error_perc = mean(trial_acc[condition == "1"] == 0, na.rm = TRUE) * 100,
    .groups = "drop"
  )


##### RT: participant level

# just keep trials with correct Go and failed No-Go trials
gng_rt <- gng_data %>%
  mutate(trial_type = if_else((condition == 1 & trial_acc == 1), "correct_go",
                              if_else((condition == 0 & trial_acc == 0), "failed_nogo",
                                      "other")))

gng_rt_correct_go <- gng_rt %>%
  filter(trial_type == "correct_go")

gng_rt_failed_nogo <- gng_rt %>%
  filter(trial_type == "failed_nogo")

# This will apply the Van Selst & Jolicoeur-style trimming per participant and per condition
gng_rt_trimmed <- nonRecursive(
  data      = gng_rt_correct_go,
  pptVar    = "participant",
  condVar   = "trial_type",
  rtVar     = "trial_rt",
  accVar    = "trial_acc",
  minRT     = anticipatory_rt,
  digits    = 0,
  returnType = "raw"
)

