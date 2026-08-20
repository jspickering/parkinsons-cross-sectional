############################################################################################################
# Pre-processing script for the Kirby Delay Discounting task (27-item Monetary Choice Questionnaire).      #
# Requires manual work before it can be used in the kirby_dd.R script                                      #
############################################################################################################

##########
# SET-UP #
##########

# load all packages and install if necessary
requiredPackages = c('tidyverse', 'readxl')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

################
# READ IN DATA #
################

data_dir <- "Z:/Study 2_cross sectional/DATA/Questionnaires" # file path to network drive

# find all xlsx files (one per participant)
files <- list.files(
  path = data_dir,
  pattern = "\\.xlsx$",
  full.names = TRUE
)
files <- files[!startsWith(basename(files), "TEMPLATE")] # removes the template file from the list

# read the 'Kirby' worksheet only from a participant's excel file
read_kirby <- function(file) {
  if (!"Kirby" %in% excel_sheets(file)) {
    warning(paste0("No 'Kirby' worksheet found in ", basename(file), " - skipping"), call. = FALSE) # give a warning if there isn't a Kirby sheet
    return(NULL)
  }
  read_excel(file, sheet = "Kirby", col_types = "text") %>% # read everything as text so participant ids keep any leading zeros
    select(SubjID, starts_with("MCQ")) # grab ID and all 27 MCQ response columns
}

# read all the files in and combine into one df (one row per participant)
kirby_raw <- map_df(files, read_kirby)

#####################
# RECODE & TRANSPOSE #
#####################

kirby_transposed <- kirby_raw %>%
  mutate(across(starts_with("MCQ"), ~ if_else(.x == "1", 0, if_else(.x == "2", 1, NA_real_)))) %>% # 1 (choice A) -> 0, 2 (choice B) -> 1; anything else becomes NA
  pivot_longer(starts_with("MCQ"), names_to = "question", values_to = "response") %>% # long format keeps MCQ1-MCQ27 in their original order
  pivot_wider(names_from = SubjID, values_from = response) # one column per participant, one row per question

########
# SAVE #
########

write_csv(kirby_transposed, "outputs/kirby_preprocessed.csv")


###### Next steps
# Copy the data from the csv into the scorer file from Kaplan et al. (2016) which can be found in Z:\Study 2_cross sectional\DATA\Kirby Delay Discounting
# Next, the scored data needs to be copied out of the scorer and into a new csv called 'kirby_scored.csv'
# Only then can this script be run
