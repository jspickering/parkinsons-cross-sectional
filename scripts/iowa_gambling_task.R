####################################################################################################################
# Script for analysis of the Iowa Gambling Task according to the pre-registration on the Open Science Framework    #
# Link: https://osf.io/y8drq/files/frzpv                                                                           #
####################################################################################################################

##########
# SET-UP #
##########

# load all packages and install if necessary
requiredPackages = c('tidyverse', 'janitor')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

# to get the raincloud plots working again
source("https://raw.githubusercontent.com/RainCloudPlots/RainCloudPlots/master/tutorial_R/R_rainclouds.R")

# task info
expected_trials = 100

# empty exclusions table to fill in later
exclusions <- tibble(
  participant = character(),
  group = character(),
  reason = character()
)

################
# READ IN DATA #
################

# the raw files are one per participant, in one folder per group, in an awkward format
# a "deck order" key at the top mapping 'screen position' (L-R) to 'actual deck', then a trial table below
# the dummy data has the same awkward format
data_dir <- "./raw_data/iowa_gambling_task_dummy" # dummy data for development
# data_dir <- "Z:/Study 2_cross sectional/DATA/IGT" # file path to network drive

groups <- c("PWP", "ICD", "OC", "YC") # specify groups
group_dfs <- list() # create empty list to hold data for each group


# for every group
for (g in groups) {
  # find all csv files in that group's folder
  files <- list.files(
    path = file.path(data_dir, g),
    pattern = "\\.csv$",
    full.names = TRUE
  )

  # empty list to store data frames
  igt_group_data <- list()

  # Iterate over each .csv file (i.e. every participant)
  for (file_path in files) {
    # Extract the name for the final data file
    # (the filename minus folder and extension, e.g. ".../PWP/participant-021.csv" -> "participant-021")
    # not sure exactly what format the real data file names take
    file_name <- tools::file_path_sans_ext(basename(file_path))

    # NB: read.csv looks at the first 5 lines to decide how many columns the file has
    # the first lines are the deck key (2 columns) so it wraps the trial data (which should be 8 columns) into 4 rows of 2 columns
    # so we need to separate the key + trial data, then unwrap the trial data
    
    # Read the entire CSV file without headers
    # after this line raw_data is: row 1 title, rows 2-5 deck key, rows 6-9 wrapped column
    # headers, row 10 onwards the wrapped trials
    raw_data <- read.csv(file_path, header = FALSE, stringsAsFactors = FALSE)

    # Extract key information (i.e. which deck is in which position)
    # ASSUMPTION (to verify): "deck 1,2" means screen position 1 (left to right) held actual deck 2 (i.e. deck B),
    # i.e. first number = physical position, second number = actual deck where 1=A, 2=B, 3=C, 4=D
    key <- raw_data[2:5, ]
    colnames(key) <- c("deck", "deck_name")

    # Adjust the deck_name column to remove "deck " prefix
    key$deck <- gsub("^deck ", "", key$deck)

    # rename the decks from numbers to letters so its easier to distinguish deck position from deck type
    # A and B are the bad decks (net loss), C and D the good decks (net gain)
    key$deck_name <- recode(key$deck_name, `1` = "A", `2` = "B", `3` = "C", `4` = "D")

    # Combine the header rows into a single vector
    combined_headers <- c("count", "current", "previous", "deck", "win", "loss", "RT", "shock")

    # Extract the data rows starting from row 10
    # DOUBLE CHECK THIS as Daisy's goes from row 9
    # it looks like the wrapped column headers are rows 6-9, so trials start at row 10
    # the original version started at row 9, which shifted RT onto the wrong trial and left an empty row
    # (we're not using RT so it didn't matter)
    # a complete participant has 400 rows ie. 100 trials x 4 wrapped rows each
    data_rows <- raw_data[10:nrow(raw_data), ]

    # empty list to collect rows
    cleaned_data <- list()

    # Process data row by row

    for (i in seq(1, nrow(data_rows), by = 4)) {
      # Extract four rows at a time
      # the 4 rows of 2 hold: (count, current), (previous, deck), (win, loss), (RT, shock)
      rows <- data_rows[i:(i+3), ]

      # Flatten the four rows into a single vector of the 8 values, in reading order
      # (left to right, top to bottom), which matches the header order in combined_headers
      # changing start row above from 9 to 10 means data is in header order
      # so they are combined in reading order
      combined_row <- c(rows[1,1], rows[1,2], rows[2,1], rows[2,2], rows[3,1], rows[3,2], rows[4,1], rows[4,2])

      # Append the combined row to the cleaned_data list
      cleaned_data <- append(cleaned_data, list(combined_row))
    }

    # Convert the cleaned_data list to a dataframe (one row per trial)
    data <- as.data.frame(do.call(rbind, cleaned_data), stringsAsFactors = FALSE)

    # Assign combined headers to the data rows
    # sanity check - head(data) should show count running in order (1, 2, 3 etc)
    colnames(data) <- combined_headers

    # Convert appropriate columns to numeric (everything is text atm as it was read in as text)
    data$count <- as.numeric(data$count)
    data$current <- as.numeric(data$current)
    data$previous <- as.numeric(data$previous)
    data$deck <- as.numeric(data$deck)
    data$win <- as.numeric(data$win)
    data$loss <- as.numeric(data$loss)
    data$RT <- as.numeric(data$RT)

    # Merge the dataframes based on the common variable 'deck'
    # this looks up each trial's deck (the screen position clicked) in the key and adds
    # deck_name (the actual deck A-D) to every trial
    # NB: merge re-sorts the rows by deck; trial order is preserved in 'count'
    merged_data <- merge(data, key, by = "deck", all.x = TRUE)

    # Store merged data frame in the list, named after the participant's file
    igt_group_data[[file_name]] <- merged_data
  }

  # loop through every ppt in this group and add group and ppt ID number
  # lapply applies function to every item of a list
  # names function retrieves the names
  # a function is applied to the names where each df is selected by name (square brackets)
  # and then participant and group are mutated
  # participant = the digits from the filename, e.g. "participant-021" = "021"
  # group = the group folder currently being processed
  igt_group_data <- lapply(names(igt_group_data), function(name) {
    igt_group_data[[name]] %>%
      mutate(participant = str_extract(name, "\\d+"), group = g)
  })

  # combine using do.call - calls function to a list
  # (stacks this group's participants into one df, stored per group)
  group_dfs[[g]] <- do.call(rbind, igt_group_data)
}

# combine all group dfs into one big df where every row is one card choice by one participant
igt_raw <- bind_rows(group_dfs)


##### Tidy data
igt_data <- igt_raw %>%
  # only keep the columns we're interested in
  mutate(
    deck_type = if_else(deck_name %in% c("C", "D"), "advantageous", "disadvantageous")
  ) %>%
  select(participant,
         group,
         trial_num = count,
         deck_name,
         deck_type
  ) %>%
  arrange(participant,
          trial_num) %>%
  # convert data types for all columns to make sure they're correct
  mutate(
    across(c(participant,
             group,
             deck_type), as.character),
    trial_num = as.numeric(trial_num)
  ) %>%
  # remove YC participants (if there are any) as this was collected for MM's project
  filter(group != "YC") %>%
  # recode group labels
  mutate(group = if_else((group == "PWP" | group == "PwP"), "PwP",
                         if_else(group == "ICD", "PwP+ICB",
                                 if_else(group == "OC", "HC", "error"))))


##### Look for exclusions

# get moca exclusions (written out by demographics.R, so that needs to have been run first)
moca_exclusions <- read_csv("outputs/moca_exclusions.csv")
moca_excluded <- str_pad(moca_exclusions$participant_id, width = 3, pad = "0") # pad to match the IGT's 3 digit style ids

# remove moca exclusions
# they're already recorded in the exclusions table in demographics.R so aren't added again here
igt_data <- igt_data %>%
  filter(!participant %in% moca_excluded)

# exclude participants who completed less than 100% of expected trials (pre-reg 8.7)
exc_expected_trials <- igt_data %>%
  group_by(group, participant) %>%
  summarise(n_trials = n(),
            .groups = "drop") %>%
  filter(n_trials < expected_trials) %>%
  transmute(participant,
            group,
            reason = "did not complete 100% of trials")

exclusions <- bind_rows(exclusions,
                        exc_expected_trials)

# filter excluded participants from main data going forwards
igt_data <- igt_data %>%
  filter(!participant %in% exclusions$participant)


##### Scoring

# total score per participant (advantageous minus disadvantageous deck choices)
igt_wide <- igt_data %>%
  group_by(group, participant) %>%
  summarise(
    deck_a = sum(deck_name == "A"),
    deck_b = sum(deck_name == "B"),
    deck_c = sum(deck_name == "C"),
    deck_d = sum(deck_name == "D"),
    .groups = "drop"
  ) %>%
  mutate(
    score = (deck_c + deck_d) - (deck_a + deck_b),
    group = factor(group, levels = c("PwP", "PwP+ICB", "HC"))
  )

# score per participant per bin of 20 consecutive choices
igt_bins <- igt_data %>%
  mutate(bin = ceiling(trial_num / 20)) %>% # adds a column with bin number (1-5)
  group_by(group, participant, bin) %>%
  summarise(
    deck_a = sum(deck_name == "A"),
    deck_b = sum(deck_name == "B"),
    deck_c = sum(deck_name == "C"),
    deck_d = sum(deck_name == "D"),
    .groups = "drop"
  ) %>%
  mutate(
    score = (deck_c + deck_d) - (deck_a + deck_b),
    bin = factor(bin, levels = 1:5, labels = c("1-20", "21-40", "41-60", "61-80", "81-100")),
    group = factor(group, levels = c("PwP", "PwP+ICB", "HC"))
  )


##### Normality checks

normality_plots <- ggplot(igt_wide, aes(score)) +
  geom_histogram() +
  facet_wrap(~ group, scales = "free") +
  labs(title = "Histograms of IGT total score")
normality_plots

normality_summary <- igt_wide %>%
  group_by(group) %>%
  summarise(
    p_value = shapiro.test(score)$p.value,
    .groups = "drop"
  )

# same again for the bin scores
bins_normality_plots <- ggplot(igt_bins, aes(score)) +
  geom_histogram() +
  facet_grid(bin ~ group, scales = "free") +
  labs(title = "Histograms of IGT bin scores")
bins_normality_plots

bins_normality_summary <- igt_bins %>%
  group_by(group,
           bin) %>%
  summarise(
    p_value = shapiro.test(score)$p.value,
    .groups = "drop"
  )

# if normality is violated go straight to the non-parametric tests on the untransformed data
# can't log10 transform as difference scores could be 0 or negative


######################
# SUMMARY STATISTICS #
######################

igt_stats <- igt_wide %>%
  group_by(group) %>%
  summarise(
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE),
    .groups = "drop"
  )

# mean number of choices from each deck per group
igt_deck_stats <- igt_wide %>%
  group_by(group) %>%
  summarise(
    across(c(deck_a, deck_b, deck_c, deck_d), mean),
    .groups = "drop"
  )

# bin scores per group per bin, for the exploratory analysis
igt_bins_stats <- igt_bins %>%
  group_by(group,
           bin) %>%
  summarise(
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE),
    .groups = "drop"
  )


#########
# PLOTS #
#########

w = 4
h = 5

### Total score

p_igt_score <- ggplot(igt_wide, aes(x = group, y = score, fill = group, colour = group)) +
  geom_flat_violin(aes(fill = group), position = position_nudge(x = .3, y = 0), adjust = 1.2, trim = TRUE, alpha = .8, colour = "black", size = .5) +
  geom_point(aes(x = group, y = score, fill = group, colour = group), position = position_jitter(width = .1), alpha = .7, size = 1) +
  geom_boxplot(aes(x = group, y = score, fill = group), position = position_nudge(x = c(.22, .22), y = 0), outlier.shape = NA, alpha = .8, width = .1, colour = "black", size = .4) +
  ylab("IGT total score (advantageous - disadvantageous choices)") +
  xlab("") +
  cowplot::theme_cowplot() +
  viridis::scale_colour_viridis(discrete = TRUE, begin = .1, end = .5) +
  viridis::scale_fill_viridis(discrete = TRUE, begin = .1, end = .5) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.position = "none")
p_igt_score

#ggsave('figs/igt_score.png', width = w, height = h)


### Bin scores

# raincloud per group, faceted by bin
p_igt_bins <- ggplot(
  igt_bins,
  aes(x = group, y = score, fill = group, colour = group)) +
  geom_flat_violin(aes(fill = group), position = position_nudge(x = .3, y = 0), adjust = 1.2, trim = TRUE, alpha = .8, colour = "black", size = .5) +
  geom_point(aes(x = group, y = score, fill = group, colour = group), position = position_jitter(width = .1), alpha = .7, size = 1) +
  geom_boxplot(aes(x = group, y = score, fill = group), position = position_nudge(x = c(.22, .22), y = 0), outlier.shape = NA, alpha = .8, width = .1, colour = "black", size = .4) +
  facet_wrap(~ bin, nrow = 1) +
  ylab("IGT bin score (advantageous - disadvantageous choices)") +
  xlab("") +
  cowplot::theme_cowplot() +
  viridis::scale_colour_viridis(discrete = TRUE, begin = .1, end = .5) +
  viridis::scale_fill_viridis(discrete = TRUE, begin = .1, end = .5) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.position = "none")
p_igt_bins

#ggsave('figs/igt_bins.png', width = w * 2.5, height = h)


###########################
# INFERENTIAL STATISTICS  #
#  Confirmatory analysis  #
###########################

# subset data for planned pairwise comparisons
pwp_hc_data <- igt_wide %>%
  filter(group == "PwP" |
           group == "HC")

pwp_icd_data <- igt_wide %>%
  filter(group == "PwP" |
           group == "PwP+ICB")

### TOTAL SCORE

# variance tests
pwp_hc_score_variance <- var.test(score ~ group, data = pwp_hc_data) %>%
  broom::tidy()
pwp_icd_score_variance <- var.test(score ~ group, data = pwp_icd_data) %>%
  broom::tidy()

# one-way between-subjects ANOVA
igt_score_aov <- aov(score ~ group, data = igt_wide) %>%
  broom::tidy()

# planned independent t-tests
# change 'var.equal' depending on the variances tests above
pwp_hc_score_ttest <- t.test(score ~ group, data = pwp_hc_data, var.equal = TRUE) %>%
  broom::tidy()
pwp_icd_score_ttest <- t.test(score ~ group, data = pwp_icd_data, var.equal = TRUE) %>%
  broom::tidy()

# non-parametric equivalents, only needed if the normality checks above fail
# igt_score_kw <- kruskal.test(score ~ group, data = igt_wide) %>%
#   broom::tidy()

# pwp_hc_score_mwu <- wilcox.test(score ~ group, data = pwp_hc_data, alternative = "two.sided", conf.int = TRUE) %>%
#   broom::tidy()
# pwp_icd_score_mwu <- wilcox.test(score ~ group, data = pwp_icd_data, alternative = "two.sided", conf.int = TRUE) %>%
#   broom::tidy()


###########################
# INFERENTIAL STATISTICS  #
#   Exploratory analysis  #
###########################

### TOTAL SCORE PER 20-TRIAL BIN

# two-way mixed ANOVA, bin is within-subjects, group is between-subjects
igt_bins_anova <- aov(score ~ group * bin + Error(participant/bin), data = igt_bins) %>%
  broom::tidy()

# # post-hoc pairwise comparisons across bins, only to be interpreted if the ANOVA shows a
# # significant effect of bin; needs rows ordered consistently within each bin for the pairing
# igt_bins_posthoc <- igt_bins %>%
#   arrange(bin, group, participant) %>%
#   with(., pairwise.t.test(score, bin, paired = TRUE, p.adjust.method = "bonferroni")) %>%
#   broom::tidy()

# # subset data for planned pairwise comparisons, same group pairs as the confirmatory analysis
# pwp_hc_bins <- igt_bins %>%
#   filter(group == "PwP" |
#            group == "HC")
# 
# pwp_icd_bins <- igt_bins %>%
#   filter(group == "PwP" |
#            group == "PwP+ICB")
#
# # variance tests per bin
# pwp_hc_bins_variance <- pwp_hc_bins %>%
#   group_by(bin) %>%
#   group_modify(~ var.test(score ~ group, data = .x) %>%
#                  broom::tidy())
# pwp_icd_bins_variance <- pwp_icd_bins %>%
#   group_by(bin) %>%
#   group_modify(~ var.test(score ~ group, data = .x) %>%
#                  broom::tidy())
# 
# # planned independent t-tests, run within each bin, only to be interpreted if the ANOVA shows
# # a significant effect of group or a group x bin interaction
# # the groups aren't compared collapsed across bins as that's just the confirmatory total score test
# # change 'var.equal' depending on the variance tests above
# pwp_hc_bins_ttest <- pwp_hc_bins %>%
#   group_by(bin) %>%
#   group_modify(~ t.test(score ~ group, data = .x, var.equal = TRUE) %>%
#                  broom::tidy())
# pwp_icd_bins_ttest <- pwp_icd_bins %>%
#   group_by(bin) %>%
#   group_modify(~ t.test(score ~ group, data = .x, var.equal = TRUE) %>%
#                  broom::tidy())

# non-parametric equivalents, only needed if the normality checks above fail
# there's no single non-parametric version of a two-way mixed ANOVA, so the two effects are
# tested separately: Friedman for bin (within-subjects) and Kruskal-Wallis per bin for group
# NB: neither tests the group x bin interaction (we can figure this out if we need it)

# igt_bins_friedman <- igt_bins %>%
#   arrange(bin, participant) %>%
#   with(., friedman.test(score, bin, participant)) %>%
#   broom::tidy()

# igt_bins_kw <- igt_bins %>%
#   group_by(bin) %>%
#   group_modify(~ kruskal.test(score ~ group, data = .x) %>%
#                  broom::tidy())

# post-hoc pairwise comparisons across bins
# igt_bins_posthoc_np <- igt_bins %>%
#   arrange(bin, group, participant) %>%
#   with(., pairwise.wilcox.test(score, bin, paired = TRUE, p.adjust.method = "bonferroni")) %>%
#   broom::tidy()

# planned Mann-Whitney U comparisons, run within each bin
# pwp_hc_bins_mwu <- pwp_hc_bins %>%
#   group_by(bin) %>%
#   group_modify(~ wilcox.test(score ~ group, data = .x, alternative = "two.sided", conf.int = TRUE) %>%
#                  broom::tidy())
# pwp_icd_bins_mwu <- pwp_icd_bins %>%
#   group_by(bin) %>%
#   group_modify(~ wilcox.test(score ~ group, data = .x, alternative = "two.sided", conf.int = TRUE) %>%
#                  broom::tidy())
