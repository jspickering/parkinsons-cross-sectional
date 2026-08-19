########################################################################################################################
# Script for analysis of the Balloon Analogue Risk Task according to the pre-registration on the Open Science Framework #
# Link: https://osf.io/y8drq/files/frzpv                                                                                #
########################################################################################################################

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
expected_trials = 30 # participants must complete 100% of trials to be included

# empty exclusions table to fill in later
exclusions <- tibble(
  participant = character(),
  group = character(),
  reason = character()
)

################
# READ IN DATA #
################

data_dir <- "./raw_data/balloon_analogue_risk_task_dummy" # dummy data for development
# data_dir <- "Z:/Study 2_cross sectional/DATA/BART" # file path to network drive

groups <- c("PWP", "ICD", "OC", "YC") # specify groups (these may need renaming to match the network drive folder names when switching to the real data)
group_dfs <- list() # create empty list to hold data for each group

# for every group
for (g in groups) {
  # find all csv files in that group's folder
  files <- list.files(
    path = file.path(data_dir, g),
    pattern = "\\.csv$",
    full.names = TRUE
  )

  # read every participant's file in as character (so ids like "006" keep their leading zeros)
  group_dfs[[g]] <- map_df(files, ~read_csv(.x, col_types = cols(.default = "c"))) %>%
    clean_names() %>% # puts the PsychoPy headers in snake case (e.g. maxPumps -> max_pumps)
    mutate(group = g) # take group from the folder currently being processed as the in-file labels aren't reliable
}

# combine all group dfs into one big df where every row is one balloon by one participant
bart_raw <- bind_rows(group_dfs)


##### Tidy data
bart_data <- bart_raw %>%
  drop_na(image_file) %>% # remove the "press space to begin" row at the top and the "press return" metadata row at the bottom of each file
  # # clean up some researcher errors with naming the participants in the real data
  # # I'd written these in the original script, but can't run on dummy data, so check that everything is still ok here
  # mutate(participant = if_else(participant == "YC", "012",
  #                              if_else(participant == "0024", "024",
  #                                      if_else(participant == "oc", "043",
  #                                              if_else(is.na(participant), "020", # double check this because if this occurs twice we'll be calling multiple participants 020
  #                                                      participant))))) %>%
  # convert data types for all columns to make sure they're correct
  mutate(
    across(c(max_pumps,
             trials_this_n,
             n_pumps,
             earnings), as.numeric),
    participant = str_pad(participant, width = 3, pad = "0") # leading 0s needed to check against MoCA failures data
  ) %>%
  # remove YC participants (if there are any) as this was collected for MM's project
  filter(group != "YC") %>%
  # recode group labels
  mutate(group = if_else((group == "PWP" | group == "PwP"), "PwP",
                         if_else(group == "ICD", "PwP+ICB",
                                 if_else((group == "OC" | group == "HC"), "HC", "error")))) %>%
  # keep only the necessary columns
  select(group, participant, trials_this_n, max_pumps, n_pumps, earnings, popped)


##### Look for exclusions

# get moca exclusions (written out by demographics.R, so that needs to have been run first)
moca_exclusions <- read_csv("outputs/moca_exclusions.csv")
moca_excluded <- str_pad(moca_exclusions$participant_id, width = 3, pad = "0") # pad to match the BART's 3 digit style ids

# remove moca exclusions
bart_data <- bart_data %>%
  filter(!participant %in% moca_excluded)

# participants are only included if they complete 100% of available trials
exc_incomplete <- bart_data %>%
  group_by(group, participant) %>%
  summarise(n_trials = n(),
            .groups = "drop") %>%
  filter(n_trials != expected_trials) %>%
  transmute(participant,
            group,
            reason = paste0("completed fewer than 100% of trials (", n_trials, " of ", expected_trials, ")"))

# participants are excluded if they have no unexploded balloons at the end of the task
exc_no_unexploded <- bart_data %>%
  group_by(group, participant) %>%
  summarise(n_unexploded = sum(popped == "False"), # count the trials where the popped column contains the string "False"
            .groups = "drop") %>%
  filter(n_unexploded == 0) %>%
  transmute(participant,
            group,
            reason = "no unexploded balloons at the end of the task")

exclusions <- bind_rows(exclusions,
                        exc_incomplete,
                        exc_no_unexploded)

# filter excluded participants from main data going forwards
bart_data <- bart_data %>%
  filter(!participant %in% exclusions$participant)


##### Scoring

# calculate everything we need for analysis
bart_scores <- bart_data %>%
  group_by(group, participant) %>%
  summarise(
    # adjusted average pumps, i.e. mean pumps on unexploded balloons only
    adjusted_pumps = mean(n_pumps[popped == "False"]),
    # unexploded minus exploded balloons
    balloon_diff = sum(popped == "False") - sum(popped == "True"),
    # final points total
    final_points = sum(earnings),
    .groups = "drop"
  ) %>%
  mutate(group = factor(group, levels = c("PwP", "PwP+ICB", "HC")))

# long format so outlier removal and normality checks can run per measure
bart_long <- bart_scores %>%
  pivot_longer(
    cols = c(adjusted_pumps, balloon_diff, final_points),
    names_to = "measure",
    values_to = "value"
  )


##### Tukey outlier removal

# calculate outliers at the group level, per measure
bart_tukey <- bart_long %>%
  group_by(group,
           measure) %>%
  mutate(
    q1 = quantile(value, 0.25, na.rm = TRUE),
    q3 = quantile(value, 0.75, na.rm = TRUE),
    upper_bound = q3 + (3 * (q3 - q1)),
    lower_bound = q1 - (3 * (q3 - q1)),
    is_outlier = (value < lower_bound) | (value > upper_bound)
  ) %>%
  ungroup()

# identify outliers and add to exclusion table
exc_tukey <- bart_tukey %>%
  filter(is_outlier) %>%
  transmute(
    participant,
    group = as.character(group),
    reason = paste0(
      "value removed just for the ", measure, " measure following measure due to Tukey's outlier removal"
    )
  )

# put exclusions into the main exclusions table
exclusions <- bind_rows(exclusions,
                        exc_tukey)

# remove outliers from df
bart_long_outliers_removed <- bart_tukey %>%
  filter(!is_outlier) %>%
  select(-q1,
         -q3,
         -upper_bound,
         -lower_bound,
         -is_outlier)

# wide format: one row per participant, one column per measure
bart_wide <- bart_long_outliers_removed %>%
  pivot_wider(names_from = measure, values_from = value)


##### Normality checks

bart_normality_plots <- ggplot(bart_long_outliers_removed, aes(value)) +
  geom_histogram() +
  facet_grid(measure ~ group, scales = "free") +
  labs(title = "Histograms of BART measures")
bart_normality_plots

bart_normality_summary <- bart_long_outliers_removed %>%
  group_by(group,
           measure) %>%
  summarise(
    p_value = shapiro.test(value)$p.value,
    .groups = "drop"
  )

# log10 transform if any conditions fail normality checks above (p < .05)
# may not need this if everything is fine with the real data
# NB: balloon_diff can have negative values so can't be log10 transformed - if it fails the normality checks use the non-parametric tests on the untransformed data instead
bart_wide <- bart_wide %>%
  mutate(log10_adjusted_pumps = log10(adjusted_pumps),
         log10_final_points = log10(final_points))

# re-check normality after transforming and if still non-normal use non-parametric tests on untransformed data
bart_normality_summary_log10 <- bart_wide %>%
  pivot_longer(
    cols = c(log10_adjusted_pumps, log10_final_points),
    names_to = "measure",
    values_to = "value"
  ) %>%
  group_by(group,
           measure) %>%
  summarise(
    p_value = shapiro.test(value)$p.value,
    .groups = "drop"
  )


######################
# SUMMARY STATISTICS #
######################

bart_summary_stats <- bart_long_outliers_removed %>%
  group_by(group,
           measure) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    .groups = "drop"
  )


#########
# PLOTS #
#########

w = 4
h = 5

### Adjusted average pumps

p_bart_pumps <- ggplot(drop_na(bart_wide, adjusted_pumps), aes(x = group, y = adjusted_pumps, fill = group, colour = group)) +
  geom_flat_violin(aes(fill = group), position = position_nudge(x = .3, y = 0), adjust = 1.2, trim = TRUE, alpha = .8, colour = "black", size = .5) +
  geom_point(aes(x = group, y = adjusted_pumps, fill = group, colour = group), position = position_jitter(width = .1), alpha = .7, size = 1) +
  geom_boxplot(aes(x = group, y = adjusted_pumps, fill = group), position = position_nudge(x = c(.22, .22), y = 0), outlier.shape = NA, alpha = .8, width = .1, colour = "black", size = .4) +
  ylab("Adjusted average pumps\n(mean pumps on unexploded balloons)") +
  xlab("") +
  cowplot::theme_cowplot() +
  viridis::scale_colour_viridis(discrete = TRUE, begin = .1, end = .5) +
  viridis::scale_fill_viridis(discrete = TRUE, begin = .1, end = .5) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.position = "none")
p_bart_pumps

#ggsave('figs/bart_pumps.png', width = w, height = h)


### Balloon difference (unexploded minus exploded)

p_bart_diff <- ggplot(drop_na(bart_wide, balloon_diff), aes(x = group, y = balloon_diff, fill = group, colour = group)) +
  geom_flat_violin(aes(fill = group), position = position_nudge(x = .3, y = 0), adjust = 1.2, trim = TRUE, alpha = .8, colour = "black", size = .5) +
  geom_point(aes(x = group, y = balloon_diff, fill = group, colour = group), position = position_jitter(width = .1), alpha = .7, size = 1) +
  geom_boxplot(aes(x = group, y = balloon_diff, fill = group), position = position_nudge(x = c(.22, .22), y = 0), outlier.shape = NA, alpha = .8, width = .1, colour = "black", size = .4) +
  ylab("Unexploded minus exploded balloons\n(higher = more risk averse)") +
  xlab("") +
  cowplot::theme_cowplot() +
  viridis::scale_colour_viridis(discrete = TRUE, begin = .1, end = .5) +
  viridis::scale_fill_viridis(discrete = TRUE, begin = .1, end = .5) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.position = "none")
p_bart_diff

#ggsave('figs/bart_diff.png', width = w, height = h)


### Final points total

p_bart_points <- ggplot(drop_na(bart_wide, final_points), aes(x = group, y = final_points, fill = group, colour = group)) +
  geom_flat_violin(aes(fill = group), position = position_nudge(x = .3, y = 0), adjust = 1.2, trim = TRUE, alpha = .8, colour = "black", size = .5) +
  geom_point(aes(x = group, y = final_points, fill = group, colour = group), position = position_jitter(width = .1), alpha = .7, size = 1) +
  geom_boxplot(aes(x = group, y = final_points, fill = group), position = position_nudge(x = c(.22, .22), y = 0), outlier.shape = NA, alpha = .8, width = .1, colour = "black", size = .4) +
  ylab("Final points total") +
  xlab("") +
  cowplot::theme_cowplot() +
  viridis::scale_colour_viridis(discrete = TRUE, begin = .1, end = .5) +
  viridis::scale_fill_viridis(discrete = TRUE, begin = .1, end = .5) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.position = "none")
p_bart_points

#ggsave('figs/bart_points.png', width = w, height = h)


###########################
# INFERENTIAL STATISTICS  #
#  Confirmatory analysis  #
###########################

# subset data for planned pairwise comparisons (all three measures are columns of bart_wide, so these are used for every analysis below)
pwp_hc_data <- bart_wide %>%
  filter(group == "PwP" |
           group == "HC")

pwp_icd_data <- bart_wide %>%
  filter(group == "PwP" |
           group == "PwP+ICB")

### ADJUSTED AVERAGE PUMPS

# variance tests
pwp_hc_pumps_variance <- var.test(adjusted_pumps ~ group, data = pwp_hc_data) %>%
  broom::tidy()
pwp_icd_pumps_variance <- var.test(adjusted_pumps ~ group, data = pwp_icd_data) %>%
  broom::tidy()

# one-way between-subjects ANOVA
bart_pumps_aov <- aov(adjusted_pumps ~ group, data = bart_wide) %>%
  broom::tidy()

# planned independent t-tests
# change 'var.equal' depending on the variances tests above
pwp_hc_pumps_ttest <- t.test(adjusted_pumps ~ group, data = pwp_hc_data, var.equal = TRUE) %>%
  broom::tidy()
pwp_icd_pumps_ttest <- t.test(adjusted_pumps ~ group, data = pwp_icd_data, var.equal = TRUE) %>%
  broom::tidy()

# non-parametric equivalents, only needed if the normality checks above fail
# bart_pumps_kw <- kruskal.test(adjusted_pumps ~ group, data = bart_wide) %>%
#   broom::tidy()

# pwp_hc_pumps_mwu <- wilcox.test(adjusted_pumps ~ group, data = pwp_hc_data, alternative = "two.sided", conf.int = TRUE) %>%
#   broom::tidy()
# pwp_icd_pumps_mwu <- wilcox.test(adjusted_pumps ~ group, data = pwp_icd_data, alternative = "two.sided", conf.int = TRUE) %>%
#   broom::tidy()


###########################
# INFERENTIAL STATISTICS  #
#   Exploratory analysis  #
###########################

### BALLOON DIFFERENCE (unexploded minus exploded)

# variance tests
pwp_hc_diff_variance <- var.test(balloon_diff ~ group, data = pwp_hc_data) %>%
  broom::tidy()
pwp_icd_diff_variance <- var.test(balloon_diff ~ group, data = pwp_icd_data) %>%
  broom::tidy()

# one-way between-subjects ANOVA
bart_diff_aov <- aov(balloon_diff ~ group, data = bart_wide) %>%
  broom::tidy()

# planned independent t-tests
# change 'var.equal' depending on the variances tests above
pwp_hc_diff_ttest <- t.test(balloon_diff ~ group, data = pwp_hc_data, var.equal = TRUE) %>%
  broom::tidy()
pwp_icd_diff_ttest <- t.test(balloon_diff ~ group, data = pwp_icd_data, var.equal = TRUE) %>%
  broom::tidy()

# non-parametric equivalents, only needed if the normality checks above fail
# NB: balloon_diff can't be log10 transformed (negative values) so go straight to these if non-normal - there isn't any transformed data
# bart_diff_kw <- kruskal.test(balloon_diff ~ group, data = bart_wide) %>%
#   broom::tidy()

# pwp_hc_diff_mwu <- wilcox.test(balloon_diff ~ group, data = pwp_hc_data, alternative = "two.sided", conf.int = TRUE) %>%
#   broom::tidy()
# pwp_icd_diff_mwu <- wilcox.test(balloon_diff ~ group, data = pwp_icd_data, alternative = "two.sided", conf.int = TRUE) %>%
#   broom::tidy()


### FINAL POINTS TOTAL

# variance tests
pwp_hc_points_variance <- var.test(final_points ~ group, data = pwp_hc_data) %>%
  broom::tidy()
pwp_icd_points_variance <- var.test(final_points ~ group, data = pwp_icd_data) %>%
  broom::tidy()

# one-way between-subjects ANOVA
bart_points_aov <- aov(final_points ~ group, data = bart_wide) %>%
  broom::tidy()

# planned independent t-tests
# change 'var.equal' depending on the variances tests above
pwp_hc_points_ttest <- t.test(final_points ~ group, data = pwp_hc_data, var.equal = TRUE) %>%
  broom::tidy()
pwp_icd_points_ttest <- t.test(final_points ~ group, data = pwp_icd_data, var.equal = TRUE) %>%
  broom::tidy()

# non-parametric equivalents, only needed if the normality checks above fail
# bart_points_kw <- kruskal.test(final_points ~ group, data = bart_wide) %>%
#   broom::tidy()

# pwp_hc_points_mwu <- wilcox.test(final_points ~ group, data = pwp_hc_data, alternative = "two.sided", conf.int = TRUE) %>%
#   broom::tidy()
# pwp_icd_points_mwu <- wilcox.test(final_points ~ group, data = pwp_icd_data, alternative = "two.sided", conf.int = TRUE) %>%
#   broom::tidy()
