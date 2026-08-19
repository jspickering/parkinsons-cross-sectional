###################################################################################################################
# Script for analysis of the Eriksen Flanker task according to the pre-registration on the Open Science Framework #
# Link: https://osf.io/y8drq/files/frzpv                                                                          #
###################################################################################################################

##########
# SET-UP #
##########

# load all packages and install if necessary
requiredPackages = c('tidyverse', 'janitor', 'trimr')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

# to get the raincloud plots working again
source("https://raw.githubusercontent.com/RainCloudPlots/RainCloudPlots/master/tutorial_R/R_rainclouds.R")

# task info
expected_trials = 440
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
# data_dir <- "Z:/Study 2_cross sectional/DATA/Flanker" # file path to network drive
# groups <- c("PWP", "ICD", "OC", "YC") # specify groups
# group_dfs <- list() # create empty list to hold data for each group
#
# for (g in groups) {
#   # find all csv files in that group's folder
#   files <- list.files(
#     path = file.path(data_dir, g),
#     pattern = "\\.csv$",
#     full.names = TRUE
#   )
#   # read all the files in and combine into one df
#   group_data <- map_df(files, ~ read_csv(.x, col_types = cols(.default = "c")))
#   # clean column names and add a 'group' column
#   group_data <- group_data %>%
#     clean_names() %>%
#     mutate(group = g)
#   # store the group's df into a list
#   group_dfs[[g]] <- group_data
# }
#
# # combine all group dfs into one big df
# flanker_raw <- bind_rows(group_dfs)


# for now have to use dummy data
flanker_raw <- read_csv("./raw_data/flanker_dummy_data.csv") %>%
  clean_names()

##### Tidy data
flanker_data <- flanker_raw %>%
  # only keep the columns we're interested in
  select(participant,
         group,
         flanker_dir,
         target_dir,
         resp,
         rt
  ) %>%
  # convert data types for all columns to make sure they're correct
  mutate(
    across(c(participant,
             group,
             flanker_dir,
             target_dir,
             resp), as.character),
    rt = as.numeric(rt)
  ) %>%
  # remove YC participants as this was collected for MM's project
  filter(group != "YC") %>%
  # recode group labels
  mutate(group = if_else((group == "PWP" | group == "PwP"), "PwP",
                         if_else(group == "ICD", "PwP+ICB",
                                 if_else(group == "OC", "HC", "error")))) %>%
  # identify trial type and accuracy (includes missed trials)
  mutate(
    congruency = if_else(flanker_dir == target_dir, "congruent",
                         "incongruent"),
    accuracy = if_else(resp == target_dir, 1,
                       0)
  )


##### Look for exclusions

# get moca exclusions (written out by demographics.R, so that needs to have been run first)
moca_exclusions <- read_csv("outputs/moca_exclusions.csv")
moca_excluded <- str_pad(moca_exclusions$participant_id, width = 3, pad = "0") # pad to match the flanker's 3 digit style ids

# remove moca exclusions
# they're already recorded in the exclusions table in demographics.R so aren't added again here
flanker_data <- flanker_data %>%
  filter(!participant %in% moca_excluded)

# exclude participants who completed less than 80% of expected trials
exc_expected_trials <- flanker_data %>%
  group_by(group, participant) %>%
  summarise(n_trials = n(),
            .groups = "drop") %>%
  filter(n_trials < needed_trials) %>%
  transmute(participant,
            group,
            reason = "not enough trials")

exclusions <- bind_rows(exclusions,
                        exc_expected_trials)

# filter excluded participants from main data going forwards
flanker_data <- flanker_data %>%
  filter(!participant %in% exclusions$participant)


##### Pre-processing: individual level

# misses have no valid RT so are excluded before passing to trimr
flanker_rt_for_trimming <- flanker_data %>%
  filter(resp != "miss")

flanker_rt_trimmed <- nonRecursive(
  data = flanker_rt_for_trimming,
  pptVar = "participant",
  condVar = "congruency",
  rtVar = "rt",
  accVar = "accuracy",
  minRT = anticipatory_rt,
  digits = 0,
  returnType = "raw"
)


##### Pre-processing: group level

# mean correct RT per participant per congruency condition
flanker_summary_rts <- flanker_rt_trimmed %>%
  group_by(group,
           participant,
           congruency) %>%
  summarise(
    rt_mean = mean(rt, na.rm = TRUE),
    rt_sd = sd(rt, na.rm = TRUE),
    .groups = "drop"
  )

# Tukey outlier removal
# calculate outliers
flanker_tukey <- flanker_summary_rts %>%
  group_by(group,
           congruency) %>%
  mutate(q1 = quantile(rt_mean, 0.25, na.rm = TRUE),
         q3 = quantile(rt_mean, 0.75, na.rm = TRUE),
         upper_bound = q3 + (3 * (q3 - q1)),
         lower_bound = q1 - (3 * (q3 - q1)),
         is_outlier = rt_mean < lower_bound | rt_mean > upper_bound
         ) %>%
  ungroup()


exc_tukey <- flanker_tukey %>%
  filter(is_outlier) %>%
  transmute(
    participant,
    group,
    reason = paste0("mean RT outlier removed for condition: ", congruency,
                    " (Tukey 3×IQR, value: ", round(rt_mean), "ms)")
  )

exclusions <- bind_rows(exclusions,
                        exc_tukey)

flanker_summary_rts_outliers_removed <- flanker_tukey %>%
  filter(!is_outlier) %>%
  select(-q1,
         -q3,
         -upper_bound,
         -lower_bound,
         -is_outlier)


##### Wide and long formats

# wide: one row per participant, congruent and incongruent RT in separate columns,
# plus the interference effect (incongruent minus congruent mean correct RT)
# give this a one-off name so it doesn't try to join to itself later when we do the log10 transformations
flanker_wide_raw <- flanker_summary_rts_outliers_removed %>%
  select(group, participant, congruency, rt_mean) %>%
  pivot_wider(
    names_from = congruency,
    values_from = rt_mean
  ) %>%
  mutate(
    interference_effect = incongruent - congruent,
    group = factor(group, levels = c("PwP", "PwP+ICB", "HC"))
  ) %>%
  filter(!is.na(interference_effect)) # remove participants missing data for one condition

# long: one row per participant per measure, for normality checks and plotting
flanker_long <- flanker_wide_raw %>%
  pivot_longer(
    cols = c(congruent, incongruent, interference_effect),
    names_to = "measure",
    values_to = "value"
  )


##### Normality checks

normality_plots <- ggplot(flanker_long, aes(value)) +
  geom_histogram() +
  facet_grid(measure ~ group, scales = "free") +
  labs(title = "Histograms of Flanker measures")
normality_plots

normality_summary <- flanker_long %>%
  group_by(group, measure) %>%
  summarise(
    p_value = shapiro.test(value)$p.value,
    .groups = "drop"
  )

# log10 transform if any measures fail normality (p < .05)
flanker_long <- flanker_long %>%
  mutate(value_log10 = log10(value))

# re-check normality after transform; if still significant use non-parametric on untransformed data
normality_summary_log10 <- flanker_long %>%
  group_by(group, measure) %>%
  summarise(
    p_value = shapiro.test(value_log10)$p.value,
    .groups = "drop"
  )


# redo the wide version with log10 transformed data included
# this isn't needed if the assumption of normality was never violated and the data was never transformed
flanker_wide <- flanker_long %>%
  select(group,
         participant,
         measure,
         value_log10) %>%
  pivot_wider(names_from = measure,
              values_from = value_log10,
              names_prefix = "log10_") %>%
  full_join(flanker_wide_raw,
            by = c("group", "participant"))


######################
# SUMMARY STATISTICS #
######################

flanker_stats <- flanker_long %>%
  group_by(group, measure) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    .groups = "drop"
  )

# computed from the tidied data (not RT-trimmed) so misses are included as errors
flanker_acc_stats <- flanker_data %>%
  group_by(group, congruency, participant) %>%
  summarise(prop_correct = mean(accuracy), .groups = "drop") %>%
  group_by(group, congruency) %>%
  summarise(
    mean_prop_acc = mean(prop_correct, na.rm = TRUE),
    sd = sd(prop_correct, na.rm = TRUE),
    .groups = "drop"
  )


#########
# PLOTS #
#########

w = 4
h = 5

### Interference effect

p_interference <- ggplot(flanker_wide, aes(x = group, y = interference_effect, fill = group, colour = group)) +
  geom_flat_violin(aes(fill = group), position = position_nudge(x = .3, y = 0), adjust = 1.2, trim = TRUE, alpha = .8, colour = "black", size = .5) +
  geom_point(aes(x = group, y = interference_effect, fill = group, colour = group), position = position_jitter(width = .1), alpha = .7, size = 1) +
  geom_boxplot(aes(x = group, y = interference_effect, fill = group), position = position_nudge(x = c(.22, .22), y = 0), outlier.shape = NA, alpha = .8, width = .1, colour = "black", size = .4) +
  ylab("Interference effect (ms)") +
  xlab("") +
  cowplot::theme_cowplot() +
  viridis::scale_colour_viridis(discrete = TRUE, begin = .1, end = .5) +
  viridis::scale_fill_viridis(discrete = TRUE, begin = .1, end = .5) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.position = "none")
p_interference

#ggsave('figs/flanker_interference.png', width = w, height = h)


###########################
# INFERENTIAL STATISTICS  #
#  Confirmatory analysis  #
###########################

# subset data for planned pairwise comparisons
pwp_hc_data <- flanker_wide %>%
  filter(group == "PwP" |
           group == "HC")

pwp_icd_data <- flanker_wide %>%
  filter(group == "PwP" |
           group == "PwP+ICB")

### INTERFERENCE EFFECT

# variance tests
pwp_hc_interference_variance <- var.test(interference_effect ~ group, data = pwp_hc_data) %>%
  broom::tidy()
pwp_icd_interference_variance <- var.test(interference_effect ~ group, data = pwp_icd_data) %>%
  broom::tidy()

# one-way between-subjects ANOVA
interference_aov <- aov(interference_effect ~ group, data = flanker_wide) %>%
  broom::tidy()

# planned independent t-tests
# change 'var.equal' depending on the variances tests above
pwp_hc_interference_ttest <- t.test(interference_effect ~ group, data = pwp_hc_data, var.equal = TRUE) %>%
  broom::tidy()
pwp_icd_interference_ttest <- t.test(interference_effect ~ group, data = pwp_icd_data, var.equal = TRUE) %>%
  broom::tidy()


###########################
# INFERENTIAL STATISTICS  #
#   Exploratory analysis  #
###########################

### PROPORTION OF CORRECT INCONGRUENT TRIALS

# computed from the tidied data (not RT-trimmed) so misses are included as errors
flanker_acc <- flanker_data %>%
  filter(congruency == "incongruent") %>%
  group_by(group, participant) %>%
  summarise(
    prop_correct_incongruent = mean(accuracy),
    .groups = "drop"
  ) %>%
  mutate(group = factor(group, levels = c("PwP", "PwP+ICB", "HC")))

# normality
acc_normality_summary <- flanker_acc %>%
  group_by(group) %>%
  summarise(
    p_value = shapiro.test(prop_correct_incongruent)$p.value,
    .groups = "drop"
  )

# plot
p_acc <- ggplot(flanker_acc, aes(x = group, y = prop_correct_incongruent, fill = group, colour = group)) +
  geom_flat_violin(aes(fill = group), position = position_nudge(x = .3, y = 0), adjust = 1.2, trim = TRUE, alpha = .8, colour = "black", size = .5) +
  geom_point(aes(x = group, y = prop_correct_incongruent, fill = group, colour = group), position = position_jitter(width = .1), alpha = .7, size = 1) +
  geom_boxplot(aes(x = group, y = prop_correct_incongruent, fill = group), position = position_nudge(x = c(.22, .22), y = 0), outlier.shape = NA, alpha = .8, width = .1, colour = "black", size = .4) +
  ylab("Proportion correct (incongruent trials)") +
  xlab("") +
  cowplot::theme_cowplot() +
  viridis::scale_colour_viridis(discrete = TRUE, begin = .1, end = .5) +
  viridis::scale_fill_viridis(discrete = TRUE, begin = .1, end = .5) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.position = "none")
p_acc

#ggsave('figs/flanker_acc.png', width = w, height = h)

# subset for planned comparisons
pwp_hc_acc <- flanker_acc %>%
  filter(group == "PwP" | group == "HC")
pwp_icd_acc <- flanker_acc %>% 
  filter(group == "PwP" | group == "PwP+ICB")

# kruskal-wallis
prop_incongruent_kw <- kruskal.test(prop_correct_incongruent ~ group, data = flanker_acc) %>%
  broom::tidy()

# planned Mann-Whitney U comparisons
pwp_hc_prop_mwu <- wilcox.test(prop_correct_incongruent ~ group, data = pwp_hc_acc, alternative = "two.sided", conf.int = TRUE) %>%
  broom::tidy()
pwp_icd_prop_mwu <- wilcox.test(prop_correct_incongruent ~ group, data = pwp_icd_acc, alternative = "two.sided", conf.int = TRUE) %>%
  broom::tidy()


### MEAN RT BY CONGRUENCY AND GROUP (two-way mixed ANOVA)
# condition (congruent vs incongruent) is within-subjects; group is between-subjects

# normality per group per condition
twoway_normality_summary <- flanker_summary_rts_outliers_removed %>%
  group_by(group,
           congruency) %>%
  summarise(
    p_value = shapiro.test(rt_mean)$p.value,
    .groups = "drop"
  )

# log10 transform if any conditions fail normality checks above (p < .05)
# may not need this if everything is fine with the real data
flanker_summary_rts_outliers_removed <- flanker_summary_rts_outliers_removed %>%
  mutate(rt_mean_log10 = log10(rt_mean))

# re-check normality after transform; if still significant use non-parametric on untransformed data
twoway_normality_summary_log10 <- flanker_summary_rts_outliers_removed %>%
  group_by(group,
           congruency) %>%
  summarise(
    p_value = shapiro.test(rt_mean_log10)$p.value,
    .groups = "drop"
  )

# plots: raincloud per condition, faceted by congruency
flanker_rt_plot_data <- flanker_summary_rts_outliers_removed %>%
  mutate(group = factor(group, levels = c("PwP", "PwP+ICB", "HC")))

p_rt_by_congruency <- ggplot(
  flanker_rt_plot_data,
  aes(x = group, y = rt_mean, fill = group, colour = group)) +
  geom_flat_violin(aes(fill = group), position = position_nudge(x = .3, y = 0), adjust = 1.2, trim = TRUE, alpha = .8, colour = "black", size = .5) +
  geom_point(aes(x = group, y = rt_mean, fill = group, colour = group), position = position_jitter(width = .1), alpha = .7, size = 1) +
  geom_boxplot(aes(x = group, y = rt_mean, fill = group), position = position_nudge(x = c(.22, .22), y = 0), outlier.shape = NA, alpha = .8, width = .1, colour = "black", size = .4) +
  facet_wrap(~ congruency) +
  ylab("Mean RT (ms)") +
  xlab("") +
  cowplot::theme_cowplot() +
  viridis::scale_colour_viridis(discrete = TRUE, begin = .1, end = .5) +
  viridis::scale_fill_viridis(discrete = TRUE, begin = .1, end = .5) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.position = "none")
p_rt_by_congruency

#ggsave('figs/flanker_rt_by_congruency.png', width = w * 2, height = h)

# two-way mixed ANOVA, congruency is within-subjects, group is between-subjects
# filter to only include participants with data for both conditions (tukey removal can drop one condition)
# otherwise there's an error
flanker_twoway_anova <- flanker_summary_rts_outliers_removed %>%
  filter(participant %in% flanker_wide$participant) %>% # we already dropped people without an interference effect here so naturally includes only those with data for both conditions
  aov(rt_mean ~ group * congruency + Error(participant/congruency), data = .) %>%
  broom::tidy()
