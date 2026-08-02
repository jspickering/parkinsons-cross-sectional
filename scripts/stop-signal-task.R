###############################################################################################################
# Script for analysis of the Stop Signal task according to the pre-registration on the Open Science Framework #
# Link:                                                                                                       #
# Authors: Jade Pickering & Marta Majewska, 2020-2026                                                              #
###############################################################################################################

##########
# SET-UP #
##########

# load all packages and install if necessary
requiredPackages = c('tidyverse', 'janitor', 'trimr')
#requiredPackages = c('broom','janitor', 'gdata', 'cowplot', 'viridis', 'tidyverse')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

# to get the raincloud plots working again
source("https://raw.githubusercontent.com/RainCloudPlots/RainCloudPlots/master/tutorial_R/R_rainclouds.R")

# task info
expected_trials = 384 # 25%/75% stop/go trials

# exclusion thresholds
trial_threshold = 0.8
needed_trials = expected_trials * trial_threshold
anticipatory_rt = 150
stop_acc_lower = 0.25   # exclude if stop accuracy < 25% (not genuinely trying to stop)
stop_acc_upper = 0.75   # exclude if stop accuracy > 75% (waiting for the stop signal)
go_response_lower = 0.60  # exclude if responding on fewer than 60% of go trials
go_choice_error_upper = 0.10  # exclude if > 10% errors (wrong key) on go trials
ssrt_min = 50           # exclude if SSRT < 50ms or negative (race model violation)

# empty exclusions table to fill in later
exclusions <- tibble(
  participant = character(),
  group = character(),
  reason = character()
)

################
# READ IN DATA #
################

# ##### Access data from network drive
# data_dir <- "Z:/Study 2_cross sectional/DATA/GNG/data" # file path to network drive
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
# stop_signal_raw <- bind_rows(group_dfs) #%>%
# # # testing which participant didn't have their ID in the participant  column - it was 072
# # select(participant, group) %>%
# # distinct(participant, group) %>%
# # arrange(participant)


# for now have to use dummy data
sst_raw <- read_csv("./raw_data/stop_signal_task_dummy_data.csv") %>%
  clean_names() %>%
  mutate(across(everything(), as.character))


##### Tidy data
sst_data <- sst_raw %>%
  # only keep the columns we're interested in
  select(participant,
         group,
         condition, # go or stop
         trial_type, # 1 = leftGo, 2 = rightGo, 3 = leftStop, 4 = rightStop
         key_pressed, # "z" (right hand), "m" (left hand), 0 (no response)
         trial_rt, # in ms
         trial_acc, # "correct", "wrong arrow", "missed", "failed stop", "successful stop"
         ssd # stop signal delay in ms
  ) %>%
  # convert data types for all columns to make sure they're correct
  mutate(
    across(c(participant,
             group,
             condition,
             trial_acc,
             key_pressed
    ), as.character),
    across(c(trial_rt,
             ssd
    ), as.numeric)
  ) %>%
  # remove YC participants as this was collected for MM's project
  filter(group != "YC") %>%
  # recode group labels to match paper writing style
  mutate(group = if_else(group == "PWP", "PwP",
                         if_else(group == "ICD", "PwP+ICB",
                                 if_else(group == "OC", "HC", "error"))))

##### Look for exclusions

# get the data we need to apply the exclusions
sst_participant_summary_for_exclusions <- sst_data %>%
  group_by(group, participant) %>%
  summarise(
    # completed trials
    n_trials = n(),
    # stop accuracy = proportion of stop trials on which participant successfully stopped
    stop_acc = sum(trial_acc == "successful stop" & condition == "stop") / sum(condition == "stop"),
    # go response rate = proportion of go trials on which participant made ANY response (correct or error)
    go_response_rate = sum(trial_acc != "missed" & condition == "go") / sum(condition == "go"),
    # choice error rate = proportion of go trials on which participant pressed the WRONG key
    go_choice_error_rate = sum(trial_acc == "wrong arrow" & condition == "go") / sum(condition == "go"),
    .groups = "drop"
  )

# Exclude participants who completed less than 80% of expected trials
exc_expected_trials <- sst_participant_summary_for_exclusions %>%
  filter(n_trials < needed_trials) %>%
  transmute(participant,
            group,
            reason = "not enough trials")

exclusions <- bind_rows(exclusions,
                        exc_expected_trials)

# Exclude participants who got a stop accuracy outside the 25-75% range
# < 25% suggests not trying to stop; > 75% suggests waiting for the stop signal
exc_stop_acc <- sst_participant_summary_for_exclusions %>%
  filter(stop_acc < stop_acc_lower | stop_acc > stop_acc_upper) %>%
  transmute(participant,
            group,
            reason = paste0("stop accuracy outside 25-75% range (", round(stop_acc * 100, 1), "%)"))

exclusions <- bind_rows(exclusions,
                        exc_stop_acc)

# Exclude participants who responded on less than 60% of go trials
exc_go_response <- sst_participant_summary_for_exclusions %>%
  filter(go_response_rate < go_response_lower) %>%
  transmute(participant,
            group,
            reason = paste0("responded on fewer than 60% of go trials (", round(go_response_rate * 100, 1), "%)"))

exclusions <- bind_rows(exclusions,
                        exc_go_response)

# Exclude participants who responded on more than 10% choice errors on go trials (wrong key)
exc_choice_errors <- sst_participant_summary_for_exclusions %>%
  filter(go_choice_error_rate > go_choice_error_upper) %>%
  transmute(participant,
            group,
            reason = paste0("greater than 10% choice errors on go trials (", round(go_choice_error_rate * 100, 1), "%)"))

exclusions <- bind_rows(exclusions,
                        exc_choice_errors)

##### Calculate SSRT

ssrt_data <- sst_data %>%
  group_by(group, participant) %>%
  summarise(
    # proportion of stop trials on which participant failed to stop
    prop_failed_stops = sum(condition == "stop" & trial_acc == "failed stop") / sum(condition == "stop"),
    # mean SSD across all stop trials
    mean_ssd = mean(ssd[condition == "stop"], na.rm = TRUE),
    # nth percentile of the go RT distribution
    # for omitted go trials, replace NA with the participant's max RT (Verbruggen et al., 2019)
    nth_percentile_go_rt = quantile(
      x = if_else(condition == "go" & is.na(trial_rt),
                      max(trial_rt, na.rm = TRUE),   # replace omissions with max RT
                      trial_rt),
      probs = prop_failed_stops,                     # nth percentile where n = proportion of failed stops
      na.rm = TRUE                                   # na.rm still needed to handle stop trial NAs in the vector
    ),
    .groups = "drop"
  ) %>%
  mutate(ssrt = nth_percentile_go_rt - mean_ssd)

# Exclude participants who responded on more than 10% choice errors on go trials (wrong key)
exc_ssrt <- ssrt_data %>%
  filter(ssrt < ssrt_min) %>%
  transmute(participant,
            group,
            reason = paste0("SSRT too low (", round(ssrt, 2), "ms)"))

exclusions <- bind_rows(exclusions,
                        exc_ssrt)

# and filter these exclusions out of the main data going forwards
sst_data <- sst_data %>%
  filter(!participant %in% exclusions$participant)

# and the ssrt data
ssrt_data <- ssrt_data %>%
  filter(!participant %in% exclusions$participant)

##### RT trimming (Van Selst & Jolicoeur non-recursive method)

sst_rt_for_trimming <- sst_data %>%
  filter(
    (condition == "go"   & trial_acc != "missed") |    # go trials with a response
    (condition == "stop" & trial_acc == "failed stop")  # unsuccessful stop trials
  ) %>%
  mutate(acc_for_trimr = if_else(trial_acc == "wrong arrow", 0, 1)) # wrong arrows treated as errors

sst_rt_trimmed <- nonRecursive(
  data = sst_rt_for_trimming,
  pptVar = "participant",
  condVar = "condition",
  rtVar = "trial_rt",
  accVar = "acc_for_trimr",
  minRT = anticipatory_rt,
  digits = 0,
  returnType = "raw"
)


##### PRE-PROCESSING: Group level

# Mean RT per participant per condition
sst_summary_rts <- sst_rt_trimmed %>%
  group_by(group,
           participant,
           condition,
           trial_acc) %>%
  summarise(
    rt_mean = mean(trial_rt, na.rm = TRUE),
    rt_sd = sd(trial_rt,   na.rm = TRUE),
    .groups = "drop"
  )


##### Tukey outlier removal

sst_tukey <- sst_summary_rts %>%
  group_by(group,
           condition,
           trial_acc) %>%
  mutate(
    q1 = quantile(rt_mean, 0.25, na.rm = TRUE),
    q3 = quantile(rt_mean, 0.75, na.rm = TRUE),
    upper_bound = q3 + (3 * (q3 - q1)),
    lower_bound = q1 - (3 * (q3 - q1)),
    is_outlier = rt_mean < lower_bound | rt_mean > upper_bound
  ) %>%
  ungroup()


exc_tukey <- sst_tukey %>%
  filter(is_outlier) %>%
  transmute(
    participant,
    group,
    reason = paste0("mean RT outlier removed for condition: ", condition, " (Tukey 3×IQR, value: ", round(rt_mean), "ms)")
  )

exclusions <- bind_rows(exclusions, exc_tukey)

sst_summary_rts_outliers_removed <- sst_tukey %>%
  filter(!is_outlier) %>%
  select(-q1,
         -q3,
         -upper_bound,
         -lower_bound,
         -is_outlier)

##### Get variables we need for analysis

# SSRT we already have from earlier

# Go RT: use version after Tukey
sst_go_rt_summary <- sst_summary_rts_outliers_removed %>%
  filter(condition == "go") %>%
  select(group, participant, rt_mean, rt_sd)

# Accuracy: use original data, not affected by outlier removals
sst_accuracy <- sst_data %>%
  group_by(group, participant) %>%
  summarise(
    correct_go = sum(trial_acc == "correct" & condition == "go") / sum(condition == "go") * 100,
    correct_stop = sum(trial_acc == "successful stop"  & condition == "stop") / sum(condition == "stop") * 100,
    .groups = "drop"
  )


#### WIDE AND LONG FORMATS for stats and plots respectively

# wide: one row per participant, one column per measure
# give this a one-off name so it doesn't try to join to itself later when we do the log10 transformations
sst_wide_raw <- ssrt_data %>%
  select(group, participant, ssrt) %>%
  full_join(
    sst_go_rt_summary %>%
      select(group,
             participant,
             rt_mean) %>%
      rename(go_rt = rt_mean),
    by = c("group", "participant")
  ) %>%
  full_join(sst_accuracy, by = c("group", "participant")) %>%
  mutate(group = factor(group, levels = c("PwP", "PwP+ICB", "HC")))

# long: one row per participant per measure, for plotting and normality checks
sst_long <- sst_wide_raw %>%
  pivot_longer(
    cols = c(ssrt, go_rt, correct_go, correct_stop),
    names_to = "measure",
    values_to = "value"
  )

# # Tidy up the environment so that everything is easier to manage
# gdata::keep(exclusions,
#             sst_raw,
#             sst_data,
#             sst_wide,
#             sst_long,
#             sure = TRUE)


##### NORMALITY CHECKS

normality_plots <- ggplot(sst_long, aes(value)) +
  geom_histogram() +
  facet_grid(measure ~ group, scales = "free") +
  labs(title = "Histograms of SST measures")
normality_plots

normality_summary <- sst_long %>%
  group_by(group, measure) %>%
  summarise(
    p_value = shapiro.test(value)$p.value,
    .groups = "drop"
  )

# log10 transform if any measures fail normality (p < .05)
sst_long <- sst_long %>%
  mutate(value_log10 = log10(value))

# redo wide with log10 values included
sst_wide <- sst_long %>%
  select(group, participant, measure, value_log10) %>%
  pivot_wider(names_from = measure, values_from = value_log10, names_prefix = "log10_") %>%
  full_join(sst_wide_raw, by = c("group", "participant"))

# then check again. if any are still significant, use non-para tests for that measure
normality_summary_log10 <- sst_long %>%
  group_by(group, measure) %>%
  summarise(
    p_value = shapiro.test(value_log10)$p.value,
    .groups = "drop"
  )


######################
# SUMMARY STATISTICS #
######################

# summary statistics table (means)
sst_stats <- sst_long %>%
  group_by(group, measure) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value,   na.rm = TRUE),
    .groups = "drop"
  )


#########
# PLOTS #
#########

w = 4
h = 5

### SSRT

p_ssrt <- ggplot(sst_wide, aes(x = group, y = ssrt, fill = group, colour = group)) +
  geom_flat_violin(aes(fill = group), position = position_nudge(x = .3, y = 0), adjust = 1.2, trim = TRUE, alpha = .8, colour = "black", size = .5) +
  geom_point(aes(x = group, y = ssrt, fill = group, colour = group), position = position_jitter(width = .1), alpha = .7, size = 1) +
  geom_boxplot(aes(x = group, y = ssrt, fill = group), position = position_nudge(x = c(.22, .22), y = 0), outlier.shape = NA, alpha = .8, width = .1, colour = "black", size = .4) +
  ylab("SSRT (ms)") +
  xlab("") +
  cowplot::theme_cowplot() +
  viridis::scale_colour_viridis(discrete = TRUE, begin = .1, end = .5) +
  viridis::scale_fill_viridis(discrete = TRUE, begin = .1, end = .5) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.position = "none")
p_ssrt

#ggsave('figs/sst_ssrt.png', width = w, height = h)


### Go RT

p_go_rt <- ggplot(sst_wide, aes(x = group, y = go_rt, fill = group, colour = group)) +
  geom_flat_violin(aes(fill = group), position = position_nudge(x = .3, y = 0), adjust = 1.2, trim = TRUE, alpha = .8, colour = "black", size = .5) +
  geom_point(aes(x = group, y = go_rt, fill = group, colour = group), position = position_jitter(width = .1), alpha = .7, size = 1) +
  geom_boxplot(aes(x = group, y = go_rt, fill = group), position = position_nudge(x = c(.22, .22), y = 0), outlier.shape = NA, alpha = .8, width = .1, colour = "black", size = .4) +
  ylab("Go RT (ms)") +
  xlab("") +
  cowplot::theme_cowplot() +
  viridis::scale_colour_viridis(discrete = TRUE, begin = .1, end = .5) +
  viridis::scale_fill_viridis(discrete = TRUE, begin = .1, end = .5) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.position = "none")
p_go_rt

#ggsave('figs/sst_go_rt.png', width = w, height = h)


### Correct Go (%)

p_correct_go <- ggplot(sst_wide, aes(x = group, y = correct_go, fill = group, colour = group)) +
  geom_flat_violin(aes(fill = group), position = position_nudge(x = .3, y = 0), adjust = 1.2, trim = TRUE, alpha = .8, colour = "black", size = .5) +
  geom_point(aes(x = group, y = correct_go, fill = group, colour = group), position = position_jitter(width = .1), alpha = .7, size = 1) +
  geom_boxplot(aes(x = group, y = correct_go, fill = group), position = position_nudge(x = c(.22, .22), y = 0), outlier.shape = NA, alpha = .8, width = .1, colour = "black", size = .4) +
  ylab("Correct go trials (%)") +
  xlab("") +
  cowplot::theme_cowplot() +
  viridis::scale_colour_viridis(discrete = TRUE, begin = .1, end = .5) +
  viridis::scale_fill_viridis(discrete = TRUE, begin = .1, end = .5) +
  scale_y_continuous(limits = c(0, 100)) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.position = "none")
p_correct_go

#ggsave('figs/sst_correct_go.png', width = w, height = h)


### Correct Stop (%)

p_correct_stop <- ggplot(sst_wide, aes(x = group, y = correct_stop, fill = group, colour = group)) +
  geom_flat_violin(aes(fill = group), position = position_nudge(x = .3, y = 0), adjust = 1.2, trim = TRUE, alpha = .8, colour = "black", size = .5) +
  geom_point(aes(x = group, y = correct_stop, fill = group, colour = group), position = position_jitter(width = .1), alpha = .7, size = 1) +
  geom_boxplot(aes(x = group, y = correct_stop, fill = group), position = position_nudge(x = c(.22, .22), y = 0), outlier.shape = NA, alpha = .8, width = .1, colour = "black", size = .4) +
  ylab("Correct stop trials (%)") +
  xlab("") +
  cowplot::theme_cowplot() +
  viridis::scale_colour_viridis(discrete = TRUE, begin = .1, end = .5) +
  viridis::scale_fill_viridis(discrete = TRUE, begin = .1, end = .5) +
  scale_y_continuous(limits = c(0, 100)) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.position = "none")
p_correct_stop

#ggsave('figs/sst_correct_stop.png', width = w, height = h)


###########################
# INFERENTIAL STATISTICS #
#  Confirmatory analysis  #
###########################

# subset data for pairwise comparisons
pwp_hc_data <- sst_wide %>%
  filter(group == "PwP"|
           group == "HC")

pwp_icd_data <- sst_wide %>%
  filter(group == "PwP" |
           group == "PwP+ICB")

### SSRT

# variance tests
pwp_hc_ssrt_variance <- var.test(ssrt ~ group, data = pwp_hc_data)%>%
  broom::tidy()
pwp_icd_ssrt_variance <- var.test(ssrt ~ group, data = pwp_icd_data) %>%
  broom::tidy()

# anova
ssrt_aov <- aov(ssrt ~ group, data = sst_wide) %>%
  broom::tidy()

# t-tests
pwp_hc_ssrt_ttest <- t.test(ssrt ~ group, data = pwp_hc_data, var.equal = TRUE) %>%
  broom::tidy()
pwp_icd_ssrt_ttest <- t.test(ssrt ~ group, data = pwp_icd_data, var.equal = TRUE) %>%
  broom::tidy()


###########################
# INFERENTIAL STATISTICS #
#   Exploratory analysis  #
###########################

### Go RT

pwp_hc_go_rt_variance <- var.test(go_rt ~ group, data = pwp_hc_data) %>%
  broom::tidy()
pwp_icd_go_rt_variance <- var.test(go_rt ~ group, data = pwp_icd_data) %>%
  broom::tidy()

go_rt_aov <- aov(go_rt ~ group, data = sst_wide) %>%
  broom::tidy()

pwp_hc_go_rt_ttest<- t.test(go_rt ~ group, data = pwp_hc_data, var.equal = TRUE) %>%
  broom::tidy()
pwp_icd_go_rt_ttest <- t.test(go_rt ~ group, data = pwp_icd_data, var.equal = TRUE) %>%
  broom::tidy()


### Correct Go %

# kruskal-wallis
correct_go_kw <- kruskal.test(correct_go ~ group, data = sst_wide) %>%
  broom::tidy()

# mann-whitney u tests (named as wilcoxon in R, but this is independent samples version)
pwp_hc_correct_go_mwu <- wilcox.test(correct_go ~ group, data = pwp_hc_data, alternative = "two.sided", conf.int = TRUE) %>%
  broom::tidy()
pwp_icd_correct_go_mwu <- wilcox.test(correct_go ~ group, data = pwp_icd_data, alternative = "two.sided", conf.int = TRUE) %>%
  broom::tidy()


### Correct Stop %

correct_stop_kw <- kruskal.test(correct_stop ~ group, data = sst_wide) %>%
  broom::tidy()

pwp_hc_correct_stop_mwu <- wilcox.test(correct_stop ~ group, data = pwp_hc_data,alternative = "two.sided", conf.int = TRUE) %>%
  broom::tidy()
pwp_icd_correct_stop_mwu <- wilcox.test(correct_stop ~ group, data = pwp_icd_data, alternative = "two.sided", conf.int = TRUE) %>%
  broom::tidy()



