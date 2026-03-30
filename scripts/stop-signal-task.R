###############################################################################################################
# Script for analysis of the Stop Signal task according to the pre-registration on the Open Science Framework #
# Link:                                                                                                       #
# Authors: Jade Pickering & Marta Majewska, 2020-2026                                                              #
###############################################################################################################

##########
# SET-UP #
##########

# load all packages and install if necessary
requiredPackages = c('broom','janitor', 'gdata', 'cowplot', 'viridis', 'tidyverse')
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
         trial_type, # left go, right go, left stop, right stop
         key_pressed, # "m" (right hand), "z" (left hand)
         trial_rt,
         trial_acc,
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
  # # recode misses as an accuracy of 0
  # mutate(trial_acc = as.numeric(if_else(trial_acc == "miss", "0",
  #                                       trial_acc))) %>%
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
    go_response_rate = sum(trial_acc != "missed arrow" & condition == "go") / sum(condition == "go"),
    # choice error rate = proportion of go trials on which participant pressed the WRONG key
    go_choice_error_rate = sum(trial_acc == "incorrect arrow" & condition == "go") / sum(condition == "go"),
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
    # stop accuracy = proportion of stop trials where participant successfully stopped
    stop_acc = sum(condition == "stop" & trial_acc == "successful stop") / sum(condition == "stop"),
    # mean SSD across all stop trials
    mean_ssd = mean(ssd[condition == "stop"], na.rm = TRUE),
    # nth percentile of the go RT distribution
    # for omitted go trials, replace NA with the participant's max RT (Verbruggen et al., 2019)
    nth_percentile_go_rt = quantile(
      x     = if_else(condition == "go" & is.na(trial_rt),
                      max(trial_rt, na.rm = TRUE),   # replace omissions with max RT
                      trial_rt),
      probs = stop_acc,                              # nth percentile where n = stop accuracy
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

# because of the dummy data this actually excludes every participant, so revisit this once I've got real data in
# # and filter these exclusions out of the main data going forwards
# sst_data <- sst_data %>%
#   filter(!participant %in% exclusions$participant)


##### Pre-processing





# label trial types
gng_data <- gng_data %>%
  mutate(trial_type = if_else((condition == 1 & trial_acc == 1), "correct_go",
                              if_else((condition == 1 & trial_acc == 0), "omitted_go",
                                      if_else((condition == 0 & trial_acc == 0), "failed_nogo",
                                              if_else((condition == 0 & trial_acc == 1), "correct_nogo",
                                                      "other")))),
         trial_type = as.character(trial_type))


# for RT just keep trials with correct Go and failed No-Go trials
gng_rt <- gng_data %>%
  filter(trial_type == "correct_go" |
           trial_type == "failed_nogo")

# gng_rt_correct_go <- gng_rt %>%
#   filter(trial_type == "correct_go")
# 
# gng_rt_failed_nogo <- gng_rt %>%
#   filter(trial_type == "failed_nogo")


##### Van Selst & Jolicoeur trimming per participant and per condition

# need to handle the fact that this method usually only includes trimming correct trials
# but here the failed_nogos are 'incorrect' but important

# gng_rt <- gng_rt %>%
#   mutate(acc_dummy = 1)  # all trials will be treated as "correct" for trimming
# 
# gng_rt_trimmed <- nonRecursive(
#   data      = gng_rt,
#   pptVar    = "participant",
#   condVar   = "trial_type",
#   rtVar     = "trial_rt",
#   accVar    = "acc_dummy",
#   minRT     = anticipatory_rt,
#   digits    = 0,
#   returnType = "raw"
# )

## as the above is overzealous and RTs aren't the main variable of interest, going with non-trimmed data as a deviation from protocol
## except for anticipatory RTs

gng_rt_trimmed <- gng_rt %>%
  filter(trial_rt >= anticipatory_rt | is.na(trial_rt))


##### PRE-PROCESSING: Group level
# Calculate mean RTs for each condition and for each participant

gng_summary_rts <- gng_rt_trimmed %>%
  group_by(group,
           trial_type,
           participant) %>%
  summarise(rt_mean = mean(trial_rt, na.rm = TRUE),
            rt_sd = sd(trial_rt, na.rm = TRUE))

# Calculate mean accuracy for each condition and for each participant
# omission errors = % of go trials on which participants didn't respond)
# commission errors = % of no-go trials on which ppts failed to withold response)

gng_summary_acc <- gng_data %>%
  mutate(condition = as.character(condition)) %>%
  group_by(group, participant) %>%
  summarise(
    # sum all Go trials where there was no response
    omission_errors = sum(trial_type == "omitted_go" & condition == "1", na.rm = TRUE)
    # and divide it by sum of all Go trials and turn proportion into a percentage
    / sum(condition == "1") * 100,
    # sum all NoGo trials where there was a response (failed nogo)
    commission_errors = sum(trial_type == "failed_nogo" & condition == "0", na.rm = TRUE)
    # and divide it by sum of all Nogo trials and turn proportion into a percentage
    / sum(condition == "0") * 100
  )



##### Tukey outlier removal

# calculate outliers
gng_tukey <- gng_summary_rts %>%
  group_by(group,
           trial_type) %>%
  mutate(
    q1 = quantile(rt_mean, 0.25, na.rm = TRUE),
    q3 = quantile(rt_mean, 0.75, na.rm = TRUE),
    upper_bound = q3 + (3 * (q3 - q1)),
    lower_bound = q1 - (3 * (q3 - q1)),
    is_outlier = (rt_mean < lower_bound) | (rt_mean > upper_bound)
  ) %>%
  ungroup()

# identify outliers and add to exclusion table
exc_tukey <- gng_tukey %>%
  filter(is_outlier) %>%
  transmute(
    participant,
    group,
    reason = paste0(
      "RT removed just for the following condition due to Tukey's outlier removal (", trial_type, ")"
    )
  )

# put exclusions into the main exclusions table
exclusions <- bind_rows(exclusions,
                        exc_tukey)

# remove outliers from df

gng_summary_rts_outliers_removed <- gng_tukey %>%
  filter(!is_outlier) %>%
  select(-q1,
         -q3,
         -upper_bound,
         -lower_bound,
         -is_outlier)

# get wide and long versions of both separately, and joined 
gng_summary_rts_wide <- gng_summary_rts_outliers_removed %>%
  select(group, participant, trial_type, rt_mean) %>%
  pivot_wider(
    names_from  = trial_type,
    values_from = rt_mean
  ) %>%
  rename(mean_rt_correct_go = correct_go,
         mean_rt_failed_nogo = failed_nogo)

gng_summary_acc_wide <- gng_summary_acc

gng_summary_wide <- gng_summary_acc %>%
  full_join(gng_summary_rts_wide, by = c("participant", "group"))

gng_summary_rts_long <- gng_summary_rts_outliers_removed %>%
  select(-`rt_sd`) %>%
  rename(measure = trial_type,
         value = rt_mean)

gng_summary_acc_long <- gng_summary_acc %>%
  pivot_longer(
    cols = c(
      omission_errors,
      commission_errors
    ),
    names_to = "measure",
    values_to = "value"
  )

gng_summary_long <- gng_summary_acc_long %>%
  full_join(gng_summary_rts_long, by = c("participant", "group", "measure", "value"))


# # Tidy up the environment so that everything is easier to manage
# gdata::keep(exclusions,
#             gng_raw
#             gng_summary_rts_wide,
#             gng_summary_acc_wide,
#             gng_summary_wide,
#             gng_summary_rts_long,
#             gng_summary_acc_long,
#             gng_summary_long
#             sure = TRUE)

##### Testing for normality

normality_plots_rts <- ggplot(gng_summary_rts_long, aes(value)) +
  geom_histogram() +
  facet_grid(measure ~ group, scales = "free") +
  labs(
    title = "Histograms of RTs"
  )
normality_plots_rts

# Shapiro-Wilk tests
normality_summary <- gng_summary_rts_long %>%
  group_by(group, measure) %>%
  summarise(
    p_value = shapiro.test(value)$p.value,
    .groups = "drop"
  )

# if any of the measures in the normality_summary df are < 0.05 then we need to transform them
gng_summary_rts_long <- gng_summary_rts_long %>%
  mutate(value_log10 = log10(value))

# check that the transformation make the data normal now
normality_summary <- gng_summary_rts_long %>%
  group_by(group, measure) %>%
  summarise(
    p_value = shapiro.test(value_log10)$p.value,
    .groups = "drop"
  )


######################
# SUMMARY STATISTICS #
######################

# summary statistics table (means)
gng_stats <- gng_summary_long %>%
  group_by(group, measure) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value,   na.rm = TRUE),
    .groups = "drop"
  )


#########
# PLOTS #
#########

gng_summary_wide <- gng_summary_wide %>%
  mutate(group = factor(group, levels = c("PwP", "PwP+ICB", "HC")))

### Commission errors

w = 4
h = 5 

p1 <- ggplot(gng_summary_wide, aes(x = group, y = commission_errors, fill = group, colour = group)) +
  geom_flat_violin(aes(fill = group), position = position_nudge(x = .3, y = 0), adjust = 1.2, trim = TRUE, alpha = .8, colour = "black", size= .5) +
  geom_point(aes(x = group, y = commission_errors, fill = group, colour = group), position = position_jitter(width = .1), alpha = .7, size = 1) +
  geom_boxplot(aes(x = group, y = commission_errors, fill = group), position = position_nudge(x = c(.22,.22), y = 0), outlier.shape=NA, alpha = .8, width = .1, colour = "black", size =.4) +
  ylab("Commission errors (%)")+
  xlab("")+
  cowplot::theme_cowplot()+
  viridis::scale_colour_viridis(discrete = TRUE, begin = .1, end = .5)+
  viridis::scale_fill_viridis(discrete = TRUE, begin = .1, end = .5)+
  scale_y_continuous(breaks = seq(0, 60, 10),
                     limits = c(0, 60)) +
  #                    labels = c(-20,"",0,"",20,"",40,"",60,"",80,"",100))+
  #scale_x_discrete(labels=c("PwP", "PwP+ICBs", "HCs"))+
  theme(axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.title.x = element_text(size=11),
        axis.title.y = element_text(size=11),
        legend.position = "none")
p1

#ggsave('figs/gng_commission.png', width = w, height = h)


###########################
# INFERENTIAL STATISTICS #
#  Confirmatory analysis  #
###########################

# subset data for pairwise comparisons
pwp_hc_data <- gng_summary_wide %>%
  filter(group == "PwP" |
           group == "HC")

pwp_icd_data <- gng_summary_wide %>%
  filter(group == "PwP" |
           group == "PwP+ICB")

# COMMISSION ERRORS
# kruskal-wallis
commission_errors_kw <- kruskal.test(commission_errors ~ group, data = gng_summary_wide) %>%
  broom::tidy()

# mann-whitney u tests (named as wilcoxon in R, but this is independent samples version)
pwp_hc_commission_mwu <- wilcox.test(commission_errors ~ group, data = pwp_hc_data, alternative = "two.sided", conf.int = TRUE) %>%
  broom::tidy()
pwp_icd_commission_mwu <- wilcox.test(commission_errors ~ group, data = pwp_icd_data, alternative = "two.sided", conf.int = TRUE) %>%
  broom::tidy()

###########################
# INFERENTIAL STATISTICS #
#   Exploratory analysis  #
###########################

### GO RT

# variance test
pwp_hc_go_rt_variance <- var.test(mean_rt_correct_go ~ group, data = pwp_hc_data) %>%
  broom::tidy() 
pwp_icd_go_rt_variance <- var.test(mean_rt_correct_go ~ group, data = pwp_icd_data) %>%
  broom::tidy() 

# anova
go_rt_aov <- aov(mean_rt_correct_go ~ group, data = gng_summary_wide) %>%
  broom::tidy() 

# t-tests
pwp_hc_go_rt_ttest <- t.test(mean_rt_correct_go ~ group, data = pwp_hc_data, var.equal = TRUE) %>%
  broom::tidy() 
pwp_icd_go_rt_ttest <- t.test(mean_rt_correct_go ~ group, data = pwp_icd_data, var.equal = TRUE) %>%
  broom::tidy() 


### NO-GO RT

# variance test
pwp_hc_nogo_rt_variance <- var.test(mean_rt_failed_nogo ~ group, data = pwp_hc_data) %>%
  broom::tidy() 
pwp_icd_nogo_rt_variance <- var.test(mean_rt_failed_nogo ~ group, data = pwp_icd_data) %>%
  broom::tidy() 

# anova
nogo_rt_aov <- aov(mean_rt_failed_nogo ~ group, data = gng_summary_wide) %>%
  broom::tidy() 

# t-tests
pwp_hc_nogo_rt_ttest <- t.test(mean_rt_failed_nogo ~ group, data = pwp_hc_data, var.equal = TRUE) %>%
  broom::tidy() 
pwp_icd_nogo_rt_ttest <- t.test(mean_rt_failed_nogo ~ group, data = pwp_icd_data, var.equal = TRUE) %>%
  broom::tidy() 



