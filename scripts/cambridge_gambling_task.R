######################################################################################################################
# Script for analysis of the Cambridge Gambling Task according to the pre-registration on the Open Science Framework #
# Link: https://osf.io/y8drq/files/frzpv                                                                             #
######################################################################################################################

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
trials_per_block = 9 # one trial per box ratio (1:9 through 9:1)
blocks_per_condition = 4 # 4 ascending and 4 descending blocks
min_blocks_per_condition = 3 # participants must complete at least 3 of 4 blocks in EACH condition

# empty exclusions table to fill in later
exclusions <- tibble(
  participant = character(),
  group = character(),
  reason = character()
)

# trials where the researcher noted the participant stopped actively deliberating (e.g. distractions or talking)
# are excluded from the deliberation time analysis only
delib_manual_exclusions <- tibble(
  participant = character(),
  condition = character(),
  block_num = numeric(),
  trial_num = numeric()
) # add trials manually here from the session notes by uncommenting the below and changing the details
# %>%
#   add_row(participant = "001", condition = "asc", block_num = 1, trial_num = 1)

################
# READ IN DATA #
################

data_dir <- "./raw_data/cambridge_gambling_task_dummy" # dummy data for development
# data_dir <- "Z:/Study 2_cross sectional/DATA/CGT" # file path to network drive

groups <- c("PWP", "ICD", "HC", "YC") # specify groups
group_dfs <- list() # create empty list to hold data for each group

cgt_cols <- c("participant", "condition", "block_num", "trial_num", "trial_type", "colour_rt", "colour_choice",
              "bet_rt", "bet_choice", "winlose", "bet_value", "total_points", "failed_block", "empty") # there's a trailing comma on each row of data, giving an empty column

# for every group
for (g in groups) {
  # find all csv files in that group's folder
  files <- list.files(
    path = file.path(data_dir, g),
    pattern = "\\.csv$",
    full.names = TRUE
  )

  # read every participant's file in as character (so ids like "006" keep their leading zeros)
  group_dfs[[g]] <- map_df(files, ~read_csv(.x, skip = 1, col_names = cgt_cols, col_types = cols(.default = "c"))) %>%
    mutate(group = g) # the group folder currently being processed
}

# combine all group dfs into one big df where every row is one trial by one participant
cgt_raw <- bind_rows(group_dfs)


##### Tidy data
cgt_data <- cgt_raw %>%
  select(-empty) %>% # drop the empty column created by the trailing commas
  # convert data types for all columns to make sure they're correct
  mutate(
    across(c(block_num,
             trial_num,
             trial_type,
             colour_rt,
             bet_rt,
             bet_choice,
             bet_value,
             total_points), as.numeric),
    participant = str_pad(participant, width = 3, pad = "0") # leading 0s needed to check against MoCA failures data
  ) %>%
  mutate(
    # trial_type = 1 means 9 red boxes and 1 blue
    # trial_type = 2 means 8 red boxes and 2 blue
    # and so on, so trial_type equals the number of blue boxes out of 10,
    # so we can work out the red and blue box counts from it
    n_blue = trial_type,
    n_red = 10 - trial_type,
    ratio = if_else(trial_type == 1 | trial_type == 9, "9_1",
                    if_else(trial_type == 2 | trial_type == 8, "8_2",
                            if_else(trial_type == 3 | trial_type == 7, "7_3",
                                    if_else(trial_type == 4 | trial_type == 6, "6_4",
                                            if_else(trial_type == 5, "5_5", "error"))))),
    # convert categorical bet choice to the proportion of total points bet
    # bet_choice is the absolute stake level (1 = 5% ... 5 = 95%) in BOTH ascending and descending blocks
    bet_proportion = if_else(bet_choice == 1, 0.05,
                       if_else(bet_choice == 2, 0.25,
                               if_else(bet_choice == 3, 0.50,
                                       if_else(bet_choice == 4, 0.75,
                                               if_else(bet_choice == 5, 0.95, NA))))),
    # the majority colour for the quality of decision making measure (NA on the even 5:5 trials, which are dropped before it's used)
    majority = if_else(n_red > n_blue, "red",
                       if_else(n_blue > n_red, "blue", NA)),
    chose_majority = colour_choice == majority
  ) %>%
  # remove YC participants (if there are any) as this was collected for MM's project
  filter(group != "YC") %>%
  # recode group labels
  mutate(group = if_else((group == "PWP" | group == "PwP"), "PwP",
                         if_else(group == "ICD", "PwP+ICB",
                                 if_else((group == "OC" | group == "HC"), "HC", "error")))) %>%
  # keep only the necessary columns 
  select(group, participant, condition, block_num, trial_num, ratio, colour_rt, bet_proportion, chose_majority, failed_block)


##### Look for exclusions

# get moca exclusions (written out by demographics.R, so that needs to have been run first)
moca_exclusions <- read_csv("outputs/moca_exclusions.csv")
moca_excluded <- str_pad(moca_exclusions$participant_id, width = 3, pad = "0") # pad to match the CGT's 3 digit style ids

# remove moca exclusions
cgt_data <- cgt_data %>%
  filter(!participant %in% moca_excluded)

# exclude participants who completed fewer than 3 of 4 blocks in either condition
# a block counts as completed if all 9 trials were played OR the participant was bankrupt which ends the block early
cgt_block_completion <- cgt_data %>%
  group_by(group, participant, condition, block_num) %>%
  summarise(n_trials = n(),
            n_failed = sum(failed_block == "True"), # count of bankrupt trials in the block (should only ever be 0 or 1)
            .groups = "drop") %>%
  mutate(bankrupt = n_failed > 0, # define a block where participant went bankrupt
         completed = (n_trials == trials_per_block | bankrupt)) # if its the expected number of trials or they went bankrupt, it counts as completed

# get a list of blocks completed per participant
cgt_condition_completion <- cgt_block_completion %>%
  group_by(group, participant, condition) %>%
  summarise(blocks_completed = sum(completed),
            .groups = "drop") %>%
  select(participant,
         group,
         blocks_completed) %>%
  distinct() # only really need one row per participant that at least  shows the lowest number they completed

# identify participants for exclusion
exc_blocks <- cgt_condition_completion %>%
  filter(blocks_completed < min_blocks_per_condition) %>%
  transmute(participant,
            group,
            reason = paste0("completed fewer than ", min_blocks_per_condition, " of ", blocks_per_condition,
                            " blocks in one or both conditions"))

exclusions <- bind_rows(exclusions,
                        exc_blocks)

# filter excluded participants from main data going forwards
cgt_data <- cgt_data %>%
  filter(!participant %in% exclusions$participant)


##### Pre-processing

# the trials where the box ratios were 5:5 are not used for analysis
cgt_data <- cgt_data %>%
  filter(ratio != "5_5")


##### Scoring

# mean bet proportion per participant per box ratio (needed for risk adjustment index)
cgt_ratio_bets <- cgt_data %>%
  group_by(group, participant, ratio) %>%
  summarise(mean_bet = mean(bet_proportion),
            .groups = "drop")

# risk adjustment index per participant, higher = less risky decision making
cgt_risk_adj <- cgt_ratio_bets %>%
  pivot_wider(names_from = ratio, values_from = mean_bet, names_prefix = "bet_") %>%
  mutate(
    overall_mean_bet = (bet_9_1 + bet_8_2 + bet_7_3 + bet_6_4) / 4,
    risk_adj_index = (((2 * bet_9_1) + bet_8_2 - bet_7_3 - (2 * bet_6_4)) / overall_mean_bet),
    group = factor(group, levels = c("PwP", "PwP+ICB", "HC"))
  ) %>%
  select(group, participant, risk_adj_index) # only keep what we need for analysis

# mean deliberation time (time to choose red or blue box) per participant per box ratio
cgt_deliberation <- cgt_data %>%
  anti_join(delib_manual_exclusions, by = c("participant", "condition", "block_num", "trial_num")) %>% # drop researcher-noted "stopped deliberating" trials
  group_by(group, participant, ratio) %>%
  summarise(mean_delib = mean(colour_rt),
            .groups = "drop") %>%
  mutate(
    ratio = factor(ratio, levels = c("9_1", "8_2", "7_3", "6_4"), labels = c("9:1", "8:2", "7:3", "6:4")),
    group = factor(group, levels = c("PwP", "PwP+ICB", "HC"))
  )

# quality of decision making per participant (% of choices that align with the most likely outcome)
cgt_quality <- cgt_data %>%
  group_by(group, participant) %>%
  summarise(quality = 100 * mean(chose_majority),
            .groups = "drop") %>%
  mutate(group = factor(group, levels = c("PwP", "PwP+ICB", "HC")))


##### Normality checks

risk_adj_normality_plots <- ggplot(cgt_risk_adj, aes(risk_adj_index)) +
  geom_histogram() +
  facet_wrap(~ group, scales = "free") +
  labs(title = "Histograms of CGT risk adjustment index")
risk_adj_normality_plots

risk_adj_normality_summary <- cgt_risk_adj %>%
  group_by(group) %>%
  summarise(
    p_value = shapiro.test(risk_adj_index)$p.value,
    .groups = "drop"
  )

# NB: the risk adjustment index can't be log10 transformed as it can have negative values,
# so if it fails the normality checks use the non-parametric tests on the untransformed data instead

# same again for the deliberation times
delib_normality_plots <- ggplot(cgt_deliberation, aes(mean_delib)) +
  geom_histogram() +
  facet_grid(ratio ~ group, scales = "free") +
  labs(title = "Histograms of CGT deliberation times")
delib_normality_plots

delib_normality_summary <- cgt_deliberation %>%
  group_by(group,
           ratio) %>%
  summarise(
    p_value = shapiro.test(mean_delib)$p.value,
    .groups = "drop"
  )

# log10 transform if any conditions fail normality checks above (p < .05)
# may not need this if everything is fine with the real data
cgt_deliberation <- cgt_deliberation %>%
  mutate(mean_delib_log10 = log10(mean_delib))

# re-check normality after transforming and if still non-normal use non-parametric tests on untransformed data
delib_normality_summary_log10 <- cgt_deliberation %>%
  group_by(group,
           ratio) %>%
  summarise(
    p_value = shapiro.test(mean_delib_log10)$p.value,
    .groups = "drop"
  )

# quality of decision making is expected to be non-normal so need to check normality


######################
# SUMMARY STATISTICS #
######################

cgt_risk_adj_stats <- cgt_risk_adj %>%
  group_by(group) %>%
  summarise(
    mean = mean(risk_adj_index, na.rm = TRUE),
    sd = sd(risk_adj_index, na.rm = TRUE),
    .groups = "drop"
  )

# deliberation times per group per ratio
cgt_delib_stats <- cgt_deliberation %>%
  group_by(group,
           ratio) %>%
  summarise(
    mean = mean(mean_delib, na.rm = TRUE),
    sd = sd(mean_delib, na.rm = TRUE),
    .groups = "drop"
  )

# quality of decision making per group
cgt_quality_stats <- cgt_quality %>%
  group_by(group) %>%
  summarise(
    mean = mean(quality, na.rm = TRUE),
    sd = sd(quality, na.rm = TRUE),
    .groups = "drop"
  )


#########
# PLOTS #
#########

w = 4
h = 5

### Risk adjustment index

p_cgt_rai <- ggplot(cgt_risk_adj, aes(x = group, y = risk_adj_index, fill = group, colour = group)) +
  geom_flat_violin(aes(fill = group), position = position_nudge(x = .3, y = 0), adjust = 1.2, trim = TRUE, alpha = .8, colour = "black", size = .5) +
  geom_point(aes(x = group, y = risk_adj_index, fill = group, colour = group), position = position_jitter(width = .1), alpha = .7, size = 1) +
  geom_boxplot(aes(x = group, y = risk_adj_index, fill = group), position = position_nudge(x = c(.22, .22), y = 0), outlier.shape = NA, alpha = .8, width = .1, colour = "black", size = .4) +
  ylab("Risk adjustment index\n(lower = riskier decision-making)") +
  xlab("") +
  cowplot::theme_cowplot() +
  viridis::scale_colour_viridis(discrete = TRUE, begin = .1, end = .5) +
  viridis::scale_fill_viridis(discrete = TRUE, begin = .1, end = .5) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.position = "none")
p_cgt_rai

#ggsave('figs/cgt_rai.png', width = w, height = h)


### Deliberation times

# raincloud per group, faceted by box ratio
p_cgt_delib <- ggplot(
  cgt_deliberation,
  aes(x = group, y = mean_delib, fill = group, colour = group)) +
  geom_flat_violin(aes(fill = group), position = position_nudge(x = .3, y = 0), adjust = 1.2, trim = TRUE, alpha = .8, colour = "black", size = .5) +
  geom_point(aes(x = group, y = mean_delib, fill = group, colour = group), position = position_jitter(width = .1), alpha = .7, size = 1) +
  geom_boxplot(aes(x = group, y = mean_delib, fill = group), position = position_nudge(x = c(.22, .22), y = 0), outlier.shape = NA, alpha = .8, width = .1, colour = "black", size = .4) +
  facet_wrap(~ ratio, nrow = 1) +
  ylab("Mean deliberation time (ms)") +
  xlab("") +
  cowplot::theme_cowplot() +
  viridis::scale_colour_viridis(discrete = TRUE, begin = .1, end = .5) +
  viridis::scale_fill_viridis(discrete = TRUE, begin = .1, end = .5) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.position = "none")
p_cgt_delib

#ggsave('figs/cgt_delib.png', width = w * 2.5, height = h)


### Quality of decision making

p_cgt_quality <- ggplot(cgt_quality, aes(x = group, y = quality, fill = group, colour = group)) +
  geom_flat_violin(aes(fill = group), position = position_nudge(x = .3, y = 0), adjust = 1.2, trim = TRUE, alpha = .8, colour = "black", size = .5) +
  geom_point(aes(x = group, y = quality, fill = group, colour = group), position = position_jitter(width = .1), alpha = .7, size = 1) +
  geom_boxplot(aes(x = group, y = quality, fill = group), position = position_nudge(x = c(.22, .22), y = 0), outlier.shape = NA, alpha = .8, width = .1, colour = "black", size = .4) +
  ylab("Quality of decision making (% most likely option was chosen)") +
  xlab("") +
  cowplot::theme_cowplot() +
  viridis::scale_colour_viridis(discrete = TRUE, begin = .1, end = .5) +
  viridis::scale_fill_viridis(discrete = TRUE, begin = .1, end = .5) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.position = "none")
p_cgt_quality

#ggsave('figs/cgt_quality.png', width = w, height = h)


###########################
# INFERENTIAL STATISTICS  #
#  Confirmatory analysis  #
###########################

# subset data for planned pairwise comparisons
pwp_hc_rai <- cgt_risk_adj %>%
  filter(group == "PwP" |
           group == "HC")

pwp_icd_rai <- cgt_risk_adj %>%
  filter(group == "PwP" |
           group == "PwP+ICB")

### RISK ADJUSTMENT INDEX

# variance tests
pwp_hc_rai_variance <- var.test(risk_adj_index ~ group, data = pwp_hc_rai) %>%
  broom::tidy()
pwp_icd_rai_variance <- var.test(risk_adj_index ~ group, data = pwp_icd_rai) %>%
  broom::tidy()

# one-way between-subjects ANOVA
cgt_rai_aov <- aov(risk_adj_index ~ group, data = cgt_risk_adj) %>%
  broom::tidy()

# planned independent t-tests
# change 'var.equal' depending on the variances tests above
pwp_hc_rai_ttest <- t.test(risk_adj_index ~ group, data = pwp_hc_rai, var.equal = TRUE) %>%
  broom::tidy()
pwp_icd_rai_ttest <- t.test(risk_adj_index ~ group, data = pwp_icd_rai, var.equal = TRUE) %>%
  broom::tidy()

# non-parametric equivalents, only needed if the normality checks above fail
# cgt_rai_kw <- kruskal.test(risk_adj_index ~ group, data = cgt_risk_adj) %>%
#   broom::tidy()

# pwp_hc_rai_mwu <- wilcox.test(risk_adj_index ~ group, data = pwp_hc_rai, alternative = "two.sided", conf.int = TRUE) %>%
#   broom::tidy()
# pwp_icd_rai_mwu <- wilcox.test(risk_adj_index ~ group, data = pwp_icd_rai, alternative = "two.sided", conf.int = TRUE) %>%
#   broom::tidy()


###########################
# INFERENTIAL STATISTICS  #
#   Exploratory analysis  #
###########################

### DELIBERATION TIMES (expected to be shorter for bigger ratios)

# two-way mixed ANOVA, ratio is within-subjects, group is between-subjects
# if normality checks fail but log10 transofmation fixess them, swap mean_delib for mean_delib_log10 here and in posthocs below
# NB: if they're still non-normal after transforming there's no standard non-para equivalent of a 2-way mixed ANOVA afaik
# we'd need to choose rank-based alternative but cross that bridge if/when we come to it
cgt_delib_anova <- aov(mean_delib ~ group * ratio + Error(participant/ratio), data = cgt_deliberation) %>%
  broom::tidy()

# # post-hoc pairwise comparisons across ratios, only to be interpreted if the ANOVA shows a
# # significant effect of ratio. needs rows ordered consistently within each ratio for the pairing
# cgt_delib_posthoc <- cgt_deliberation %>%
#   arrange(ratio, group, participant) %>%
#   with(., pairwise.t.test(mean_delib, ratio, paired = TRUE, p.adjust.method = "bonferroni")) %>%
#   broom::tidy()

# # subset data for planned pairwise comparisons, same group pairs as the confirmatory analysis
# pwp_hc_delib <- cgt_deliberation %>%
#   filter(group == "PwP" |
#            group == "HC")
#
# pwp_icd_delib <- cgt_deliberation %>%
#   filter(group == "PwP" |
#            group == "PwP+ICB")
#
# # planned independent t-tests, run within each ratio, only to be interpreted if the ANOVA shows
# # a group x ratio interaction
# pwp_hc_delib_ttest <- pwp_hc_delib %>%
#   group_by(ratio) %>%
#   group_modify(~ t.test(mean_delib ~ group, data = .x, var.equal = TRUE) %>%
#                  broom::tidy())
# pwp_icd_delib_ttest <- pwp_icd_delib %>%
#   group_by(ratio) %>%
#   group_modify(~ t.test(mean_delib ~ group, data = .x, var.equal = TRUE) %>%
#                  broom::tidy())

# # post-hoc group comparisons for a main effect of group, on deliberation times averaged across
# # ratios per participant, same group pairs as the confirmatory analysis
# cgt_delib_group_means <- cgt_deliberation %>%
#   group_by(group, participant) %>%
#   summarise(overall_mean_delib = mean(mean_delib),
#             .groups = "drop")
#
# pwp_hc_delib_main_ttest <- cgt_delib_group_means %>%
#   filter(group == "PwP" |
#            group == "HC") %>%
#   t.test(overall_mean_delib ~ group, data = ., var.equal = TRUE) %>%
#   broom::tidy()
# pwp_icd_delib_main_ttest <- cgt_delib_group_means %>%
#   filter(group == "PwP" |
#            group == "PwP+ICB") %>%
#   t.test(overall_mean_delib ~ group, data = ., var.equal = TRUE) %>%
#   broom::tidy()


### QUALITY OF DECISION MAKING

# subset data for planned pairwise comparisons
pwp_hc_quality <- cgt_quality %>%
  filter(group == "PwP" |
           group == "HC")

pwp_icd_quality <- cgt_quality %>%
  filter(group == "PwP" |
           group == "PwP+ICB")

# Kruskal-Wallis
cgt_quality_kw <- kruskal.test(quality ~ group, data = cgt_quality) %>%
  broom::tidy()

# planned Mann-Whitney U tests (named as wilcoxon in R, but this is the independent samples version)
pwp_hc_quality_mwu <- wilcox.test(quality ~ group, data = pwp_hc_quality, alternative = "two.sided", conf.int = TRUE) %>%
  broom::tidy()
pwp_icd_quality_mwu <- wilcox.test(quality ~ group, data = pwp_icd_quality, alternative = "two.sided", conf.int = TRUE) %>%
  broom::tidy()
