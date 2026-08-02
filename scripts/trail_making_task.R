############################################################################################################
# Script for analysis of the Trail Making Test according to the pre-registration on the Open Science      #
# Framework                                                                                                #
# Link:                                                                                                    #
# Authors: Jade Pickering & Marta Majewska, 2020-2026                                                      #
############################################################################################################

##########
# SET-UP #
##########

# load all packages and install if necessary
requiredPackages = c('tidyverse', 'janitor')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

# to get the raincloud plots working
source("https://raw.githubusercontent.com/RainCloudPlots/RainCloudPlots/master/tutorial_R/R_rainclouds.R")

################
# READ IN DATA #
################

# TMT data is in the same file as demographics and questionnaires
tmt_raw <- read_csv("raw_data/demographics_q_dummy_data.csv") %>%
  clean_names() %>%
  drop_na(group) %>%
  select(
    participant_id = x1,
    group,
    tmt_a,
    tmt_b
  ) %>%
  mutate(
    across(c(participant_id, group), as.character),
    across(starts_with("tmt"), as.numeric)
  ) %>%
  filter(group != "YC") %>%
  mutate(group = if_else(group %in% c("PWP", "PwP"), "PwP",
                   if_else(group == "ICD", "PwP+ICB",
                     if_else(group == "OC", "HC", "error"))))


##############
# EXCLUSIONS #
##############

# empty exclusions table to fill in later
exclusions <- tibble(
  participant_id = character(),
  group = character(),
  reason = character()
)

# get moca exclusions
moca_exclusions <- read_csv("outputs/moca_exclusions.csv")
moca_excluded <- moca_exclusions$participant_id

# remove moca exclusions
tmt_data <- tmt_raw %>%
  filter(!participant_id %in% moca_excluded) %>%
  mutate(group = factor(group, levels = c("PwP", "PwP+ICB", "HC")))

# identify participants who did not complete trail A or trail B as we won't have anything to analyse
exc_incomplete <- tmt_data %>%
  filter(is.na(tmt_a) | is.na(tmt_b)) %>%
  transmute(
    participant_id,
    group,
    reason = case_when(
      is.na(tmt_a) & is.na(tmt_b) ~ "did not complete TMT trails A or B",
      is.na(tmt_a) ~ "did not complete TMT trail A",
      is.na(tmt_b) ~ "did not complete TMT trail B"
    )
  )

# add them to the exclusions df
exclusions <- bind_rows(exclusions, exc_incomplete)

# and remove them from this analysis
tmt_data <- tmt_data %>%
  filter(!participant_id %in% exc_incomplete$participant_id)


#################
# PRE-PROCESSING #
#################

# trail A = time for numbers only (1-2-3 etc)
# trail B = time for alternating numbers and letters (1-A-2-B etc)
# switch cost = trail B time minus trail A time

# calculate switch cost
tmt_wide <- tmt_data %>%
  mutate(
    switch_cost = tmt_b - tmt_a
  )

# Tukey outlier removal on completion times and switch cost
tmt_tukey <- tmt_wide %>%
  select(participant_id,
         group,
         tmt_a,
         tmt_b,
         switch_cost) %>%
  pivot_longer(
    cols = c(tmt_a,
             tmt_b,
             switch_cost),
    names_to = "measure",
    values_to = "value"
  ) %>%
  group_by(group,
           measure) %>%
  mutate(
    q1 = quantile(value, 0.25, na.rm = TRUE),
    q3 = quantile(value, 0.75, na.rm = TRUE),
    upper_bound = q3 + (3 * (q3 - q1)),
    lower_bound = q1 - (3 * (q3 - q1)),
    is_outlier = value < lower_bound | value > upper_bound
  ) %>%
  ungroup()

exc_tukey <- tmt_tukey %>%
  filter(is_outlier) %>%
  transmute(
    participant_id,
    group,
    reason = paste0("TMT outlier removed for measure: ", measure,
                    " (Tukey 3×IQR, value: ", round(value, 1), "s)")
  )

exclusions <- bind_rows(exclusions, exc_tukey)

# long format with outliers removed
# used for normality checks, plots, and summary stats
tmt_long <- tmt_tukey %>%
  filter(!is_outlier) %>%
  select(participant_id, group, measure, value)

# give this a one-off name so it doesn't try to join to itself later when we do the log10 transformations
tmt_wide_raw <- tmt_long %>%
  pivot_wider(names_from = measure, values_from = value)


##### Normality checks

normality_plots <- ggplot(tmt_long, aes(value)) +
  geom_histogram() +
  facet_grid(measure ~ group, scales = "free") +
  labs(title = "Histograms of TMT measures")
normality_plots

normality_summary <- tmt_long %>%
  group_by(group, measure) %>%
  summarise(
    p_value = shapiro.test(value[!is.na(value)])$p.value,
    .groups = "drop"
  )

# log10 transform if any measures fail normality (p < .05)
# it's easier to apply the transformation to everything and then just choose which value to use in the tests later
# the str_detect ensures we don't apply it to switch_cost which may have negative values (and can't be transformed)
tmt_long <- tmt_long %>%
  mutate(value_log10 = if_else(!str_detect(measure, "switch_cost"), log10(value), NA_real_))

# re-check normality after transform and if still significant use non-parametric on untransformed data
# switch_cost excluded here as they were not log10 transformed above (see note)
# non-parametric tests should use the original untransformed value for switch_cost
normality_summary_log10 <- tmt_long %>%
  filter(!str_detect(measure, "switch_cost")) %>%
  group_by(group, measure) %>%
  summarise(
    p_value = shapiro.test(value_log10[!is.na(value_log10)])$p.value,
    .groups = "drop"
  )

# redo wide with log10 values included
tmt_wide <- tmt_long %>%
  select(group, participant_id, measure, value_log10) %>%
  pivot_wider(names_from = measure, values_from = value_log10, names_prefix = "log10_") %>%
  full_join(tmt_wide_raw, by = c("group", "participant_id")) %>%
  select(-log10_switch_cost)


######################
# SUMMARY STATISTICS #
######################

tmt_descriptives <- tmt_long %>%
  group_by(group, measure) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value, na.rm = TRUE),
    .groups = "drop"
  )


#########
# PLOTS #
#########

w = 4
h = 5

### Switch cost

p_switch_cost <- ggplot(tmt_wide, aes(x = group, y = switch_cost, fill = group, colour = group)) +
  geom_flat_violin(aes(fill = group), position = position_nudge(x = .3, y = 0), adjust = 1.2, trim = TRUE, alpha = .8, colour = "black", size = .5) +
  geom_point(aes(x = group, y = switch_cost, fill = group, colour = group), position = position_jitter(width = .1), alpha = .7, size = 1) +
  geom_boxplot(aes(x = group, y = switch_cost, fill = group), position = position_nudge(x = c(.22, .22), y = 0), outlier.shape = NA, alpha = .8, width = .1, colour = "black", size = .4) +
  ylab("Switch cost (s)") +
  xlab("") +
  cowplot::theme_cowplot() +
  viridis::scale_colour_viridis(discrete = TRUE, begin = .1, end = .5) +
  viridis::scale_fill_viridis(discrete = TRUE, begin = .1, end = .5) +
  theme(axis.text.x  = element_text(size = 10),
        axis.text.y  = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.position = "none")
p_switch_cost

#ggsave('figs/tmt_switch_cost.png', width = w, height = h)


### Trail completion times (A and B), faceted

p_trail_times <- ggplot(
  tmt_long %>% filter(measure %in% c("tmt_a", "tmt_b")),
  aes(x = group, y = value, fill = group, colour = group)) +
  geom_flat_violin(aes(fill = group), position = position_nudge(x = .3, y = 0), adjust = 1.2, trim = TRUE, alpha = .8, colour = "black", size = .5) +
  geom_point(aes(x = group, y = value, fill = group, colour = group), position = position_jitter(width = .1), alpha = .7, size = 1) +
  geom_boxplot(aes(x = group, y = value, fill = group), position = position_nudge(x = c(.22, .22), y = 0), outlier.shape = NA, alpha = .8, width = .1, colour = "black", size = .4) +
  facet_wrap(~ measure, scales = "free_y") +
  ylab("Time (s)") +
  xlab("") +
  cowplot::theme_cowplot() +
  viridis::scale_colour_viridis(discrete = TRUE, begin = .1, end = .5) +
  viridis::scale_fill_viridis(discrete = TRUE, begin = .1, end = .5) +
  theme(axis.text.x  = element_text(size = 10),
        axis.text.y  = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.position = "none")
p_trail_times

#ggsave('figs/tmt_trails.png', width = w * 2, height = h)


###########################
# INFERENTIAL STATISTICS  #
#  Confirmatory analysis  #
###########################

# subset data for planned pairwise comparisons
pwp_hc_data  <- tmt_wide %>%
  filter(group == "PwP" |
           group == "HC")

pwp_icd_data <- tmt_wide %>%
  filter(group == "PwP" |
           group == "PwP+ICB")

### SWITCH COST (trail B minus trail A)
# (here and below, if we need to use non-parametric tests on real data we'll need to edit this section)

# variance tests
pwp_hc_switch_cost_variance  <- var.test(switch_cost ~ group, data = pwp_hc_data) %>%
  broom::tidy()
pwp_icd_switch_cost_variance <- var.test(switch_cost ~ group, data = pwp_icd_data) %>%
  broom::tidy()

# one-way between-subjects ANOVA
switch_cost_aov <- aov(switch_cost ~ group, data = tmt_wide) %>%
  broom::tidy()

# planned independent t-tests
# change 'var.equal' depending on the variance tests above
pwp_hc_switch_cost_ttest  <- t.test(switch_cost ~ group, data = pwp_hc_data, var.equal = TRUE) %>%
  broom::tidy()
pwp_icd_switch_cost_ttest <- t.test(switch_cost ~ group, data = pwp_icd_data, var.equal = TRUE) %>%
  broom::tidy()


###########################
# INFERENTIAL STATISTICS  #
#   Exploratory analysis  #
###########################

### TRAIL B COMPLETION TIME

# normality
tmt_b_normality <- tmt_wide %>%
  group_by(group) %>%
  summarise(
    p_value = shapiro.test(tmt_b[!is.na(tmt_b)])$p.value,
    .groups = "drop"
  )

# variance tests
pwp_hc_tmt_b_variance  <- var.test(tmt_b ~ group, data = pwp_hc_data) %>%
  broom::tidy()
pwp_icd_tmt_b_variance <- var.test(tmt_b ~ group, data = pwp_icd_data) %>%
  broom::tidy()

# one-way ANOVA
tmt_b_aov <- aov(tmt_b ~ group, data = tmt_wide) %>%
  broom::tidy()

# planned t-tests
# change 'var.equal' depending on the variance tests above
pwp_hc_tmt_b_ttest  <- t.test(tmt_b ~ group, data = pwp_hc_data, var.equal = TRUE) %>%
  broom::tidy()
pwp_icd_tmt_b_ttest <- t.test(tmt_b ~ group, data = pwp_icd_data, var.equal = TRUE) %>%
  broom::tidy()
