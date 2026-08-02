############################################################################################################
# Script for analysis of the Stroop Color-Word Test according to the pre-registration on the Open Science #
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

# Stroop data is in the same file as demographics and questionnaires
stroop_raw <- read_csv("raw_data/demographics_q_dummy_data.csv") %>%
  clean_names() %>%
  drop_na(group) %>%
  select(
    participant_id = x1,
    group,
    stroop_a_rt,
    stroop_a_uncorrected,
    stroop_a_corrected,
    stroop_b_rt,
    stroop_b_uncorrected,
    stroop_b_corrected,
    stroop_c_rt,
    stroop_c_uncorrected,
    stroop_c_corrected,
    stroop_d_rt,
    stroop_d_uncorrected,
    stroop_d_corrected
  ) %>%
  mutate(
    across(c(participant_id, group), as.character),
    across(starts_with("stroop"), as.numeric)
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
stroop_data <- stroop_raw %>%
  filter(!participant_id %in% moca_excluded) %>%
  mutate(group = factor(group, levels = c("PwP", "PwP+ICB", "HC")))

# identify participants who did not complete card C or card D
exc_incomplete <- stroop_data %>%
  filter(is.na(stroop_c_rt) | is.na(stroop_d_rt)) %>%
  transmute(
    participant_id,
    group,
    reason = case_when(
      is.na(stroop_c_rt) & is.na(stroop_d_rt) ~ "did not complete Stroop cards C or D",
      is.na(stroop_c_rt) ~ "did not complete Stroop card C",
      is.na(stroop_d_rt) ~ "did not complete Stroop card D"
    )
  )

# add them to the exclusions df
exclusions <- bind_rows(exclusions, exc_incomplete)

# and remove them from this analysis
stroop_data <- stroop_data %>%
  filter(!participant_id %in% exc_incomplete$participant_id)


#################
# PRE-PROCESSING #
#################

# card A = color patch naming
# card B = word reading
# card C = inhibition (name ink color of words)
# card D = inhibition/switching
# interference effect: card C minus card A
# switch cost: card D minus card C

# calculate interference effect and switch cost
stroop_wide <- stroop_data %>%
  mutate(
    interference_effect = stroop_c_rt - stroop_a_rt,
    switch_cost = stroop_d_rt - stroop_c_rt
  )

# Tukey outlier removal on RT conditions
stroop_tukey <- stroop_wide %>%
  select(participant_id,
         group,
         stroop_a_rt,
         stroop_b_rt,
         stroop_c_rt,
         stroop_d_rt,
         interference_effect,
         switch_cost) %>%
  pivot_longer(
    cols = c(stroop_a_rt,
             stroop_b_rt,
             stroop_c_rt,
             stroop_d_rt,
             interference_effect,
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

exc_tukey <- stroop_tukey %>%
  filter(is_outlier) %>%
  transmute(
    participant_id,
    group,
    reason = paste0("Stroop outlier removed for measure: ", measure,
                    " (Tukey 3×IQR, value: ", round(value, 1), "s)")
  )

exclusions <- bind_rows(exclusions, exc_tukey)

# long format with outliers removed
# used for normality checks, plots, and summary stats
stroop_long <- stroop_tukey %>%
  filter(!is_outlier) %>%
  select(participant_id, group, measure, value)

# give this a one-off name so it doesn't try to join to itself later when we do the log10 transformations
stroop_wide_raw <- stroop_long %>%
  pivot_wider(names_from = measure, values_from = value)


##### Normality checks

normality_plots <- ggplot(stroop_long, aes(value)) +
  geom_histogram() +
  facet_grid(measure ~ group, scales = "free") +
  labs(title = "Histograms of Stroop measures")
normality_plots

normality_summary <- stroop_long %>%
  group_by(group, measure) %>%
  summarise(
    p_value = shapiro.test(value[!is.na(value)])$p.value,
    .groups = "drop"
  )

# log10 transform if any measures fail normality (p < .05)
# it's easier to apply the transformation to everything and then just choose which value to use in the tests later
# the str_detect ensures we don't apply it to interference_effect/switch_cost which may have negative values (and can't be transformed)
# if we need those back in when running on the real data we can change the string to something else
stroop_long <- stroop_long %>%
  mutate(value_log10 = if_else(str_detect(measure, "_rt$"), log10(value), NA_real_))

# re-check normality after transform and if still significant use non-parametric on untransformed data
# interference_effect/switch_cost excluded here as they were not log10 transformed above (see note)
# non-parametric tests should use the original untransformed value for these measures
normality_summary_log10 <- stroop_long %>%
  filter(str_detect(measure, "_rt$")) %>%
  group_by(group, measure) %>%
  summarise(
    p_value = shapiro.test(value_log10[!is.na(value_log10)])$p.value,
    .groups = "drop"
  )
# redo wide with log10 values included
stroop_wide <- stroop_long %>%
  select(group, participant_id, measure, value_log10) %>%
  pivot_wider(names_from = measure, values_from = value_log10, names_prefix = "log10_") %>%
  full_join(stroop_wide_raw, by = c("group", "participant_id"))


######################
# SUMMARY STATISTICS #
######################

stroop_rt_descriptives <- stroop_long %>%
  group_by(group, measure) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value, na.rm = TRUE),
    .groups = "drop"
  )

# summary statistics for errors (not doing statistical tests on these so they're in a separate df)
stroop_error_descriptives <- stroop_data %>%
  pivot_longer(
    cols      = c(stroop_a_uncorrected, stroop_a_corrected,
                  stroop_b_uncorrected, stroop_b_corrected,
                  stroop_c_uncorrected, stroop_c_corrected,
                  stroop_d_uncorrected, stroop_d_corrected),
    names_to  = "measure",
    values_to = "value"
  ) %>%
  group_by(group, measure) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value, na.rm = TRUE),
    .groups = "drop"
  )

# combine all descriptives into one df
stroop_descriptives <- bind_rows(stroop_rt_descriptives,
                                 stroop_error_descriptives)




#########
# PLOTS #
#########

w = 4
h = 5

### Interference effect

p_interference <- ggplot(stroop_wide, aes(x = group, y = interference_effect, fill = group, colour = group)) +
  geom_flat_violin(aes(fill = group), position = position_nudge(x = .3, y = 0), adjust = 1.2, trim = TRUE, alpha = .8, colour = "black", size = .5) +
  geom_point(aes(x = group, y = interference_effect, fill = group, colour = group), position = position_jitter(width = .1), alpha = .7, size = 1) +
  geom_boxplot(aes(x = group, y = interference_effect, fill = group), position = position_nudge(x = c(.22, .22), y = 0), outlier.shape = NA, alpha = .8, width = .1, colour = "black", size = .4) +
  ylab("Interference effect (s)") +
  xlab("") +
  cowplot::theme_cowplot() +
  viridis::scale_colour_viridis(discrete = TRUE, begin = .1, end = .5) +
  viridis::scale_fill_viridis(discrete = TRUE, begin = .1, end = .5) +
  theme(axis.text.x  = element_text(size = 10),
        axis.text.y  = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.position = "none")
p_interference

#ggsave('figs/stroop_interference.png', width = w, height = h)


### Switch cost

p_switch <- ggplot(stroop_wide, aes(x = group, y = switch_cost, fill = group, colour = group)) +
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
p_switch

#ggsave('figs/stroop_switch.png', width = w, height = h)


### Condition RTs (A, B, C, D), faceted

p_rt_by_condition <- ggplot(
  stroop_long %>% filter(measure %in% c("stroop_a_rt", "stroop_b_rt", "stroop_c_rt", "stroop_d_rt")),
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
p_rt_by_condition

#ggsave('figs/stroop_conditions.png', width = w * 2, height = h * 2)


###########################
# INFERENTIAL STATISTICS  #
#  Confirmatory analysis  #
###########################

# subset data for planned pairwise comparisons
pwp_hc_data  <- stroop_wide %>%
  filter(group == "PwP" |
           group == "HC")

pwp_icd_data <- stroop_wide %>%
  filter(group == "PwP" |
           group == "PwP+ICB")

### INTERFERENCE EFFECT
# (here and below, if we need to use non-parametric tests on real data we'll need to edit this section)

# variance tests
pwp_hc_interference_variance  <- var.test(interference_effect ~ group, data = pwp_hc_data) %>%
  broom::tidy()
pwp_icd_interference_variance <- var.test(interference_effect ~ group, data = pwp_icd_data) %>%
  broom::tidy()

# one-way between-subjects ANOVA
interference_aov <- aov(interference_effect ~ group, data = stroop_wide) %>%
  broom::tidy()

# planned independent t-tests
# change 'var.equal' depending on the variance tests above
pwp_hc_interference_ttest  <- t.test(interference_effect ~ group, data = pwp_hc_data, var.equal = TRUE) %>%
  broom::tidy()
pwp_icd_interference_ttest <- t.test(interference_effect ~ group, data = pwp_icd_data, var.equal = TRUE) %>%
  broom::tidy()


###########################
# INFERENTIAL STATISTICS  #
#   Exploratory analysis  #
###########################

### SWITCH COST

# variance tests
pwp_hc_switch_cost_variance  <- var.test(switch_cost ~ group, data = pwp_hc_data) %>%
  broom::tidy()
pwp_icd_switch_cost_variance <- var.test(switch_cost ~ group, data = pwp_icd_data) %>%
  broom::tidy()

# one-way ANOVA
switch_cost_aov <- aov(switch_cost ~ group, data = stroop_wide) %>%
  broom::tidy()

# planned t-tests
# change 'var.equal' depending on the variance tests above
pwp_hc_switch_cost_ttest  <- t.test(switch_cost ~ group, data = pwp_hc_data, var.equal = TRUE) %>%
  broom::tidy()
pwp_icd_switch_cost_ttest <- t.test(switch_cost ~ group, data = pwp_icd_data, var.equal = TRUE) %>%
  broom::tidy()
