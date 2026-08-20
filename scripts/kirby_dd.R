############################################################################################################
# Script for analysis of the Kirby Delay Discounting task according to the pre-registration on the Open    #
# Science Framework                                                                                        #
# Link: https://osf.io/y8drq/files/frzpv                                                                   #
############################################################################################################

####### IMPORTANT INFO #######
# This script can't be run until the kirby_preprocess.R script has been run
# Then, the data needs to be copied into the scorer file from Kaplan et al. (2016)
# Next, the scored data needs to be copied out of the scorer and into a new csv called 'kirby_scored.csv'
# Only then can this script be run


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

cons_threshold <- 0.75 # the Kaplan et al., 2016 Excel sheet highlights them in a different colour, but we need to remove them properly

################
# READ IN DATA #
################

# responses to the 27-item monetary choice questionnaire are scored outside R using the excel-based
# scorer from Kaplan et al. (2016), which produces one row per participant with the k values and
# consistency scores already calculated
kirby_raw <- read_csv("raw_data/kirby_scored.csv") %>%
  clean_names() %>%
  mutate(
    across(c(participant, group), as.character),
    across(c(ends_with("_k"), ends_with("_cons")), as.numeric)
  ) %>%
  filter(!group %in% c("YC", "yc")) %>%
  mutate(group = if_else(group %in% c("PWP", "PwP", "pwp"), "PwP",
                   if_else(group %in% c("ICD", "icd"), "PwP+ICB",
                     if_else(group %in% c("OC", "oc"), "HC", "error")))) %>%
  select(participant,
         group,
         overall_k,
         sml_k,
         med_k,
         lrg_k,
         overall_cons)

# when switching to the real data, swap the read_csv above for the scored export on the network drive. Might need to change column names
# kirby_raw <- read_csv("Z:/Study 2_cross sectional/DATA/kirby_scored.csv") %>%


##############
# EXCLUSIONS #
##############

# empty exclusions table to fill in later
exclusions <- tibble(
  participant = character(),
  group = character(),
  reason = character()
)

# get moca exclusions
moca_exclusions <- read_csv("outputs/moca_exclusions.csv")
moca_excluded <- str_pad(moca_exclusions$participant_id, width = 3, pad = "0") # pad to match kirby's 3 digit style ids

# remove moca exclusions
kirby_data <- kirby_raw %>%
  filter(!participant %in% moca_excluded) %>%
  mutate(group = factor(group, levels = c("PwP", "PwP+ICB", "HC")))

# identify participants whose choices were too inconsistent for their k value to be a reliable estimate
# the excel file (Kaplan et al., 2016) only highlights these in a diff colour rather than removes them directly
exc_consistency <- kirby_data %>%
  filter(overall_cons < cons_threshold) %>%
  transmute(
    participant,
    group,
    reason = paste0("overall consistency below 75% (proportion: ", round(overall_cons, 2), ")")
  )

# add them to the exclusions df
exclusions <- bind_rows(exclusions, exc_consistency)

# and remove them from this analysis
kirby_data <- kirby_data %>%
  filter(!participant %in% exc_consistency$participant)


#################
# PRE-PROCESSING #
#################

# overall_k = the k value with the highest consistency score for that participant
# sml/med/lrg_k = k values for each magnitude of larger delayed reward (small: £25-£35; medium: £50-£60; large: £75-£85)

# long format
# used for normality checks, plots, and summary stats
kirby_long <- kirby_data %>%
  select(participant, group, overall_k, sml_k, med_k, lrg_k) %>%
  pivot_longer(
    cols = c(overall_k,
             sml_k,
             med_k,
             lrg_k),
    names_to = "measure",
    values_to = "value"
  )

# give this a one-off name so it doesn't try to join to itself later when we do the log10 transformations
kirby_wide_raw <- kirby_long %>%
  pivot_wider(names_from = measure, values_from = value)


##### Normality checks

normality_plots <- ggplot(kirby_long, aes(value)) +
  geom_histogram() +
  facet_grid(measure ~ group, scales = "free") +
  labs(title = "Histograms of Kirby k values")
normality_plots

normality_summary <- kirby_long %>%
  group_by(group, measure) %>%
  summarise(
    p_value = shapiro.test(value[!is.na(value)])$p.value,
    .groups = "drop"
  )

# log10 transform if any measures fail normality (p < .05)
# it's easier to apply the transformation to everything and then just choose which value to use in the tests later
# k values are always positive so they can all be transformed
kirby_long <- kirby_long %>%
  mutate(value_log10 = log10(value))

# re-check normality after transform and if still significant use non-parametric on untransformed data
normality_summary_log10 <- kirby_long %>%
  group_by(group, measure) %>%
  summarise(
    p_value = shapiro.test(value_log10[!is.na(value_log10)])$p.value,
    .groups = "drop"
  )

# redo wide with log10 values included
kirby_wide <- kirby_long %>%
  select(group, participant, measure, value_log10) %>%
  pivot_wider(names_from = measure, values_from = value_log10, names_prefix = "log10_") %>%
  full_join(kirby_wide_raw, by = c("group", "participant"))


######################
# SUMMARY STATISTICS #
######################

kirby_k_descriptives <- kirby_long %>%
  group_by(group, measure) %>%
  summarise(
    mean   = mean(value, na.rm = TRUE),
    sd     = sd(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE), # medians more representative than means for skewed k values
    iqr    = IQR(value, na.rm = TRUE),
    .groups = "drop"
  )

# summary statistics for consistency score (not doing statistical tests on these so they're in a separate df)
kirby_cons_descriptives <- kirby_data %>%
  pivot_longer(
    cols      = c(overall_cons),
    names_to  = "measure",
    values_to = "value"
  ) %>%
  group_by(group, measure) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value, na.rm = TRUE),
    min  = min(value, na.rm = TRUE),
    max  = max(value, na.rm = TRUE),
    .groups = "drop"
  )

# # combine all descriptives into one df
# kirby_descriptives <- bind_rows(kirby_k_descriptives,
#                                 kirby_cons_descriptives)


#########
# PLOTS #
#########

w = 4
h = 5

### Overall k (k value with the highest consistency score)

p_overall_k <- ggplot(kirby_wide, aes(x = group, y = overall_k, fill = group, colour = group)) +
  geom_flat_violin(aes(fill = group), position = position_nudge(x = .3, y = 0), adjust = 1.2, trim = TRUE, alpha = .8, colour = "black", size = .5) +
  geom_point(aes(x = group, y = overall_k, fill = group, colour = group), position = position_jitter(width = .1), alpha = .7, size = 1) +
  geom_boxplot(aes(x = group, y = overall_k, fill = group), position = position_nudge(x = c(.22, .22), y = 0), outlier.shape = NA, alpha = .8, width = .1, colour = "black", size = .4) +
  ylab("Delay discounting index (k)") +
  xlab("") +
  cowplot::theme_cowplot() +
  viridis::scale_colour_viridis(discrete = TRUE, begin = .1, end = .5) +
  viridis::scale_fill_viridis(discrete = TRUE, begin = .1, end = .5) +
  theme(axis.text.x  = element_text(size = 10),
        axis.text.y  = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.position = "none")
p_overall_k

#ggsave('figs/kirby_overall_k.png', width = w, height = h)


### Magnitude k values (small: £25-£35; medium: £50-£60; large: £75-£85), faceted

p_magnitude_k <- ggplot(
  kirby_long %>%
    filter(measure %in% c("sml_k", "med_k", "lrg_k")) %>%
    mutate(measure = factor(measure, levels = c("sml_k", "med_k", "lrg_k"))), # facets in size order rather than alphabetical
  aes(x = group, y = value, fill = group, colour = group)) +
  geom_flat_violin(aes(fill = group), position = position_nudge(x = .3, y = 0), adjust = 1.2, trim = TRUE, alpha = .8, colour = "black", size = .5) +
  geom_point(aes(x = group, y = value, fill = group, colour = group), position = position_jitter(width = .1), alpha = .7, size = 1) +
  geom_boxplot(aes(x = group, y = value, fill = group), position = position_nudge(x = c(.22, .22), y = 0), outlier.shape = NA, alpha = .8, width = .1, colour = "black", size = .4) +
  facet_wrap(~ measure, scales = "free_y") +
  ylab("Delay discounting index (k)") +
  xlab("") +
  cowplot::theme_cowplot() +
  viridis::scale_colour_viridis(discrete = TRUE, begin = .1, end = .5) +
  viridis::scale_fill_viridis(discrete = TRUE, begin = .1, end = .5) +
  theme(axis.text.x  = element_text(size = 10),
        axis.text.y  = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.position = "none")
p_magnitude_k

#ggsave('figs/kirby_magnitude_k.png', width = w * 3, height = h)


###########################
# INFERENTIAL STATISTICS  #
#  Confirmatory analysis  #
###########################

# subset data for planned pairwise comparisons
pwp_hc_data  <- kirby_wide %>%
  filter(group == "PwP" |
           group == "HC")

pwp_icd_data <- kirby_wide %>%
  filter(group == "PwP" |
           group == "PwP+ICB")

### OVERALL K (k value with the highest consistency score)
# the pre-reg expects k to be normally distributed
# but on reflection we should have expected it to be non-normal
# so have put the non-parametric tests in as default
# we can always change back to parametric if the normality tests/transformation surprises us when we run against the real data


# # variance tests
# pwp_hc_overall_k_variance  <- var.test(overall_k ~ group, data = pwp_hc_data) %>%
#   broom::tidy()
# pwp_icd_overall_k_variance <- var.test(overall_k ~ group, data = pwp_icd_data) %>%
#   broom::tidy()

# # one-way between-subjects ANOVA
# overall_k_aov <- aov(overall_k ~ group, data = kirby_wide) %>%
#   broom::tidy()
# 
# # planned independent t-tests
# # change 'var.equal' depending on the variance tests above
# pwp_hc_overall_k_ttest  <- t.test(overall_k ~ group, data = pwp_hc_data, var.equal = TRUE) %>%
#   broom::tidy()
# pwp_icd_overall_k_ttest <- t.test(overall_k ~ group, data = pwp_icd_data, var.equal = TRUE) %>%
#   broom::tidy()

# kruskal-wallis (non-parametric route)
overall_k_kw <- kruskal.test(overall_k ~ group, data = kirby_wide) %>%
  broom::tidy()

# mann-whitney u tests (named as wilcoxon in R, but this is independent samples version)
pwp_hc_overall_k_mwu  <- wilcox.test(overall_k ~ group, data = pwp_hc_data, alternative = "two.sided", conf.int = TRUE) %>%
  broom::tidy()
pwp_icd_overall_k_mwu <- wilcox.test(overall_k ~ group, data = pwp_icd_data, alternative = "two.sided", conf.int = TRUE) %>%
  broom::tidy()


###########################
# INFERENTIAL STATISTICS  #
#   Exploratory analysis  #
###########################

# k analysed separately for each magnitude of larger delayed reward

### SMALL REWARD K
# 
# # variance tests
# pwp_hc_sml_k_variance  <- var.test(sml_k ~ group, data = pwp_hc_data) %>%
#   broom::tidy()
# pwp_icd_sml_k_variance <- var.test(sml_k ~ group, data = pwp_icd_data) %>%
#   broom::tidy()
# 
# # one-way ANOVA
# sml_k_aov <- aov(sml_k ~ group, data = kirby_wide) %>%
#   broom::tidy()
# 
# # planned t-tests
# # change 'var.equal' depending on the variance tests above
# pwp_hc_sml_k_ttest  <- t.test(sml_k ~ group, data = pwp_hc_data, var.equal = TRUE) %>%
#   broom::tidy()
# pwp_icd_sml_k_ttest <- t.test(sml_k ~ group, data = pwp_icd_data, var.equal = TRUE) %>%
#   broom::tidy()

# kruskal-wallis
sml_k_kw <- kruskal.test(sml_k ~ group, data = kirby_wide) %>%
  broom::tidy()

# mann-whitney u tests (named as wilcoxon in R, but this is independent samples version)
pwp_hc_sml_k_mwu  <- wilcox.test(sml_k ~ group, data = pwp_hc_data, alternative = "two.sided", conf.int = TRUE) %>%
  broom::tidy()
pwp_icd_sml_k_mwu <- wilcox.test(sml_k ~ group, data = pwp_icd_data, alternative = "two.sided", conf.int = TRUE) %>%
  broom::tidy()


### MEDIUM REWARD K

# # variance tests
# pwp_hc_med_k_variance  <- var.test(med_k ~ group, data = pwp_hc_data) %>%
#   broom::tidy()
# pwp_icd_med_k_variance <- var.test(med_k ~ group, data = pwp_icd_data) %>%
#   broom::tidy()
# 
# # one-way ANOVA
# med_k_aov <- aov(med_k ~ group, data = kirby_wide) %>%
#   broom::tidy()
# 
# # planned t-tests
# # change 'var.equal' depending on the variance tests above
# pwp_hc_med_k_ttest  <- t.test(med_k ~ group, data = pwp_hc_data, var.equal = TRUE) %>%
#   broom::tidy()
# pwp_icd_med_k_ttest <- t.test(med_k ~ group, data = pwp_icd_data, var.equal = TRUE) %>%
#   broom::tidy()

# kruskal-wallis
med_k_kw <- kruskal.test(med_k ~ group, data = kirby_wide) %>%
  broom::tidy()

# mann-whitney u tests (named as wilcoxon in R, but this is independent samples version)
pwp_hc_med_k_mwu  <- wilcox.test(med_k ~ group, data = pwp_hc_data, alternative = "two.sided", conf.int = TRUE) %>%
  broom::tidy()
pwp_icd_med_k_mwu <- wilcox.test(med_k ~ group, data = pwp_icd_data, alternative = "two.sided", conf.int = TRUE) %>%
  broom::tidy()


### LARGE REWARD K

# # variance tests
# pwp_hc_lrg_k_variance  <- var.test(lrg_k ~ group, data = pwp_hc_data) %>%
#   broom::tidy()
# pwp_icd_lrg_k_variance <- var.test(lrg_k ~ group, data = pwp_icd_data) %>%
#   broom::tidy()
# 
# # one-way ANOVA
# lrg_k_aov <- aov(lrg_k ~ group, data = kirby_wide) %>%
#   broom::tidy()
# 
# # planned t-tests
# # change 'var.equal' depending on the variance tests above
# pwp_hc_lrg_k_ttest  <- t.test(lrg_k ~ group, data = pwp_hc_data, var.equal = TRUE) %>%
#   broom::tidy()
# pwp_icd_lrg_k_ttest <- t.test(lrg_k ~ group, data = pwp_icd_data, var.equal = TRUE) %>%
#   broom::tidy()

# kruskal-wallis
lrg_k_kw <- kruskal.test(lrg_k ~ group, data = kirby_wide) %>%
  broom::tidy()

# mann-whitney u tests (named as wilcoxon in R, but this is independent samples version)
pwp_hc_lrg_k_mwu  <- wilcox.test(lrg_k ~ group, data = pwp_hc_data, alternative = "two.sided", conf.int = TRUE) %>%
  broom::tidy()
pwp_icd_lrg_k_mwu <- wilcox.test(lrg_k ~ group, data = pwp_icd_data, alternative = "two.sided", conf.int = TRUE) %>%
  broom::tidy()
