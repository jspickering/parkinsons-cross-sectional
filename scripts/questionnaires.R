#############################################################################################################
# Script for analysis of questionnaire data according to the pre-registration on the Open Science Framework #                                         #
# Link: https://osf.io/y8drq/files/frzpv                                                                    #
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

questionnaires_raw <- read_csv("raw_data/demographics_q_dummy_data.csv") %>%
  clean_names() %>%
  drop_na(group) %>%
  select(
    participant_id = x1,
    group,
    barratt,
    barratt_a,
    barratt_m,
    barratt_n,
    bisbas_bis,
    bisbas_drive,
    bisbas_fun,
    bisbas_reward,
    upps_sensation,
    upps_neg,
    upps_premed,
    upps_pers,
    upps_pos
  ) %>%
  mutate(
    across(c(participant_id,
             group),
           as.character),
    across(c(barratt, barratt_a, barratt_m, barratt_n,
             bisbas_bis, bisbas_drive, bisbas_fun, bisbas_reward,
             upps_sensation, upps_neg, upps_premed, upps_pers, upps_pos),
           as.numeric)
  ) %>%
  filter(group != "YC") %>% # for MMs proj
  mutate(group = if_else(group == "PwP", "PwP",
                   if_else(group == "ICD", "PwP+ICB",
                     if_else(group == "OC", "HC", "error")))) %>%
  mutate(bas_total = bisbas_drive + bisbas_fun + bisbas_reward)


##############
# EXCLUSIONS #
##############

# read in MoCA exclusions saved by demographics.R
moca_excluded <- read_csv("outputs/moca_exclusions.csv") %>%
  pull(participant_id) %>%
  as.character()

questionnaires <- questionnaires_raw %>%
  filter(!participant_id %in% moca_excluded) %>%
  mutate(group = factor(group, levels = c("PwP", "PwP+ICB", "HC")))


######################
# SUMMARY STATISTICS #
######################

questionnaire_summaries <- questionnaires %>%
  group_by(group) %>%
  summarise(
    mean_barratt = mean(barratt, na.rm = TRUE), sd_barratt = sd(barratt, na.rm = TRUE),
    mean_barratt_a = mean(barratt_a, na.rm = TRUE), sd_barratt_a = sd(barratt_a, na.rm = TRUE),
    mean_barratt_m = mean(barratt_m, na.rm = TRUE), sd_barratt_m = sd(barratt_m, na.rm = TRUE),
    mean_barratt_n = mean(barratt_n, na.rm = TRUE), sd_barratt_n = sd(barratt_n, na.rm = TRUE),
    mean_bis = mean(bisbas_bis, na.rm = TRUE), sd_bis = sd(bisbas_bis, na.rm = TRUE),
    mean_bas = mean(bas_total, na.rm = TRUE), sd_bas = sd(bas_total, na.rm = TRUE),
    mean_bas_drive = mean(bisbas_drive, na.rm = TRUE), sd_bas_drive = sd(bisbas_drive, na.rm = TRUE),
    mean_bas_fun = mean(bisbas_fun, na.rm = TRUE), sd_bas_fun = sd(bisbas_fun, na.rm = TRUE),
    mean_bas_reward = mean(bisbas_reward, na.rm = TRUE), sd_bas_reward = sd(bisbas_reward, na.rm = TRUE),
    mean_upps_ss = mean(upps_sensation, na.rm = TRUE),sd_upps_ss = sd(upps_sensation, na.rm = TRUE),
    mean_upps_neg = mean(upps_neg, na.rm = TRUE), sd_upps_neg = sd(upps_neg, na.rm = TRUE),
    mean_upps_pre = mean(upps_premed, na.rm = TRUE), sd_upps_pre = sd(upps_premed, na.rm = TRUE),
    mean_upps_pers = mean(upps_pers, na.rm = TRUE), sd_upps_pers = sd(upps_pers, na.rm = TRUE),
    mean_upps_pos = mean(upps_pos, na.rm = TRUE), sd_upps_pos = sd(upps_pos, na.rm = TRUE),
    .groups = "drop"
  )


####################
# NORMALITY CHECKS #
####################

questionnaires_long <- questionnaires %>%
  pivot_longer(
    cols      = c(barratt, barratt_a, barratt_m, barratt_n,
                  bisbas_bis, bas_total, bisbas_drive, bisbas_fun, bisbas_reward,
                  upps_sensation, upps_neg, upps_premed, upps_pers, upps_pos),
    names_to  = "measure",
    values_to = "value"
  )

normality_plots <- ggplot(questionnaires_long, aes(value)) +
  geom_histogram() +
  facet_grid(measure ~ group, scales = "free") +
  labs(title = "Histograms of questionnaire measures")
normality_plots

normality_summary <- questionnaires_long %>%
  filter(!is.na(value)) %>%
  group_by(group, measure) %>%
  summarise(
    p_value = shapiro.test(value)$p.value,
    .groups = "drop"
  )

# log10 transform if any measures fail normality (p < .05)
questionnaires_long <- questionnaires_long %>%
  mutate(value_log10 = log10(value))

# re-check normality after transform; if still significant use non-parametric tests on the untransformed data
normality_summary_log10 <- questionnaires_long %>%
  filter(!is.na(value_log10)) %>%
  group_by(group, measure) %>%
  summarise(
    p_value = shapiro.test(value_log10)$p.value,
    .groups = "drop"
  )

# wide version with the log10 values alongside the originals
# use the log10_ columns for the parametric tests on any measure that fails raw normality but passes after transform
questionnaires_wide <- questionnaires_long %>%
  select(group, participant_id, measure, value_log10) %>%
  pivot_wider(names_from = measure, values_from = value_log10, names_prefix = "log10_") %>%
  full_join(questionnaires, by = c("group", "participant_id"))


###########################
# INFERENTIAL STATISTICS  # 
###########################

# pairwise subsets for planned comparisons
pwp_hc_data  <- questionnaires %>%
  filter(group == "PwP"|
           group == "HC")

pwp_icb_data <- questionnaires %>%
  filter(group == "PwP"|
           group == "PwP+ICB")

###########
# BARRATT # 
###########

# total score

barratt_kw           <- kruskal.test(barratt ~ group, data = questionnaires) %>% broom::tidy()
pwp_hc_barratt_mwu   <- wilcox.test(barratt ~ group, data = pwp_hc_data, alternative = "two.sided", conf.int = TRUE) %>% broom::tidy()
pwp_icb_barratt_mwu  <- wilcox.test(barratt ~ group, data = pwp_icb_data, alternative = "two.sided", conf.int = TRUE) %>% broom::tidy()

barratt_aov          <- aov(barratt ~ group, data = questionnaires) %>% broom::tidy()
pwp_hc_barratt_var   <- var.test(barratt ~ group, data = pwp_hc_data) %>% broom::tidy()
pwp_icb_barratt_var  <- var.test(barratt ~ group, data = pwp_icb_data) %>% broom::tidy()
pwp_hc_barratt_tt    <- t.test(barratt ~ group, data = pwp_hc_data, var.equal = TRUE) %>% broom::tidy()
pwp_icb_barratt_tt   <- t.test(barratt ~ group, data = pwp_icb_data, var.equal = TRUE) %>% broom::tidy()

# attentional

barratt_a_kw         <- kruskal.test(barratt_a ~ group, data = questionnaires) %>% broom::tidy()
pwp_hc_ba_mwu        <- wilcox.test(barratt_a ~ group, data = pwp_hc_data, alternative = "two.sided", conf.int = TRUE) %>% broom::tidy()
pwp_icb_ba_mwu       <- wilcox.test(barratt_a ~ group, data = pwp_icb_data, alternative = "two.sided", conf.int = TRUE) %>% broom::tidy()

barratt_a_aov        <- aov(barratt_a ~ group, data = questionnaires) %>% broom::tidy()
pwp_hc_ba_var        <- var.test(barratt_a ~ group, data = pwp_hc_data) %>% broom::tidy()
pwp_icb_ba_var       <- var.test(barratt_a ~ group, data = pwp_icb_data) %>% broom::tidy()
pwp_hc_ba_tt         <- t.test(barratt_a ~ group, data = pwp_hc_data, var.equal = TRUE) %>% broom::tidy()
pwp_icb_ba_tt        <- t.test(barratt_a ~ group, data = pwp_icb_data, var.equal = TRUE) %>% broom::tidy()

# motor

barratt_m_kw         <- kruskal.test(barratt_m ~ group, data = questionnaires) %>% broom::tidy()
pwp_hc_bm_mwu        <- wilcox.test(barratt_m ~ group, data = pwp_hc_data, alternative = "two.sided", conf.int = TRUE) %>% broom::tidy()
pwp_icb_bm_mwu       <- wilcox.test(barratt_m ~ group, data = pwp_icb_data, alternative = "two.sided", conf.int = TRUE) %>% broom::tidy()

barratt_m_aov        <- aov(barratt_m ~ group, data = questionnaires) %>% broom::tidy()
pwp_hc_bm_var        <- var.test(barratt_m ~ group, data = pwp_hc_data) %>% broom::tidy()
pwp_icb_bm_var       <- var.test(barratt_m ~ group, data = pwp_icb_data) %>% broom::tidy()
pwp_hc_bm_tt         <- t.test(barratt_m ~ group, data = pwp_hc_data, var.equal = TRUE) %>% broom::tidy()
pwp_icb_bm_tt        <- t.test(barratt_m ~ group, data = pwp_icb_data, var.equal = TRUE) %>% broom::tidy()

# non-planning

barratt_n_kw         <- kruskal.test(barratt_n ~ group, data = questionnaires) %>% broom::tidy()
pwp_hc_bn_mwu        <- wilcox.test(barratt_n ~ group, data = pwp_hc_data, alternative = "two.sided", conf.int = TRUE) %>% broom::tidy()
pwp_icb_bn_mwu       <- wilcox.test(barratt_n ~ group, data = pwp_icb_data, alternative = "two.sided", conf.int = TRUE) %>% broom::tidy()

barratt_n_aov        <- aov(barratt_n ~ group, data = questionnaires) %>% broom::tidy()
pwp_hc_bn_var        <- var.test(barratt_n ~ group, data = pwp_hc_data) %>% broom::tidy()
pwp_icb_bn_var       <- var.test(barratt_n ~ group, data = pwp_icb_data) %>% broom::tidy()
pwp_hc_bn_tt         <- t.test(barratt_n ~ group, data = pwp_hc_data, var.equal = TRUE) %>% broom::tidy()
pwp_icb_bn_tt        <- t.test(barratt_n ~ group, data = pwp_icb_data, var.equal = TRUE) %>% broom::tidy()


###########
# BIS/BAS # 
###########

# overall BIS score

bis_kw               <- kruskal.test(bisbas_bis ~ group, data = questionnaires) %>% broom::tidy()
pwp_hc_bis_mwu       <- wilcox.test(bisbas_bis ~ group, data = pwp_hc_data, alternative = "two.sided", conf.int = TRUE) %>% broom::tidy()
pwp_icb_bis_mwu      <- wilcox.test(bisbas_bis ~ group, data = pwp_icb_data, alternative = "two.sided", conf.int = TRUE) %>% broom::tidy()

bis_aov              <- aov(bisbas_bis ~ group, data = questionnaires) %>% broom::tidy()
pwp_hc_bis_var       <- var.test(bisbas_bis ~ group, data = pwp_hc_data) %>% broom::tidy()
pwp_icb_bis_var      <- var.test(bisbas_bis ~ group, data = pwp_icb_data) %>% broom::tidy()
pwp_hc_bis_tt        <- t.test(bisbas_bis ~ group, data = pwp_hc_data, var.equal = TRUE) %>% broom::tidy()
pwp_icb_bis_tt       <- t.test(bisbas_bis ~ group, data = pwp_icb_data, var.equal = TRUE) %>% broom::tidy()

# overall BAS score

bas_kw               <- kruskal.test(bas_total ~ group, data = questionnaires) %>% broom::tidy()
pwp_hc_bas_mwu       <- wilcox.test(bas_total ~ group, data = pwp_hc_data, alternative = "two.sided", conf.int = TRUE) %>% broom::tidy()
pwp_icb_bas_mwu      <- wilcox.test(bas_total ~ group, data = pwp_icb_data, alternative = "two.sided", conf.int = TRUE) %>% broom::tidy()

bas_aov              <- aov(bas_total ~ group, data = questionnaires) %>% broom::tidy()
pwp_hc_bas_var       <- var.test(bas_total ~ group, data = pwp_hc_data) %>% broom::tidy()
pwp_icb_bas_var      <- var.test(bas_total ~ group, data = pwp_icb_data) %>% broom::tidy()
pwp_hc_bas_tt        <- t.test(bas_total ~ group, data = pwp_hc_data, var.equal = TRUE) %>% broom::tidy()
pwp_icb_bas_tt       <- t.test(bas_total ~ group, data = pwp_icb_data, var.equal = TRUE) %>% broom::tidy()


# drive

bas_drive_kw         <- kruskal.test(bisbas_drive ~ group, data = questionnaires) %>% broom::tidy()
pwp_hc_drive_mwu     <- wilcox.test(bisbas_drive ~ group, data = pwp_hc_data, alternative = "two.sided", conf.int = TRUE) %>% broom::tidy()
pwp_icb_drive_mwu    <- wilcox.test(bisbas_drive ~ group, data = pwp_icb_data, alternative = "two.sided", conf.int = TRUE) %>% broom::tidy()

bas_drive_aov        <- aov(bisbas_drive ~ group, data = questionnaires) %>% broom::tidy()
pwp_hc_drive_var     <- var.test(bisbas_drive ~ group, data = pwp_hc_data) %>% broom::tidy()
pwp_icb_drive_var    <- var.test(bisbas_drive ~ group, data = pwp_icb_data) %>% broom::tidy()
pwp_hc_drive_tt      <- t.test(bisbas_drive ~ group, data = pwp_hc_data, var.equal = TRUE) %>% broom::tidy()
pwp_icb_drive_tt     <- t.test(bisbas_drive ~ group, data = pwp_icb_data, var.equal = TRUE) %>% broom::tidy()


# fun seeking

bas_fun_kw           <- kruskal.test(bisbas_fun ~ group, data = questionnaires) %>% broom::tidy()
pwp_hc_fun_mwu       <- wilcox.test(bisbas_fun ~ group, data = pwp_hc_data, alternative = "two.sided", conf.int = TRUE) %>% broom::tidy()
pwp_icb_fun_mwu      <- wilcox.test(bisbas_fun ~ group, data = pwp_icb_data, alternative = "two.sided", conf.int = TRUE) %>% broom::tidy()

bas_fun_aov          <- aov(bisbas_fun ~ group, data = questionnaires) %>% broom::tidy()
pwp_hc_fun_var       <- var.test(bisbas_fun ~ group, data = pwp_hc_data) %>% broom::tidy()
pwp_icb_fun_var      <- var.test(bisbas_fun ~ group, data = pwp_icb_data) %>% broom::tidy()
pwp_hc_fun_tt        <- t.test(bisbas_fun ~ group, data = pwp_hc_data, var.equal = TRUE) %>% broom::tidy()
pwp_icb_fun_tt       <- t.test(bisbas_fun ~ group, data = pwp_icb_data, var.equal = TRUE) %>% broom::tidy()

# reward responsiveness

bas_reward_kw        <- kruskal.test(bisbas_reward ~ group, data = questionnaires) %>% broom::tidy()
pwp_hc_reward_mwu    <- wilcox.test(bisbas_reward ~ group, data = pwp_hc_data, alternative = "two.sided", conf.int = TRUE) %>% broom::tidy()
pwp_icb_reward_mwu   <- wilcox.test(bisbas_reward ~ group, data = pwp_icb_data, alternative = "two.sided", conf.int = TRUE) %>% broom::tidy()

bas_reward_aov       <- aov(bisbas_reward ~ group, data = questionnaires) %>% broom::tidy()
pwp_hc_reward_var    <- var.test(bisbas_reward ~ group, data = pwp_hc_data) %>% broom::tidy()
pwp_icb_reward_var   <- var.test(bisbas_reward ~ group, data = pwp_icb_data) %>% broom::tidy()
pwp_hc_reward_tt     <- t.test(bisbas_reward ~ group, data = pwp_hc_data, var.equal = TRUE) %>% broom::tidy()
pwp_icb_reward_tt    <- t.test(bisbas_reward ~ group, data = pwp_icb_data, var.equal = TRUE) %>% broom::tidy()

##########
# UPPS-P # 
##########

# sensation seeking

upps_ss_kw           <- kruskal.test(upps_sensation ~ group, data = questionnaires) %>% broom::tidy()
pwp_hc_upps_ss_mwu   <- wilcox.test(upps_sensation ~ group, data = pwp_hc_data, alternative = "two.sided", conf.int = TRUE) %>% broom::tidy()
pwp_icb_upps_ss_mwu  <- wilcox.test(upps_sensation ~ group, data = pwp_icb_data, alternative = "two.sided", conf.int = TRUE) %>% broom::tidy()

upps_ss_aov          <- aov(upps_sensation ~ group, data = questionnaires) %>% broom::tidy()
pwp_hc_upps_ss_var   <- var.test(upps_sensation ~ group, data = pwp_hc_data) %>% broom::tidy()
pwp_icb_upps_ss_var  <- var.test(upps_sensation ~ group, data = pwp_icb_data) %>% broom::tidy()
pwp_hc_upps_ss_tt    <- t.test(upps_sensation ~ group, data = pwp_hc_data, var.equal = TRUE) %>% broom::tidy()
pwp_icb_upps_ss_tt   <- t.test(upps_sensation ~ group, data = pwp_icb_data, var.equal = TRUE) %>% broom::tidy()

# neg urgency

upps_neg_kw          <- kruskal.test(upps_neg ~ group, data = questionnaires) %>% broom::tidy()
pwp_hc_neg_mwu       <- wilcox.test(upps_neg ~ group, data = pwp_hc_data, alternative = "two.sided", conf.int = TRUE) %>% broom::tidy()
pwp_icb_neg_mwu      <- wilcox.test(upps_neg ~ group, data = pwp_icb_data, alternative = "two.sided", conf.int = TRUE) %>% broom::tidy()

upps_neg_aov         <- aov(upps_neg ~ group, data = questionnaires) %>% broom::tidy()
pwp_hc_neg_var       <- var.test(upps_neg ~ group, data = pwp_hc_data) %>% broom::tidy()
pwp_icb_neg_var      <- var.test(upps_neg ~ group, data = pwp_icb_data) %>% broom::tidy()
pwp_hc_neg_tt        <- t.test(upps_neg ~ group, data = pwp_hc_data, var.equal = TRUE) %>% broom::tidy()
pwp_icb_neg_tt       <- t.test(upps_neg ~ group, data = pwp_icb_data, var.equal = TRUE) %>% broom::tidy()

# premeditation

upps_premed_kw       <- kruskal.test(upps_premed ~ group, data = questionnaires) %>% broom::tidy()
pwp_hc_premed_mwu    <- wilcox.test(upps_premed ~ group, data = pwp_hc_data, alternative = "two.sided", conf.int = TRUE) %>% broom::tidy()
pwp_icb_premed_mwu   <- wilcox.test(upps_premed ~ group, data = pwp_icb_data, alternative = "two.sided", conf.int = TRUE) %>% broom::tidy()

upps_premed_aov      <- aov(upps_premed ~ group, data = questionnaires) %>% broom::tidy()
pwp_hc_premed_var    <- var.test(upps_premed ~ group, data = pwp_hc_data) %>% broom::tidy()
pwp_icb_premed_var   <- var.test(upps_premed ~ group, data = pwp_icb_data) %>% broom::tidy()
pwp_hc_premed_tt     <- t.test(upps_premed ~ group, data = pwp_hc_data, var.equal = TRUE) %>% broom::tidy()
pwp_icb_premed_tt    <- t.test(upps_premed ~ group, data = pwp_icb_data, var.equal = TRUE) %>% broom::tidy()

# perseverance

upps_pers_kw         <- kruskal.test(upps_pers ~ group, data = questionnaires) %>% broom::tidy()
pwp_hc_pers_mwu      <- wilcox.test(upps_pers ~ group, data = pwp_hc_data, alternative = "two.sided", conf.int = TRUE) %>% broom::tidy()
pwp_icb_pers_mwu     <- wilcox.test(upps_pers ~ group, data = pwp_icb_data, alternative = "two.sided", conf.int = TRUE) %>% broom::tidy()

upps_pers_aov        <- aov(upps_pers ~ group, data = questionnaires) %>% broom::tidy()
pwp_hc_pers_var      <- var.test(upps_pers ~ group, data = pwp_hc_data) %>% broom::tidy()
pwp_icb_pers_var     <- var.test(upps_pers ~ group, data = pwp_icb_data) %>% broom::tidy()
pwp_hc_pers_tt       <- t.test(upps_pers ~ group, data = pwp_hc_data, var.equal = TRUE) %>% broom::tidy()
pwp_icb_pers_tt      <- t.test(upps_pers ~ group, data = pwp_icb_data, var.equal = TRUE) %>% broom::tidy()

# pos urgency

upps_pos_kw          <- kruskal.test(upps_pos ~ group, data = questionnaires) %>% broom::tidy()
pwp_hc_pos_mwu       <- wilcox.test(upps_pos ~ group, data = pwp_hc_data, alternative = "two.sided", conf.int = TRUE) %>% broom::tidy()
pwp_icb_pos_mwu      <- wilcox.test(upps_pos ~ group, data = pwp_icb_data, alternative = "two.sided", conf.int = TRUE) %>% broom::tidy()

upps_pos_aov         <- aov(upps_pos ~ group, data = questionnaires) %>% broom::tidy()
pwp_hc_pos_var       <- var.test(upps_pos ~ group, data = pwp_hc_data) %>% broom::tidy()
pwp_icb_pos_var      <- var.test(upps_pos ~ group, data = pwp_icb_data) %>% broom::tidy()
pwp_hc_pos_tt        <- t.test(upps_pos ~ group, data = pwp_hc_data, var.equal = TRUE) %>% broom::tidy()
pwp_icb_pos_tt       <- t.test(upps_pos ~ group, data = pwp_icb_data, var.equal = TRUE) %>% broom::tidy()
