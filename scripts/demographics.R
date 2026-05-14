############################################################################################################
# Script for analysis of demographic data according to the pre-registration on the Open Science Framework  #
# Link:                                                                                                    #
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

################
# READ IN DATA #
################

### read in and tidy data
demographics_raw <- read_csv("raw_data/demographics_q_dummy_data.csv") %>%
  # put variables names in snake_case
  clean_names() %>%
  # remove any rows without a participant
  drop_na(group) %>%
  # keep only columns needed for demographics; Stroop, TMT, and questionnaires handled in separate scripts
  select(participant_id = x1,
    group,
    age,
    gender,
    mo_ca,
    gds,
    quip_current_pos,
    quip_anytime_pos,
    quip_rs_comb,
    updrs_iii,
    h_y,
    td_pigd,
    parkinsons_duration_months,
    icb_gambling = ic_dc_gambling,      # rename here and below
    icb_sexual_behaviour = ic_dc_sexual_behaviour,
    icb_buying = ic_dc_buying,
    icb_eating = ic_dc_eating,
    icb_hobbism = ic_dc_hobbism,
    icb_punding = ic_dc_punding,
    icb_walkabout = ic_dc_walkabout,
    icb_medication = ic_dc_medication,
    ledd_levodopa_da,
    ledd_da_only
  ) %>%
  # convert data types for all columns to make sure they're correct
  mutate(
    across(c(participant_id,
             group,
             gender,
             td_pigd,
             icb_gambling,
             icb_sexual_behaviour,
             icb_buying,
             icb_eating,
             icb_hobbism,
             icb_punding,
             icb_walkabout,
             icb_medication
    ), as.character),
    across(c(age,
             mo_ca,
             quip_current_pos,
             quip_anytime_pos,
             quip_rs_comb,
             gds,
             updrs_iii,
             h_y,
             parkinsons_duration_months,
             ledd_levodopa_da,
             ledd_da_only
    ), as.numeric)
  ) %>%
  # remove YC participants as this was collected for MM's project
  subset(group != "YC") %>%
  # recode group labels
  mutate(group = if_else(group %in% c("PWP", "PwP"), "PwP",
                   if_else(group == "ICD", "PwP+ICB",
                     if_else(group == "OC", "HC", "error"))))


##############
# EXCLUSIONS #
##############

# empty exclusions table to fill in
exclusions <- tibble(
  participant_id = character(),
  group = character(),
  reason = character()
)

# MoCA <26 means they should be excluded from analysis across the whole study
exc_moca <- demographics_raw %>%
  filter(mo_ca <= 25) %>%
  transmute(
    participant_id,
    group,
    reason = "MoCA < 26"
  )

exclusions <- bind_rows(exclusions, exc_moca)

moca_excluded <- exc_moca$participant_id # participant IDs for other scripts to use

# apply MoCA exclusions
demographics <- demographics_raw %>%
  filter(!participant_id %in% moca_excluded) %>%
  mutate(group = factor(group, levels = c("PwP", "PwP+ICB", "HC")))


######################
# SUMMARY STATISTICS #
######################

# descriptives (all participants)
demographic_summaries <- demographics %>%
  group_by(group) %>%
  summarise(
    n_ppts            = n(),
    mean_age          = mean(age, na.rm = TRUE),           sd_age          = sd(age, na.rm = TRUE),
    min_age           = min(age, na.rm = TRUE),            max_age         = max(age, na.rm = TRUE),
    mean_moca         = mean(mo_ca, na.rm = TRUE),         sd_moca         = sd(mo_ca, na.rm = TRUE),
    min_moca          = min(mo_ca, na.rm = TRUE),          max_moca        = max(mo_ca, na.rm = TRUE),
    mean_gds          = mean(gds, na.rm = TRUE),           sd_gds          = sd(gds, na.rm = TRUE),
    min_gds           = min(gds, na.rm = TRUE),            max_gds         = max(gds, na.rm = TRUE),
    mean_quip_rs      = mean(quip_rs_comb, na.rm = TRUE),  sd_quip_rs      = sd(quip_rs_comb, na.rm = TRUE),
    min_quip_rs       = min(quip_rs_comb, na.rm = TRUE),   max_quip_rs     = max(quip_rs_comb, na.rm = TRUE),
    .groups = "drop"
  )

# PwP-only summaries (UPDRS, H&Y, duration)
pwp_summaries <- demographics %>%
  filter(group %in% c("PwP", "PwP+ICB")) %>%
  group_by(group) %>%
  summarise(
    mean_updrs        = mean(updrs_iii, na.rm = TRUE),     sd_updrs        = sd(updrs_iii, na.rm = TRUE),
    min_updrs         = min(updrs_iii, na.rm = TRUE),      max_updrs       = max(updrs_iii, na.rm = TRUE),
    mean_hy           = mean(h_y, na.rm = TRUE),           sd_hy           = sd(h_y, na.rm = TRUE),
    min_hy            = min(h_y, na.rm = TRUE),            max_hy          = max(h_y, na.rm = TRUE),
    mean_duration_yrs = mean(parkinsons_duration_months / 12, na.rm = TRUE),
    sd_duration_yrs   = sd(parkinsons_duration_months / 12, na.rm = TRUE),
    min_duration_yrs  = min(parkinsons_duration_months / 12, na.rm = TRUE),
    max_duration_yrs  = max(parkinsons_duration_months / 12, na.rm = TRUE),
    .groups = "drop"
  )

# LEDD summaries separately — not all participants have LEDD recorded
ledd_summaries <- demographics %>%
  filter(!is.na(ledd_levodopa_da), !is.na(ledd_da_only)) %>%
  group_by(group) %>%
  summarise(
    mean_ledd_levo_da = mean(ledd_levodopa_da), sd_ledd_levo_da  = sd(ledd_levodopa_da),
    min_ledd_levo_da  = min(ledd_levodopa_da),  max_ledd_levo_da = max(ledd_levodopa_da),
    mean_ledd_da      = mean(ledd_da_only),     sd_ledd_da       = sd(ledd_da_only),
    min_ledd_da       = min(ledd_da_only),      max_ledd_da      = max(ledd_da_only),
    .groups = "drop"
  )


# frequencies
gender_freqs <- demographics %>%
  group_by(group,
           gender) %>%
  summarise(n = n(),
            .groups = "drop") %>%
  pivot_wider(names_from = gender,
              values_from = n,
              values_fill = 0)

td_pigd_freqs <- demographics %>%
  filter(!is.na(td_pigd)) %>%
  group_by(group,
           td_pigd) %>%
  summarise(n = n(),
            .groups = "drop") %>%
  pivot_wider(names_from = td_
              igd,
              values_from = n, values_fill = 0)

# frequency of ICB types
icb_freqs <- demographics %>%
  group_by(group) %>%
  summarise(
    gambling         = sum(icb_gambling == "1", na.rm = TRUE),
    sexual_behaviour = sum(icb_sexual_behaviour == "1", na.rm = TRUE),
    buying           = sum(icb_buying == "1", na.rm = TRUE),
    eating           = sum(icb_eating == "1", na.rm = TRUE),
    hobbism          = sum(icb_hobbism == "1", na.rm = TRUE),
    punding          = sum(icb_punding == "1", na.rm = TRUE),
    walkabout        = sum(icb_walkabout == "1", na.rm = TRUE),
    medication       = sum(icb_medication == "1", na.rm = TRUE),
    .groups = "drop"
  )


####################
# NORMALITY CHECKS #
####################

age_normality <- demographics %>%
  group_by(group) %>%
  summarise(
    p_value = shapiro.test(age)$p.value,
    .groups = "drop"
  )


###########################
# INFERENTIAL STATISTICS  #
###########################

# Age only
# pairwise subsets for planned t-tests
pwp_hc_data  <- demographics %>%
  filter(group == "PwP" |
           group == "HC")
pwp_icb_data <- demographics %>%
  filter(group == "PwP" |
         group == "PwP+ICB")

age_aov <- aov(age ~ group, data = demographics) %>%
  broom::tidy()

pwp_hc_age_var <- var.test(age ~ group, data = pwp_hc_data) %>%
  broom::tidy()
pwp_icb_age_var <- var.test(age ~ group, data = pwp_icb_data) %>%
  broom::tidy()

# change var.equal depending on variance tests
pwp_hc_age_ttest <- t.test(age ~ group, data = pwp_hc_data, var.equal = TRUE) %>%
  broom::tidy()
pwp_icb_age_ttest <- t.test(age ~ group, data = pwp_icb_data, var.equal = TRUE) %>%
  broom::tidy()