library(tidyverse)
library(janitor)

### Resources
# G:\My Drive\Dropbox (University of Manchester) backup 2023\Impulsivity analysis\impulsivity-study\jade-thesis-chapter
# G:\My Drive\Dropbox (University of Manchester) backup 2023\Impulsivity analysis\impulsivity-study\jade-thesis-chapter - unfinished
# G:\My Drive\Dropbox (University of Manchester) backup 2023\Impulsivity analysis\impulsivity-study\questionnaire-analysis
# G:\My Drive\Dropbox (University of Manchester) backup 2023\Impulsivity analysis\impulsivity-study\Questionnaires
# Z:\Study 2_cross sectional\Jade-2024

### read in and tidy data
demographics_raw <- read_csv("raw_data/demographics_q_dummy_data.csv") %>%
  # put variables names in snake_case
  clean_names() %>%
  # remove any rows without a participant
  drop_na(group) %>%
  # convert data types for all columns to make sure they're correct
  mutate(
    across(c(x1,
             group,
             gender,
             td_pigd,
             ic_dc_gambling, # check this and all the below because the underscore doesn't look right
             ic_dc_sexual_behaviour,
             ic_dc_buying,
             ic_dc_eating,
             ic_dc_hobbism,
             ic_dc_punding,
             ic_dc_walkabout,
             ic_dc_medication
    ), as.character),
    across(c(age,
             mo_ca,
             quip_current_pos,
             quip_anytime_pos,
             quip_rs_comb,
             gds,
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
             stroop_d_corrected,
             tmt_a,
             tmt_b,
             barratt,
             barratt_a,
             barratt_m,
             barratt_n,
             bisbas_total,
             bisbas_drive,
             bisbas_fun,
             bisbas_reward,
             bisbas_bis,
             upps_total,
             upps_neg,
             upps_premed,
             upps_pers,
             upps_sensation,
             upps_pos,
             updrs_iii,
             h_y,
             parkinsons_duration_months
    ), as.numeric)
  ) %>%
  # remove YC participants as this was collected for MM's project
  subset(group != "YC")


# need to figure out how many are excluded due to low moca scores
# then need a list of participant IDs to exclude from all other analyses
# and need to be able to report the number of exclusions in the paper
moca_exclusions <- demographics_raw %>%
  count()