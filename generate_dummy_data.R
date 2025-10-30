# Generate dummy data so that we can write the analysis code
# without needing to run it on the real data on the network drive
# because it's really slow
# NB: This doesn't represent the shape of the real data in terms of distributions etc
# but they are at least vaguely realistic

library(tidyverse)

set.seed(186)  # for reproducibility

##### Demographics

demographics_q_dummy_data <- tibble(
  `x1` = 1:30,
  `Group` = sample(c("OC", "ICD", "PwP"), size = 30, replace = TRUE),
  `Age` = round(runif(30, min = 50, max = 75)),   # uniform ages 50–75
  `Gender` = sample(c("m", "f"), size = 30, replace = TRUE),
  `MoCA` = round(runif(30, min = 24, max = 30)),
  `QUIP-current_pos` = round(runif(30, min = 0, max = 5)),
  `QUIP-anytime_pos` = round(runif(30, min = 0, max = 5)),
  `QUIP-RS_comb` = round(runif(30, min = 0, max = 70)),
  `GDS` = round(runif(30, min = 0, max = 30)),
  `StroopA_RT` = round(runif(30, min = 20, max = 45)),
  `StroopA_uncorrected` = round(runif(30, min = 0, max = 2)),
  `StroopA_corrected` = round(runif(30, min = 0, max = 1)),
  `StroopB_RT` = round(runif(30, min = 15, max = 30)),
  `StroopB_uncorrected` = round(runif(30, min = 0, max = 1)),
  `StroopB_corrected` = round(runif(30, min = 0, max = 1)),
  `StroopC_RT` = round(runif(30, min = 45, max = 110)),
  `StroopC_uncorrected` = round(runif(30, min = 0, max = 3)),
  `StroopC_corrected` = round(runif(30, min = 0, max = 3)),
  `StroopD_RT` = round(runif(30, min = 50, max = 100)),
  `StroopD_uncorrected` = round(runif(30, min = 0, max = 5)),
  `StroopD_corrected` = round(runif(30, min = 0, max = 3)),
  `TMT_A` = round(runif(30, min = 20, max = 60)),
  `TMT_B` = round(runif(30, min = 40, max = 120)),
  `Barratt` = round(runif(30, min = 40, max = 80)),
  `Barratt_A` = round(runif(30, min = 9, max = 24)),
  `Barratt_M` = round(runif(30, min = 9, max = 24)),
  `Barratt_N` = round(runif(30, min = 9, max = 24)),
  `BISBAS_total` = round(runif(30, min = 44, max = 80)),
  `BISBAS_drive` = round(runif(30, min = 7, max = 17)),
  `BISBAS_fun` = round(runif(30, min = 7, max = 17)),
  `BISBAS_reward` = round(runif(30, min = 7, max = 17)),
  `BISBAS_bis` = round(runif(30, min = 16, max = 28)),
  `UPPS_total` = round(runif(30, min = 76, max = 140)),
  `UPPS_neg` = round(runif(30, min = 15, max = 28)),
  `UPPS_premed` = round(runif(30, min = 16, max = 28)),
  `UPPS_pers` = round(runif(30, min = 16, max = 28)),
  `UPPS_sensation` = round(runif(30, min = 15, max = 45)),
  `UPPS_pos` = round(runif(30, min = 14, max = 30)),
  
  `UPDRS-III` = round(runif(30, min = 10, max = 50)),
  `H&Y` = round(runif(30, min = 1, max = 3)),
  `TD/PIGD` = sample(c("PIGD", "TD"), size = 30, replace = TRUE),
  `Parkinson's_duration_months` = round(runif(30, min = 12, max = 200)),
  `ICDc_gambling` = sample(c("0", "1"), size = 30, replace = TRUE),
  `ICDc_sexual_behaviour` = sample(c("0", "1"), size = 30, replace = TRUE),
  `ICDc_buying` = sample(c("0", "1"), size = 30, replace = TRUE),
  `ICDc_eating` = sample(c("0", "1"), size = 30, replace = TRUE),
  `ICDc_hobbism` = sample(c("0", "1"), size = 30, replace = TRUE),
  `ICDc_punding` = sample(c("0", "1"), size = 30, replace = TRUE),
  `ICDc_walkabout` = sample(c("0", "1"), size = 30, replace = TRUE),
  `ICDc_medication` = sample(c("0", "1"), size = 30, replace = TRUE),
  `LEDD levodopa + DA` = round(runif(30, min = 0, max = 1400)),
  `LEDD DA only` = round(runif(30, min = 0, max = 700))
) %>%
  # the OC group don't have Parkinson's specific data so replace with NAs
  mutate(`UPDRS-III` = if_else(Group == "OC", NA, `UPDRS-III`),
         `H&Y` = if_else(Group == "OC", NA, `H&Y`),
         `TD/PIGD`= if_else(Group == "OC", NA, `TD/PIGD`),
         `Parkinson's_duration_months` = if_else(Group == "OC", NA, `Parkinson's_duration_months`),
         `LEDD levodopa + DA`= if_else(Group == "OC", NA, `LEDD levodopa + DA`),
         `LEDD DA only`= if_else(Group == "OC", NA, `LEDD DA only`),
  # neither the OC nor Parkinson's groups have ICD data so replace with NAs
         `ICDc_gambling`= if_else((Group == "OC" | Group == "PwP"), NA, `ICDc_gambling`),
         `ICDc_sexual_behaviour`= if_else((Group == "OC" | Group == "PwP"), NA, `ICDc_sexual_behaviour`),
         `ICDc_buying`= if_else((Group == "OC" | Group == "PwP"), NA, `ICDc_buying`),
         `ICDc_eating`= if_else((Group == "OC" | Group == "PwP"), NA, `ICDc_eating`),
         `ICDc_hobbism`= if_else((Group == "OC" | Group == "PwP"), NA, `ICDc_hobbism`),
         `ICDc_punding`= if_else((Group == "OC" | Group == "PwP"), NA, `ICDc_punding`),
         `ICDc_walkabout`= if_else((Group == "OC" | Group == "PwP"), NA, `ICDc_walkabout`),
         `ICDc_medication`= if_else((Group == "OC" | Group == "PwP"), NA, `ICDc_medication`)
  )

demographics_q_dummy_data

write_csv(demographics_q_dummy_data, "raw_data/demographics_q_dummy_data.csv")


##### Go/No-Go
go_no_go_dummy_data <- tibble(
  `x1` = seq_len(40),
  participant = sprintf("%03d", rep(seq_len(10), each = 4)),
  trial_num = rep(seq_len(4), times = 10),
  # condition: 25% zeros, 75% ones, shuffled across rows
  condition = sample(c(rep(0, times = round(0.25 * 40)),
                       rep(1, times = 40 - round(0.25 * 40)))),
  # reaction time between 250 and 1000 ms with 2 decimal places
  trial_rt = round(runif(40, min = 250, max = 1000), 2),
  # accuracy: 90% 1s, 10% 0s
  trial_acc = sample(c(1, 0), size = 40, replace = TRUE, prob = c(0.9, 0.1))
)