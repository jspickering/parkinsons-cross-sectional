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

groups = c("PwP", "ICD", "OC", "YC")
n_participants = 3
trials_per_participant = 4
n_rows_group = n_participants * trials_per_participant # 12

go_no_go_dummy_data <- map_dfr(seq_along(groups), function(i) {
  grp <- groups[i]
  tibble(
    group = grp,
    x1 = seq_len(n_rows_group) + (i - 1) * n_rows_group,                # 1:12, 13:24, ...
    participant = sprintf("%03d", rep(seq_len(length(groups) * n_participants), each = trials_per_participant))[ ( (i-1)*n_rows_group + 1) : (i*n_rows_group) ],
    trial_num = rep(seq_len(trials_per_participant), times = n_participants),
    condition = sample(c(rep(0, times = round(0.25 * n_rows_group)),
                         rep(1, times = n_rows_group - round(0.25 * n_rows_group)))),
    trial_rt = round(runif(n_rows_group, min = 250, max = 1000), 2),
    trial_acc = sample(c(1, 0), size = n_rows_group, replace = TRUE, prob = c(0.9, 0.1))
  )
})

write_csv(go_no_go_dummy_data, "raw_data/go_no_go_dummy_data.csv")


##### Stop Signal Task

groups <- c("PWP", "ICD", "OC", "YC")
n_per_group   <- 5     # more participants so exclusions don't wipe out the dataset
total_trials  <- 384   # pre-reg: 384 trials per participant
n_stop        <- round(total_trials * 0.25)  # 96 stop trials
n_go          <- total_trials - n_stop       # 288 go trials

generate_sst_participant <- function(pid, grp) {
  # trialType: 1=leftGo, 2=rightGo, 3=leftStop, 4=rightStop
  # condition is derived from trialType so the two columns are always consistent
  trial_types <- sample(c(sample(1:2, n_go,   replace = TRUE),
                           sample(3:4, n_stop, replace = TRUE)))
  condition   <- if_else(trial_types %in% 1:2, "go", "stop")

  # Go trial outcomes: ~93% correct, ~5% wrong arrow, ~2% missed
  # Stop trial outcomes: ~50% successful stop (keeps stop accuracy within the 25–75% exclusion window)
  go_outcomes   <- sample(c("correct", "wrong arrow", "missed"), n_go,   replace = TRUE, prob = c(0.93, 0.05, 0.02))
  stop_outcomes <- sample(c("successful stop", "failed stop"),   n_stop, replace = TRUE, prob = c(0.50, 0.50))

  outcomes              <- character(total_trials)
  outcomes[condition == "go"]   <- go_outcomes
  outcomes[condition == "stop"] <- stop_outcomes

  # keyPressed: "z" for left trials, "m" for right trials, "0" for no response
  correct_key <- if_else(trial_types %in% c(1, 3), "z", "m")
  wrong_key   <- if_else(correct_key == "z", "m", "z")
  key_pressed <- case_when(
    outcomes == "correct"        ~ correct_key,
    outcomes == "wrong arrow"    ~ wrong_key,
    outcomes %in% c("missed", "successful stop") ~ "0",
    outcomes == "failed stop"    ~ correct_key   # responded despite the stop signal
  )

  # trialRT in ms: realistic go/failed-stop RTs centred ~450 ms; NA when no response
  # Mean SSD ~250 ms → SSRT ≈ 450 – 250 = 200 ms, well above the 50 ms exclusion floor
  has_response        <- key_pressed != "0"
  trial_rt            <- rep(NA_real_, total_trials)
  trial_rt[has_response] <- pmax(round(rnorm(sum(has_response), mean = 450, sd = 80)), 151)

  # SSD present on every row (only meaningful on stop trials per pre-reg, but always recorded)
  ssd         <- round(runif(total_trials, min = 150, max = 350))
  trial_start <- runif(total_trials, min = 2e-6, max = 1e-5)
  resp_time   <- rep(0, total_trials)
  resp_time[has_response] <- trial_start[has_response] + trial_rt[has_response] / 1000

  tibble(
    participant = pid,
    group       = grp,
    condition   = condition,
    trialType   = trial_types,
    keyPressed  = key_pressed,
    blockNum    = rep(1:4, each = total_trials / 4),
    trialNum    = rep(seq_len(total_trials / 4), times = 4),
    ISI         = round(runif(total_trials, min = 250, max = 500)),
    SSD         = ssd,
    trialStart  = round(trial_start, 6),
    respTime    = round(resp_time, 6),
    trialRT     = trial_rt,
    trialAcc    = outcomes
  )
}

sst_participants <- tibble(
  participant = sprintf("%03d", seq_len(length(groups) * n_per_group)),
  group       = rep(groups, each = n_per_group)
)

sst_dummy_data <- map2_dfr(
  sst_participants$participant,
  sst_participants$group,
  generate_sst_participant
)

write_csv(sst_dummy_data, "raw_data/stop_signal_task_dummy_data.csv")

