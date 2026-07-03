# -------------------------------------------------------------------------
# Script: 01_behavioral_preprocessing.R
# Purpose: Import raw PsychoPy data, clean columns, run QC, and save formatted files.
# -------------------------------------------------------------------------


setwd("C:/Users/noahm/projects/loc_analysis")

# Check if tidyverse is installed; if not, install it
if (!"tidyverse" %in% installed.packages()) {
  message("Installing tidyverse...")
  install.packages("tidyverse")
}

library(tidyverse)


# =========================================================================
# 0. SETUP
# =========================================================================

sub_id   <- "s14" 
base_dir <- "data/raw"
save_dir <- "data/derivatives/beh"

# Create output folder if missing
if(!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)

# QC Constants 
N_TRIALS_ENC  <- 64
N_TRIALS_RET  <- 84
KEYS_ALLOWED  <- c("1", "2", "3", "4", "left", "right", "space")

# Helper function to read the correct file
read_psychopy <- function(sub_folder, pattern_string) {
  path <- file.path(base_dir, sub_id, sub_folder)
  file <- list.files(path, pattern = pattern_string, full.names = TRUE)
  if(length(file) == 0) stop(paste("File not found in:", sub_folder))
  read_csv(file[1], show_col_types = FALSE)
}

# =========================================================================
# 1. ENCODING PHASE (Import, Select, Check, Save)
# =========================================================================

# A. Import & Select
raw_enc <- read_psychopy("loc_label-encoding", "*.csv")

# The data set has 68 rows.. why?
sum(!is.na(raw_enc$practice_trials.target_resp.keys))
# There seems to be only 1 practice trial saved

sum(!is.na(raw_enc$target_resp.keys))
# It seems that 64 trials were saved, good!

enc_df <- raw_enc %>%
  rename(trial_n = trials.thisTrialN) %>% 
  mutate(trial_n = trial_n + 1) %>%
  filter(trial_n %in% c(1:64)) %>%
  relocate(trial_n) %>%
  select(
    trial_n,
    question_type,
    corr_ans_side,
    corr_label,
    target_resp.keys,
    target_resp.corr,
    target_resp.rt,
    condition_id,
    low_prediction,
    high_prediction,
    thisRow.t,
    fixcross_display.started,
    fixcross_display.stopped,
    cue_trial.started,
    cue_trial.stopped,
    action1_trial.started,
    action1_trial.stopped, 
    action2_trial.started,
    action2_trial.stopped, 
    action3_trial.started,
    action3_trial.stopped, 
    target_trial.stopped,
    target_trial.started,
    response.started,
    response.stopped,
    trials.target_resp.keys,
    trials.target_resp.corr,
    participant,
    date,
    expName
  )
    
    
write_csv(enc_df, file.path(save_dir, paste0(sub_id, "_enc_clean.csv")))


# Participant has 100% accuracy for both types of encoding question
# Mean RT for action: 1.41s, for color: 1.42s
# Where do we ask for and save participants' age & gender?

# =========================================================================
# 2. DISTRACTOR PHASE (Import, Select, Check, Save)
# =========================================================================

# A. Import & Select
raw_distractor <- read_psychopy("loc_label-distractor", "*.csv")

dist_duration = sum(raw_distractor$rt)
print(dist_duration)

# Why is it only 220 seconds?? And not 300!

# =========================================================================
# 3. RETRIEVAL PHASE (Import, Select, Check, Save)
# =========================================================================

# A. Import & Select
raw_retrieval <- read_psychopy("loc_label-retrieval", "*.csv")

ret_df <- raw_retrieval %>%
  rename(trial_n = trials.thisTrialN) %>% 
  mutate(trial_n = trial_n + 1) %>%
  filter(trial_n %in% c(1:84)) %>%
  relocate(trial_n) %>%
  select(
    trial_n,
    comic_name,
    condition_id,
    low_prediction,
    high_prediction,
    direction,
    cue_file,
    target_file,
    ending_corr_label,
    OvsN,
    OvsN_code,
    target_sat,
    cue_rec_resp.corr,
    cue_rec_resp.rt,
    ending_rec_resp.corr,
    ending_rec_resp.rt,
    afc_resp.corr,
    afc_resp.rt,
    thisRow.t,
    fixcross_display.started,
    fixcross_display.stopped,
    cue_display.started,
    cue_display.stopped,
    cue_recog.started,
    story_rec_prompt.started,
    cue_recog.stopped,
    trials.cue_rec_resp.corr,
    trials.cue_rec_resp.rt,
    ending_recall.started,
    ending_rec_prompt.started,
    ending_recall.stopped,
    trials.ending_rec_resp.corr,
    trials.ending_rec_resp.rt,
    target_trial.started,
    trials.afc_resp.corr,
    trials.afc_resp.rt,
    fix2.started,
    fix2.stopped,
    participant
  )
    



acc_story_rec <- mean(clean_retrieval$story_rec_resp.corr) #Is this the first question? Seen before?
acc_afc <- mean(clean_retrieval$afc_resp.corr)
# No variable for the 'Did something unexpected' response collected

sum(clean_retrieval$story_rec_resp.corr)


# C. Save
write_csv(ret_df, file.path(save_dir, paste0(sub_id, "_ret_clean.csv")))
message("✅ Retrieval data processed and saved.")

