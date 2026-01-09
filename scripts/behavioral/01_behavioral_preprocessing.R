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

sub_id   <- "pilot_02" 
base_dir <- "data/raw"
save_dir <- "data/derivatives/behavioral"

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

raw_encoding_trials <- raw_enc %>%
  filter(!is.na(target_resp.keys))

raw_encoding_trials  %>%
  filter(question_type %in% c("color", "action")) %>%
  group_by(question_type) %>%
  summarise(
    mean_corr = mean(trials.target_resp.corr, na.rm = TRUE),
    n = n()
  )

raw_encoding_trials  %>%
  filter(question_type %in% c("color", "action")) %>%
  group_by(question_type) %>%
  summarise(
    mean_corr = mean(target_resp.rt, na.rm = TRUE),
    n = n()
  )

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

retrieval_df <- raw_retrieval %>%
  filter(!is.na(comic_name))

mean(retrieval_df$trials.cue_rec_resp.corr) #average accuracy of 75%
mean(retrieval_df$trials.afc_resp.corr) #average accuracy of 14%

# Why are there only 64 trials? And all of them are OLD? 
# Missing column for end_rec_resp.corr

clean_retrieval <- retrieval_df %>%
  select(
    thisN,
    comic_name,
    condition_id,
    low_prediction,
    high_prediction,
    cue_file,
    target_file,
    OvsN,
    target_sat,
    story_rec_resp.corr,
    afc_resp.corr,
    trials.story_rec_resp.corr,
    trials.thisIndex,
    thisRow.t,
  )

acc_story_rec <- mean(clean_retrieval$story_rec_resp.corr) #Is this the first question? Seen before?
acc_afc <- mean(clean_retrieval$afc_resp.corr)
# No variable for the 'Did something unexpected' response collected

sum(clean_retrieval$story_rec_resp.corr)


# C. Save
write_csv(clean_ret, file.path(save_dir, paste0(sub_id, "_ret_clean.csv")))
message("✅ Retrieval data processed and saved.")

