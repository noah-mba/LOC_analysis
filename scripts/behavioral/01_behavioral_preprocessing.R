# -------------------------------------------------------------------------
# Script: 01_behavioral_preprocessing.R
# Purpose: Import raw PsychoPy data, clean columns, run QC, and save formatted files.
# -------------------------------------------------------------------------

# Check if tidyverse is installed; if not, install it
if (!"tidyverse" %in% installed.packages()) {
  message("Installing tidyverse...")
  install.packages("tidyverse")
}

library(tidyverse)

# =========================================================================
# 0. SETUP
# =========================================================================

sub_id   <- "pilot_01" 
base_dir <- "data/raw"
save_dir <- "data/derivatives/behavioral"

# Create output folder if missing
if(!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)

# QC Constants (Update these based on your design!)
N_TRIALS_ENC  <- 64
N_TRIALS_RET  <- 64
#N_TRIALS_DIST <- 64 # Example number
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

# Variables schema, relationship, object, context, pair, spec_obj, violation 
# differently encoded or missing for the normal trials vs. practice trials


raw_encoding_trials <- raw_enc %>%
  filter(!is.na(cue_file))

raw_encoding_trials$practice <- ifelse(
  is.na(raw_encoding_trials$list_id),  # Test: Is 'list_id' NA?
  1,                                  # Yes (TRUE): Assign 1
  0                                   # No (FALSE): Assign 0
)



# B. Sanity Checks
if(nrow(clean_enc) != N_TRIALS_ENC) {
  stop(paste("Encoding Error: Expected", N_TRIALS_ENC, "trials, found", nrow(clean_enc)))
}
if(any(duplicated(clean_enc$stim_id))) {
  stop("Encoding Error: Duplicate stimulus IDs found.")
}

# C. Save
write_csv(clean_enc, file.path(save_dir, paste0(sub_id, "_enc_clean.csv")))
message("✅ Encoding data processed and saved.")

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

# Why are there only 64 trials? And all of them are OLD? 

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
}

# C. Save
write_csv(clean_ret, file.path(save_dir, paste0(sub_id, "_ret_clean.csv")))
message("✅ Retrieval data processed and saved.")

