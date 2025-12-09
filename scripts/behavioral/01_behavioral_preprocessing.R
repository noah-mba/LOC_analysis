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
raw_dist <- read_psychopy("loc_label-distractor", "*.csv")

clean_dist <- raw_dist %>%
  filter(!is.na(key_resp_dist.keys)) %>%
  select(
    subject = participant,
    dist_resp = key_resp_dist.keys,
    dist_rt   = key_resp_dist.rt,
    correct_ans = corr_ans  # Assuming math task or similar
  )

# B. Sanity Checks
if(nrow(clean_dist) != N_TRIALS_DIST) {
  warning(paste("Distractor Warning: Expected", N_TRIALS_DIST, "trials, found", nrow(clean_dist)))
}

# C. Save
write_csv(clean_dist, file.path(save_dir, paste0(sub_id, "_distractor_clean.csv")))
message("✅ Distractor data processed and saved.")

# =========================================================================
# 3. RETRIEVAL PHASE (Import, Select, Check, Save)
# =========================================================================

# A. Import & Select
raw_ret <- read_psychopy("loc_label-retrieval", "*.csv")

clean_ret <- raw_ret %>%
  filter(!is.na(key_resp_ret.keys)) %>%
  select(
    subject = participant,
    stim_id = image_file,      # Must match Encoding name!
    ret_resp = key_resp_ret.keys,
    ret_rt   = key_resp_ret.rt,
    ret_condition = condition, 
    correct_ans = corr_ans
  )

# B. Sanity Checks
if(nrow(clean_ret) != N_TRIALS_RET) {
  stop(paste("Retrieval Error: Expected", N_TRIALS_RET, "trials, found", nrow(clean_ret)))
}

# Check for undefined keys
if(any(!clean_ret$ret_resp %in% KEYS_ALLOWED)) {
  warning("Retrieval Warning: Unexpected key presses detected.")
}

# C. Save
write_csv(clean_ret, file.path(save_dir, paste0(sub_id, "_ret_clean.csv")))
message("✅ Retrieval data processed and saved.")

# =========================================================================
# 4. MERGE PHASES
# =========================================================================

# A. Merge
# specific columns from encoding we want to carry over to retrieval
enc_to_merge <- clean_enc %>%
  select(subject, stim_id, enc_resp, enc_rt, enc_condition = condition)

merged_data <- clean_ret %>%
  left_join(enc_to_merge, by = c("subject", "stim_id"))

# B. Merge Logic Check
# Did we lose data? or gain rows?
if(nrow(merged_data) != nrow(clean_ret)) {
  stop("Merge Error: Row count changed after merge! Check Stimulus IDs.")
}

# Check if condition labels match (optional but recommended)
mismatches <- merged_data %>% filter(ret_condition != enc_condition)
if(nrow(mismatches) > 0) {
  warning("Merge Warning: Condition labels differ between phases for some items.")
}

# C. Save Final
write_csv(merged_data, file.path(save_dir, paste0(sub_id, "_all_phases_merged.csv")))
message("✅ MERGE COMPLETE. File saved.")