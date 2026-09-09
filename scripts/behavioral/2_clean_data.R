# Laura Cardós-Vicente, lauracardos@ugr.es
# Adapted by Noah — flags (does not delete) rows affected by the retrieval-task
# cue_file bug, so the CSV keeps its full 84 rows (64 encoding + 20 new) and stays
# aligned with the actual EEG trigger count for epoching.

library(dplyr)
library(readr)
library(stringr)
library(fs)

bids_dir <- "C:/Users/noahm/projects/loc_analysis/data/bids"

sujetos_g1 <- sprintf("sub-%02d", 3:14)
sujetos_g2 <- sprintf("sub-%02d", 16:44)
# todos_los_sujetos <- c(sujetos_g1, sujetos_g2)

cat("\n=== STARTING QC FLAGGING IN BIDS ===\n")

todos_los_sujetos <- c("sub-09", "sub-10", "sub-11", "sub-12") # I ran the updated script for the subjects who passed behavioral inclusion,  but had the retrieval bug

for (sujeto in todos_los_sujetos) {
  
  ruta_carpeta <- file.path(bids_dir, sujeto, "beh")
  
  if (!dir_exists(ruta_carpeta)) {
    warning(sprintf("Folder not found, skipping: %s", ruta_carpeta))
    next
  }
  
  archivos_csv <- dir_ls(ruta_carpeta, regexp = "\\.csv$")
  
  if (length(archivos_csv) == 0) {
    warning(sprintf("No CSV files found for subject: %s", sujeto))
    next
  }
  
  for (archivo in archivos_csv) {
    
    datos <- read_csv(archivo, col_types = cols(.default = "c"), show_col_types = FALSE)
    
    if (!("cue_file" %in% colnames(datos))) {
      warning(sprintf("File %s does not have a column named 'cue_file'.", basename(archivo)))
      next
    }
    
    # A cue_file that appears more than once means the same stimulus was
    # presented twice at retrieval (the early retrieval-task bug). We don't
    # guess which copy is "real" by label — we trust enc_trial_count, which
    # comes from the corrected Python merge and only exists on the copy that
    # genuinely matched an encoding trial. The copy WITHOUT a match is the
    # untrustworthy one and gets flagged.
    cue_dup <- duplicated(datos$cue_file) | duplicated(datos$cue_file, fromLast = TRUE) # cue_dup marks every row that shares its cue_file with at least one other row
    sin_match_encoding <- is.na(datos$enc_trial_count) # TRUE if a row has no tie to an encoding trial (foil retrieval trials!)
    
    bad_trial <- cue_dup & sin_match_encoding # only flags those rows where the cue_file is duplicated somewhere in the data
    # and it's the copy that didn't get a real encoding match
    
    datos$bad_trial <- bad_trial
    datos$bad_trial_reason <- ifelse(bad_trial, "duplicate_cue_no_encoding_match", NA_character_)
    
    n_flagged <- sum(bad_trial)
    write_csv(datos, archivo, na = "")
    
    cat(sprintf("[OK] %s | %s -> %d/%d rows flagged bad_trial (all rows kept).\n",
                sujeto, basename(archivo), n_flagged, nrow(datos)))
  }
}

cat("\n=== BIDS QC FLAGGING COMPLETED ===\n")