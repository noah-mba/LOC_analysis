#Laura Cardós-Vicente, lauracardos@ugr.es#
#26-05-2026#
#Modified 2026-09-04, Noah#
# This script uses BIDS behavioral data cleaning (Merged Encoding & Retrieval)
#This script FLAGS (rather than deletes) duplicate and practice trials, so row#
#count stays aligned with the EEG trigger count for epoch/metadata merging.#
library(dplyr)
library(readr)
library(stringr)
library(fs)

# ==============================================================================
# 1. Path and group configuration
# ==============================================================================
# Adjust this path to where your BIDS folder is located
bids_dir <- "C:/Users/noahm/projects/loc_analysis/data/bids"

# Define subjects in BIDS format (sub-XX)
# G1: sub-03 to sub-14
sujetos_g1 <- sprintf("sub-%02d", 3:14)
# G2: sub-16 to sub-44 (automatically skipping sub-15)
sujetos_g2 <- sprintf("sub-%02d", 16:44)

# todos_los_sujetos <- c(sujetos_g1, sujetos_g2)
todos_los_sujetos <- "sub-12"

# Forbidden stories lists. 
# Note: Story 136 is already in Group 1, so it will be automatically removed.
practice_g1 <- c(133, 136, 81, 19, 28, 32, 26, 156)
practice_g2 <- c(133, 153, 81, 1, 28, 32, 26, 156)

cat("\n=== STARTING DIRECT CLEANING IN BIDS ===\n")

# ==============================================================================
# 2. Main cleaning loop
# ==============================================================================
for (sujeto in todos_los_sujetos) {
  
  # Build the path to the subject folder
  ruta_carpeta <- file.path(bids_dir, sujeto, "beh")
  
  if (!dir_exists(ruta_carpeta)) {
    warning(sprintf("Folder not found, skipping: %s", ruta_carpeta))
    next
  }
  
  # Search for CSV files inside the subject folder
  archivos_csv <- dir_ls(ruta_carpeta, regexp = "\\.csv$")
  
  if (length(archivos_csv) == 0) {
    warning(sprintf("No CSV files found for subject: %s", sujeto))
    next
  }
  
  # 3. Iterate through each CSV (typically only one unified file per subject)
  for (archivo in archivos_csv) {
    
    # Read everything as plain text to prevent format alterations
    datos <- read_csv(archivo, col_types = cols(.default = "c"), show_col_types = FALSE)
    
    if ("cue_file" %in% colnames(datos)) {
      
      filas_antes <- nrow(datos)
      
      # Assign the correct forbidden list based on the group
      forbidden_list <- if (sujeto %in% sujetos_g1) practice_g1 else practice_g2
      
      # Step A: Extract the story number from 'cue_file'
      # Look for digits that are exactly between a "_" and "_Slide"
      patron <- "(?<=_)\\d+(?=_Slide)"
      numeros <- as.numeric(str_extract(datos$cue_file, patron))
      
      # Step B: Create exclusion masks (unchanged logic, just no longer used to drop rows)
      # 1. It is a practice story (or story 136 for G1)
      es_practica <- !is.na(numeros) & (numeros %in% forbidden_list)
      
      # 2. It is a duplicate (marks TRUE starting from the second occurrence)
      es_duplicado <- !is.na(numeros) & duplicated(numeros)
      
      # Step C: Flag instead of filter.
      # All original rows are kept (EEG trigger counts were confirmed complete for every
      # subject during QC, including sub-03/04/09/10/11/12 — so every row here corresponds
      # to a real triggered trial and must stay in place for epoch/metadata alignment).
      # 'enc_high_prediction' / 'enc_low_prediction' are left untouched so condition_label
      # in Notebook 3 keeps reflecting what was actually presented.
      datos$trial_excluded <- es_practica | es_duplicado
      datos$exclusion_reason <- dplyr::case_when(
        es_practica & es_duplicado ~ "practice_and_duplicate",
        es_practica               ~ "practice",
        es_duplicado              ~ "duplicate",
        TRUE                      ~ NA_character_
      )
      
      # Step D: Overwrite the original file (same row count as before cleaning)
      # write_csv(datos, archivo, na = "")
      
      # Print detailed report
      filas_marcadas <- sum(datos$trial_excluded, na.rm = TRUE)
      cat(sprintf("[OK] %s | %s -> Overwritten. %d/%d trials flagged (kept, not removed).\n", 
                  sujeto, basename(archivo), filas_marcadas, filas_antes))
      
    } else {
      warning(sprintf("File %s does not have a column named 'cue_file'.", basename(archivo)))
    }
  }
}

cat("\n=== BIDS CLEANING PROCESS COMPLETED ===\n")

# in R, on the pre-fix (original) sub-12 CSV, or recompute masks fresh:
cat("es_practica:", sum(es_practica), "\n")   # expect 6, for sub-12
cat("es_duplicado:", sum(es_duplicado), "\n") # expect 1, for sub-12
