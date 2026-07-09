#Laura Cardós-Vicente, lauracardos@ugr.es#
#26-05-2026#
# This script uses BIDS behavioral data cleaning (Merged Encoding & Retrieval)
#This script aims to clean the merged data by deleting the duplicates and practice trials#
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
todos_los_sujetos <- c(sujetos_g1, sujetos_g2)

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
      
      # Step B: Create deletion masks
      # 1. It is a practice story (or story 136 for G1)
      es_practica <- !is.na(numeros) & (numeros %in% forbidden_list)
      
      # 2. It is a duplicate (marks TRUE starting from the second occurrence)
      es_duplicado <- !is.na(numeros) & duplicated(numeros)
      
      # Step C: Apply the filter
      # Keep only what is NOT a practice story and NOT a duplicate
      filas_a_mantener <- !es_practica & !es_duplicado
      datos_limpios <- datos[filas_a_mantener, ]
      
      # Step D: Overwrite the original file
      # Use na = "" to keep missing values completely blank (as in your previous step)
      write_csv(datos_limpios, archivo, na = "")
      
      # Print detailed report
      filas_borradas <- filas_antes - nrow(datos_limpios)
      cat(sprintf("[OK] %s | %s -> Overwritten. %d trials removed.\n", 
                  sujeto, basename(archivo), filas_borradas))
      
    } else {
      warning(sprintf("File %s does not have a column named 'cue_file'.", basename(archivo)))
    }
  }
}

cat("\n=== BIDS CLEANING PROCESS COMPLETED ===\n")
