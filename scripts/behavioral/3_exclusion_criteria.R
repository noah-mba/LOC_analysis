#Laura Cardós-Vicente, lauracardos@ugr.es#
#26-05-2026#

#This script applies the encoding and retrieval exclusion criteria#

# ==============================================================================
# Exclusion Criteria: Encoding Accuracy & Retrieval d'
# Adjusted for unified BIDS merged files
# ==============================================================================

# install.packages(jsonliste)
library(jsonlite)
library(dplyr)
library(readr)
library(fs)


# --- 1. CONFIGURATION --- #
bids_dir <- "C:/Users/noahm/projects/loc_analysis/data/bids"

# Define subjects (skipping sub-15)
sujetos_g1 <- sprintf("sub-%02d", 3:14)
sujetos_g2 <- sprintf("sub-%02d", 16:44)
todos_los_sujetos <- c(sujetos_g1, sujetos_g2)

# --- 2. HELPER FUNCTIONS FOR d' --- #
# Calculate d' with edge correction for extreme rates (0 or 1)
calc_dprime <- function(n_hit, n_miss, n_fa, n_cr) {
  n_old <- n_hit + n_miss
  n_new <- n_fa + n_cr
  
  hr <- n_hit / n_old
  far <- n_fa / n_new
  
  # Edge correction
  if (!is.na(hr) && hr == 1) hr <- (n_old - 0.5) / n_old
  if (!is.na(hr) && hr == 0) hr <- 0.5 / n_old
  if (!is.na(far) && far == 1) far <- (n_new - 0.5) / n_new
  if (!is.na(far) && far == 0) far <- 0.5 / n_new
  
  dprime_val <- qnorm(hr) - qnorm(far)
  return(list(dprime = dprime_val, hr = hr, far = far))
}

# Simulate null distribution to find the chance threshold (95th percentile)
simulate_dprime_threshold <- function(n_old, n_new, n_iterations = 5000, prob = 0.95) {
  n_trials <- n_old + n_new
  trial_seq <- c(rep(1, n_old), rep(0, n_new))
  sim_dprimes <- numeric(n_iterations)
  
  for(i in 1:n_iterations) {
    # Simulate random guessing
    sim_resp <- sample(c(0, 1), n_trials, prob = c(0.5, 0.5), replace = TRUE)
    
    n_hit <- sum(trial_seq == 1 & sim_resp == 1)
    n_miss <- sum(trial_seq == 1 & sim_resp == 0)
    n_fa <-  sum(trial_seq == 0 & sim_resp == 1)
    n_cr <-  sum(trial_seq == 0 & sim_resp == 0)
    
    sim_dprimes[i] <- calc_dprime(n_hit, n_miss, n_fa, n_cr)$dprime
  }
  
  # Return the exact threshold value
  return(quantile(sim_dprimes, probs = prob, na.rm = TRUE))
}

# --- 3. CALCULATE GROUP THRESHOLDS --- #
cat("Simulating null general thresholds (5000 iterations)...\n")
set.seed(123) # For reproducibility

# G1 (sub-03 to sub-14): 57 old, 16 new | G2 (sub-16 to sub-44): 64 old, 20 new
threshold_g1 <- simulate_dprime_threshold(57, 16)
threshold_g2 <- simulate_dprime_threshold(64, 20)

cat(sprintf("Threshold G1 (95%%): %.3f\nThreshold G2 (95%%): %.3f\n\n", threshold_g1, threshold_g2))

# --- 4. MAIN ANALYSIS LOOP --- #
# Empty dataframe to store results
resultados <- data.frame()

for (sujeto in todos_los_sujetos) {
  
  ruta_carpeta <- file.path(bids_dir, sujeto, "beh")
  if (!dir_exists(ruta_carpeta)) next
  
  # Find the unified CSV
  archivos_csv <- dir_ls(ruta_carpeta, regexp = "\\.csv$")
  if (length(archivos_csv) == 0) next
  archivo <- archivos_csv[1]
  
  # Read as plain text to prevent parsing errors
  datos <- read_csv(archivo, col_types = cols(.default = "c"), show_col_types = FALSE)
  
  # Ensure necessary columns exist before proceeding
  if (all(c("enc_acc", "ret_trial_type_code", "cue_recog_acc") %in% colnames(datos))) {
    
    # -- A. ENCODING PERFORMANCE --
    target_corr <- as.numeric(datos$enc_acc)
    enc_performance <- mean(target_corr, na.rm = TRUE) * 100
    
    # -- B. RETRIEVAL D' --
    estimulo <- as.numeric(datos$ret_trial_type_code)
    precision <- as.numeric(datos$cue_recog_acc)
    
    # Filter valid retrieval trials (ignore NAs in cue_rec_resp)
    valid_ret <- !is.na(estimulo) & !is.na(precision)
    est_v <- estimulo[valid_ret]
    prec_v <- precision[valid_ret]
    
    # Reconstruct subject response (If correct, response = stimulus. Else, opposite)
    resp_v <- ifelse(prec_v == 1, est_v, 1 - est_v)
    
    n_hit <- sum(est_v == 1 & resp_v == 1)
    n_miss <- sum(est_v == 1 & resp_v == 0)
    n_fa <- sum(est_v == 0 & resp_v == 1)
    n_cr <- sum(est_v == 0 & resp_v == 0)
    
    metrics <- calc_dprime(n_hit, n_miss, n_fa, n_cr)
    
    # -- C. EVALUATION --
    current_threshold <- if (sujeto %in% sujetos_g1) threshold_g1 else threshold_g2
    
    pass_enc <- ifelse(enc_performance > 90, "YES", "NO")
    pass_ret <- ifelse(metrics$dprime > current_threshold, "YES", "NO")
    final_decision <- ifelse(pass_enc == "YES" & pass_ret == "YES", "KEEP", "EXCLUDE")
    
    # Store data
    resultados <- rbind(resultados, data.frame(
      Subject = sujeto,
      Enc_Perf = enc_performance,
      Pass_Enc = pass_enc,
      Ret_dprime = metrics$dprime,
      Required_Thres = current_threshold,
      Pass_Ret = pass_ret,
      Decision = final_decision
    ))
  }
}

# --- 5. PRINT REPORTS --- #
cat("=========================================\n")
cat("          FINAL EXCLUSION REPORT         \n")
cat("=========================================\n")
print(resultados %>% select(Subject, Enc_Perf, Pass_Enc, Ret_dprime, Pass_Ret, Decision))

cat("\n=========================================\n")
cat("            SUBJECTS TO EXCLUDE          \n")
cat("=========================================\n")

excluidos <- resultados %>% filter(Decision == "EXCLUDE")

ids_to_exclude <- sub("sub-", "", excluidos$Subject)
ids_to_exclude

exclusion_list <- list(
  behavioral_exclusions = ids_to_exclude
)

write_json(
  exclusion_list,
  "../../data/derivatives/subject_exclusions.json",
  pretty = TRUE,
  auto_unbox = TRUE
)

if (nrow(excluidos) > 0) {
  for (i in 1:nrow(excluidos)) {
    cat(sprintf("-> %s | Enc: %s (%.1f%%) | Ret: %s (d'=%.2f)\n", 
                excluidos$Subject[i], excluidos$Pass_Enc[i], excluidos$Enc_Perf[i], 
                excluidos$Pass_Ret[i], excluidos$Ret_dprime[i]))
  }
} else {
  cat("Great news! All subjects passed both encoding and retrieval criteria.\n")
}


save.csv