####### Multilevel Mediation with Within-Between Centering + Time×Group on Mediator #######
### CHANGES FROM ORIGINAL SCRIPT:
### Starting point: Falk stacked model
### 1️⃣ Change 1: Add time as a variable (Smt = Sm × time)
### 2️⃣ Change 2: Add Tx×Time interaction on mediator (SmTxTime = Sm × Tx × time)
### 3️⃣ Change 3: Within–between centering of the mediator (SyMw, SyMb)
### 4️⃣ Change 4: Estimate separate within vs between b-paths
### 5️⃣ Change 5: Redefining the a-path (a_base vs a_change)
### 6️⃣ Change 6: Bootstrap adapted for all of the above
### 
### Includes PCS, Chairst30, and all other mediators ###

# Install packages if not installed
install.packages("devtools", repos = "https://cran.rstudio.com/")
install.packages("multilevelmediation", repos = "https://cran.rstudio.com/")
install.packages("readxl", repos = "https://cran.rstudio.com/")

library(devtools)
library(multilevelmediation)
library(tidyr)
library(nlme)
library(MCMCpack)
library(boot)
library(dplyr)
options(scipen = 999)

set.seed(1234)

# ---- Load data
library(readxl)
data <- read_excel("./CLBPEO.xlsx")
data <- as.data.frame(data)
data$ID <- as.factor(data$ID)

# 1️⃣ CHANGE 1: Add time as a variable
# Create time variable: 1=baseline (0), 2=post (1)
data$time <- ifelse(data$Time == 1, 0, 1)

# Keep rows with Pain
data <- subset(data, !is.na(Pain))

# Tx: 1 = PE, 0 = WL
data$Tx_bin <- ifelse(data$Tx == "PE", 1, 0)

# Mediators including PCS and Chairst30
mediators <- c("PCS", "Chairst30", "ISI", "TakaiP", "SLD", "GeneralDisability",
               "Health_status", "BMI", "Other_pain", "TSK", "BDI", "SAI",
               "TAI", "Balance", "TUG", "WT4S", "WT4F", "RAPA",
               "MeP_Balance", "MeP_chairst", "MeP_TUG", "MeP_Takai",
               "MeP_WT4", "MEP")

sink("model_mlmed_time.txt")
cat("=== MULTILEVEL MEDIATION with Within-Between Centering + Time×Group on Mediator ===\n\n")

results <- list()

for (mediator in mediators) {
  cat("========================================\n")
  cat("MEDIATOR:", mediator, "\n")
  cat("========================================\n\n")

  # Subset to available mediator rows
  temp_data <- subset(data, !is.na(data[[mediator]]) & !is.na(time))

  # 3️⃣ CHANGE 3: Within–between centering of the mediator
  # For each person: compute M_mean = average mediator across all times
  # Compute M_within = each timepoint minus M_mean
  temp_data <- temp_data %>%
    group_by(ID) %>%
    mutate(
      M_mean   = mean(.data[[mediator]], na.rm = TRUE),
      M_within = .data[[mediator]] - M_mean
    ) %>% ungroup()

  # Starting point: Falk stacked model (unchanged structure)
  tmp <- data.frame(
    Tx_bin = temp_data$Tx_bin,
    Y      = temp_data$Pain,
    M      = temp_data[[mediator]],
    L2id   = temp_data$ID,
    Md_w   = temp_data$M_within,  # within-person mediator
    M_bar  = temp_data$M_mean,    # between-person mean mediator
    time   = temp_data$time       # 1️⃣ CHANGE 1: Add time variable
  )

  tmp <- pivot_longer(tmp, cols = c(Y, M), names_to = "Outcome", values_to = "Z")

  # Starting point: Bauer et al. gates (unchanged)
  tmp$Sy <- ifelse(tmp$Outcome == "Y", 1, 0)
  tmp$Sm <- ifelse(tmp$Outcome == "M", 1, 0)

  # Starting point: Classic Tx & mediator effects (unchanged)
  tmp$SmTx <- tmp$Sm * tmp$Tx_bin        # baseline group gap in mediator (a_base)
  tmp$SyTx <- tmp$Sy * tmp$Tx_bin        # c' path (unchanged)
  
  # 3️⃣ CHANGE 3: Within–between centering effects
  tmp$SyMw <- tmp$Sy * tmp$Md_w          # within-person mediator effect (b-path)
  tmp$SyMb <- tmp$Sy * tmp$M_bar         # between-person mean control

  # 1️⃣ CHANGE 1: Add time effects on mediator
  tmp$Smt      <- tmp$Sm * tmp$time      # main effect of time on mediator
  
  # 2️⃣ CHANGE 2: Add Tx×Time interaction on mediator
  tmp$SmTxTime <- tmp$Sm * tmp$Tx_bin * tmp$time  # Tx×time on mediator = a_change (difference-in-differences)

  # Starting point: Fixed & random formulas (modified)
  # Original: "Z ~ 0 + Sm + Sy + SmTx + SyTx + SyM"
  # Modified: Added SyMw, SyMb, Smt, SmTxTime
  fixed.formula <- "Z ~ 0 + Sm + Sy + SmTx + SyTx + SyMw + SyMb + Smt + SmTxTime"
  random.formula <- "~ 0 + Sm + Sy | L2id"  # STARTING POINT: Unchanged

  control <- lmeControl(opt = "optim", maxIter = 100, msMaxIter = 100)
  mod_med_tmp <- lme(
    fixed   = as.formula(fixed.formula),
    random  = as.formula(random.formula),
    weights = varIdent(form = ~ 1 | Sm),
    data    = tmp,
    control = control
  )

  cat("### MAIN ANALYSIS - Model Summary ###\n")
  print(summary(mod_med_tmp))
  cat("\n")

  extract.model <- extract.modmed.mlm(list(model = mod_med_tmp, conv = TRUE))

  # 5️⃣ CHANGE 5: Redefining the a-path
  # Original: a_effect <- extract.model[["SmTx"]]  # baseline group difference
  # Modified: Now we have two a-paths:
  a_base   <- extract.model[["SmTx"]]      # baseline group difference (a_base)
  a_change <- extract.model[["SmTxTime"]] # difference-in-differences (a_change)
  
  # 4️⃣ CHANGE 4: Estimate separate within vs between b-paths
  # Original: b_effect <- extract.model[["SyM"]]   # simple mediator effect
  # Modified: within-person b-path only
  b_effect <- extract.model[["SyMw"]]      # within-person mediator effect (b-path)
  
  cprime_effect <- extract.model[["SyTx"]]  # STARTING POINT: Unchanged
  
  # 5️⃣ CHANGE 5: Compute indirect effects for both a-paths
  indirect_base   <- a_base * b_effect      # baseline mediation
  indirect_change <- a_change * b_effect   # change mediation

  cat("### EXTRACTED EFFECTS ###\n")
  cat("a_base (baseline group difference):", a_base, "\n")
  cat("a_change (Tx×time difference-in-differences):", a_change, "\n")
  cat("b-path (within mediator -> pain):", b_effect, "\n")
  cat("c'-path (Tx -> pain):", cprime_effect, "\n")
  cat("Indirect effect baseline (a_base × b):", indirect_base, "\n")
  cat("Indirect effect change (a_change × b):", indirect_change, "\n\n")

  # 6️⃣ CHANGE 6: Bootstrap adapted for all of the above
  # ---- Bootstrap function
  bootout <- function(dat, indices) {
    rdat <- dat[indices, ]

    # 6️⃣ CHANGE 6: Recompute within–between centering for each bootstrap sample
    # Original: No within-between centering
    # Modified: Recompute M_mean and M_within for each bootstrap sample
    rdat <- rdat %>%
      group_by(ID) %>%
      mutate(
        M_mean   = mean(.data[[mediator]], na.rm = TRUE),
        M_within = .data[[mediator]] - M_mean
      ) %>% ungroup()

    # 6️⃣ CHANGE 6: Bootstrap data frame (same structure as main analysis)
    tmpb <- data.frame(
      Tx_bin = ifelse(rdat$Tx == "PE", 1, 0),
      Y      = rdat$Pain,
      M      = rdat[[mediator]],
      L2id   = as.factor(rdat$ID),
      Md_w   = rdat$M_within,  # within-person mediator
      M_bar  = rdat$M_mean,  # between-person mean mediator
      time   = rdat$time     # 1️⃣ CHANGE 1: Include time variable
    )

    tmpb <- pivot_longer(tmpb, cols = c(Y, M), names_to = "Outcome", values_to = "Z")
    tmpb$Sy <- ifelse(tmpb$Outcome == "Y", 1, 0)
    tmpb$Sm <- ifelse(tmpb$Outcome == "M", 1, 0)

    # 6️⃣ CHANGE 6: Bootstrap effects (same as main analysis)
    tmpb$SmTx     <- tmpb$Sm * tmpb$Tx_bin      # baseline group gap
    tmpb$SyTx     <- tmpb$Sy * tmpb$Tx_bin      # c' path
    tmpb$SyMw     <- tmpb$Sy * tmpb$Md_w        # within-person mediator effect
    tmpb$SyMb     <- tmpb$Sy * tmpb$M_bar       # between-person mean control
    tmpb$Smt      <- tmpb$Sm * tmpb$time        # 1️⃣ CHANGE 1: time main effect
    tmpb$SmTxTime <- tmpb$Sm * tmpb$Tx_bin * tmpb$time  # 2️⃣ CHANGE 2: Tx×time interaction

    control <- lmeControl(opt = "optim", maxIter = 100, msMaxIter = 100)
    fitb <- tryCatch(
      lme(fixed = as.formula(fixed.formula),
          random = as.formula(random.formula),
          weights = varIdent(form = ~ 1 | Sm),
          data = tmpb,
          control = control),
      error = function(e) NULL)
    if (is.null(fitb)) return(rep(NA, 4))

    em <- tryCatch(extract.modmed.mlm(list(model = fitb, conv = TRUE)), error = function(e) NULL)
    if (is.null(em)) return(rep(NA, 4))

    # 6️⃣ CHANGE 6: Bootstrap return values (modified from original)
    # Original: a <- em[["SmTx"]], b <- em[["SyM"]]
    # Modified: Now returns a_change and within-person b-path
    a  <- em[["SmTxTime"]]  # 2️⃣ CHANGE 2: a_change (Tx×time interaction)
    b  <- em[["SyMw"]]      # 4️⃣ CHANGE 4: within-person b-path
    cp <- em[["SyTx"]]      # STARTING POINT: c' path (unchanged)
    ab <- a * b             # 5️⃣ CHANGE 5: indirect effect (a_change × b)
    c(a, b, cp, ab)
  }

  cat("### BOOTSTRAP ###\n")
  bootmlm <- boot(data = temp_data, statistic = bootout, R = 5000)
  print(bootmlm); cat("\n")

  # 6️⃣ CHANGE 6: Bootstrap confidence intervals (modified from original)
  # Original: ci_a for SmTx, ci_b for SyM
  # Modified: ci_a for SmTxTime (a_change), ci_b for SyMw (within-person b-path)
  ci_a  <- boot.ci(bootmlm, index = 1, type = "perc")  # 2️⃣ CHANGE 2: a_change (Tx×time)
  ci_b  <- boot.ci(bootmlm, index = 2, type = "perc")  # 4️⃣ CHANGE 4: within-person b-path
  ci_cp <- boot.ci(bootmlm, index = 3, type = "perc")  # STARTING POINT: c' path (unchanged)
  ci_ab <- boot.ci(bootmlm, index = 4, type = "perc")  # 5️⃣ CHANGE 5: indirect effect (a_change × b)

  cat("### BOOTSTRAP CIs ###\n")
  print(ci_a); print(ci_b); print(ci_cp); print(ci_ab); cat("\n")

  # 6️⃣ CHANGE 6: Bootstrap p-values (modified from original)
  boot_p <- function(boot_results, idx) {
    est <- na.omit(boot_results$t[, idx])
    2 * min(mean(est >= 0), mean(est <= 0))
  }
  p_a  <- boot_p(bootmlm, 1)  # 2️⃣ CHANGE 2: p-value for a_change (Tx×time)
  p_b  <- boot_p(bootmlm, 2)  # 4️⃣ CHANGE 4: p-value for within-person b-path
  p_cp <- boot_p(bootmlm, 3)  # STARTING POINT: p-value for c' path (unchanged)
  p_ab <- boot_p(bootmlm, 4)  # 5️⃣ CHANGE 5: p-value for indirect effect (a_change × b)

  # 6️⃣ CHANGE 6: Bootstrap p-values output (modified from original)
  cat("### BOOTSTRAP p-values ###\n")
  cat("a_change (Tx×time) p-value:", p_a, "\n")      # 2️⃣ CHANGE 2: a_change p-value
  cat("within-person b-path p-value:", p_b, "\n")    # 4️⃣ CHANGE 4: within-person b-path p-value
  cat("c'-path p-value:", p_cp, "\n")                 # STARTING POINT: c' path p-value (unchanged)
  cat("indirect effect (a_change × b) p-value:", p_ab, "\n\n")  # 5️⃣ CHANGE 5: indirect effect p-value

  results[[mediator]] <- list(
    initial_model_summary = summary(mod_med_tmp),
    extracted_effects = extract.model,
    boot = bootmlm,
    ci = list(a = ci_a, b = ci_b, cp = ci_cp, ab = ci_ab),
    p  = list(a = p_a, b = p_b, cp = p_cp, ab = p_ab)
  )
}

cat("========================================\n")
cat("FINAL SUMMARY - ALL MEDIATORS\n")
cat("========================================\n\n")

# 5️⃣ CHANGE 5: Final summary with both a-paths and indirect effects
for (mediator in mediators) {
  cat("MEDIATOR:", mediator, "\n")
  cat("a_base (SmTx):", results[[mediator]]$extracted_effects[["SmTx"]], "\n")           # baseline group difference
  cat("a_change (SmTxTime):", results[[mediator]]$extracted_effects[["SmTxTime"]], "\n") # difference-in-differences
  cat("b (SyMw):", results[[mediator]]$extracted_effects[["SyMw"]], "\n")                 # within-person b-path
  cat("c' (SyTx):", results[[mediator]]$extracted_effects[["SyTx"]], "\n")               # c' path
  cat("indirect_base (a_base × b):",
      results[[mediator]]$extracted_effects[["SmTx"]] * results[[mediator]]$extracted_effects[["SyMw"]], "\n")
  cat("indirect_change (a_change × b):",
      results[[mediator]]$extracted_effects[["SmTxTime"]] * results[[mediator]]$extracted_effects[["SyMw"]], "\n\n")
}

sink()
cat("Analysis complete! Results saved to model_mlmed_time.txt\n")

