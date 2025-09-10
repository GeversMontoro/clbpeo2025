library(readxl)
library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(performance)

file_path <- "./CLBPEO.xlsx"
data <- read_excel(file_path)
data$Tx <- relevel(factor(data$Tx), ref = "WL")

# Output file
sink("modelQ_output.txt")

# List of variables
variables <- c("EDD", "SLD", "GeneralDisability", "Health_status", "BMI", 
               "Other_pain", "PCS", "TSK", "BDI", "SAI", "TAI", "ISI")

for (var in variables) {
  cat("\n### Analysis for", var, "###\n")
  
  # Subset complete cases for current variable
  data_temp <- data[!is.na(data[[var]]), ]
  
  # --- Null model (REML = FALSE) ---
  null_model <- lmer(as.formula(paste(var, "~ 1 + (1|ID)")), data = data_temp, REML = FALSE)
  cat("\n### Null Model ICC for", var, "###\n")
  print(performance::icc(null_model))
  
  # --- Model 1: Time only (REML = FALSE) ---
  model1 <- lmer(as.formula(paste(var, "~ Time + (1 | ID)")), data = data_temp, REML = FALSE)
  cat("\n### Model 1 Summary for", var, " (Time only) ###\n")
  print(summary(model1))
  print(performance::icc(model1))
  
  # --- Model 2: Time + Tx + Time*Tx (REML = FALSE, for comparison) ---
  model2_ML <- lmer(as.formula(paste(var, "~ Time + Tx + Time*Tx + (1 | ID)")), data = data_temp, REML = FALSE)
  cat("\n### Model 2 Summary for", var, " (for comparison) ###\n")
  print(summary(model2_ML))
  print(r2(model2_ML))
  print(performance::icc(model2_ML))
  
  # --- Model comparison ---
  cat("\n### Model Comparison (Model 1 vs Model 2) for", var, "###\n")
  print(anova(model1, model2_ML))
  
  # --- Final model (REML = TRUE, for reporting + bootstrapping) ---
  model2 <- lmer(as.formula(paste(var, "~ Time + Tx + Time*Tx + (1 | ID)")), data = data_temp, REML = TRUE)
  cat("\n### Final Model with REML=TRUE for", var, "###\n")
  print(summary(model2))
  print(r2(model2))
  print(performance::icc(model2))
  
  # --- Bootstrapped Confidence Intervals ---
  set.seed(123)
  boot_results <- tryCatch({
    bootMer(model2, 
            FUN = fixef,
            nsim = 5000,
            use.u = TRUE,
            type = "parametric")
  }, error = function(e) {
    cat("\n### Bootstrapping failed for", var, "###\n")
    print(e)
    return(NULL)
  })
  
  if (!is.null(boot_results)) {
    boot_CI <- tryCatch({
      confint(boot_results, method = "boot", level = 0.95)
    }, error = function(e) {
      cat("\n### CI extraction failed for", var, "###\n")
      print(e)
      return(NULL)
    })
    
    if (!is.null(boot_CI)) {
      cat("\n### Bootstrapped CIs for", var, "###\n")
      print(boot_CI)
    }
  }
}

# Close output
sink()