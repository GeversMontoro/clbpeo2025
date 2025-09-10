library(readxl)
library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(performance)

file_path <- "./CLBPEO.xlsx"
data <- read_excel(file_path)
data$Group <- relevel(factor(data$Group), ref = "Videos")

# Output file
sink("modelPEg_output.txt")

# List of variables (no baseline variables here)
variables <- c("Balance", "Chairst30", "TUG", "TakaiP", "WT4S", "WT4F", "RAPA")

for (var in variables) {
  cat("\n### Analysis for", var, "###\n")
  
  # Ensure consistent dataset (no missing values in outcome or Group)
  data_temp <- data[!is.na(data[[var]]) & !is.na(data$Group), ]
  
  # Null model (REML = FALSE)
  null_model <- lmer(as.formula(paste(var, "~ 1 + (1|ID)")), data = data_temp, REML = FALSE)
  cat("\n### Null Model for", var, "###\n")
  print(performance::icc(null_model))
  
  # Time-only model (REML = FALSE)
  model1 <- lmer(as.formula(paste(var, "~ Time + (1 | ID)")), data = data_temp, REML = FALSE)
  cat("\n### Model 1 for", var, "###\n")
  print(summary(model1))
  print(performance::icc(model1))
  
  # Full model (REML = FALSE for comparison)
  model2_ML <- lmer(as.formula(paste(var, "~ Time + Group + Time*Group + (1 | ID)")), data = data_temp, REML = FALSE)
  cat("\n### Model 2 for", var, "(ML version for comparison) ###\n")
  print(summary(model2_ML))
  print(performance::icc(model2_ML))
  print(r2(model2_ML))
  
  # Model comparison
  cat("\n### Model Comparison (Time vs. Time+Group) for", var, "###\n")
  print(anova(model1, model2_ML))
  
  # Final model (REML = TRUE for reporting and bootstrapping)
  model2 <- lmer(as.formula(paste(var, "~ Time + Group + Time*Group + (1 | ID)")), data = data_temp, REML = TRUE)
  cat("\n### Final Model (REML=TRUE) for", var, "###\n")
  print(summary(model2))
  print(performance::icc(model2))
  print(r2(model2))
  
  # Bootstrapping
  cat("\n### Bootstrapped CIs for", var, "###\n")
  set.seed(123)
  boot_results <- tryCatch({
    bootMer(model2, 
            FUN = fixef,
            nsim = 5000,
            use.u = TRUE,
            type = "parametric")
  }, error = function(e) {
    cat("\nBootstrapping failed for", var, ":\n")
    print(e)
    return(NULL)
  })
  
  if (!is.null(boot_results)) {
    boot_CI <- tryCatch({
      confint(boot_results, method = "boot", level = 0.95)
    }, error = function(e) {
      cat("\nCI extraction failed for", var, ":\n")
      print(e)
      return(NULL)
    })
    
    if (!is.null(boot_CI)) {
      print(boot_CI)
    }
  }
}

# Close output
sink()