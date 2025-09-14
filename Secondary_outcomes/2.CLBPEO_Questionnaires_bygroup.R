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
sink("modelQg_output.txt")

# List of variables to analyze
variables <- c("EDD", "SLD", "GeneralDisability", "Health_status", "BMI", 
               "Other_pain", "PCS", "TSK", "BDI", "SAI", "TAI", "ISI")

for (var in variables) {
  cat("\n### Analysis for", var, "###\n")
  
  # Subset data: remove NA in outcome and predictors
  data_temp <- data[!is.na(data[[var]]) & !is.na(data$Time) & !is.na(data$Group), ]
  
  # Null model
  null_model <- lmer(as.formula(paste(var, "~ 1 + (1|ID)")), data = data_temp, REML = FALSE)
  cat("\n### Null Model ICC for", var, "###\n")
  print(performance::icc(null_model))
  
  # Model 1: Time only
  model1 <- lmer(as.formula(paste(var, "~ Time + (1 | ID)")), data = data_temp, REML = FALSE)
  cat("\n### Model 1 Summary for", var, " (Time only) ###\n")
  print(summary(model1))
  print(performance::icc(model1))
  
  # Model 2: Time + Group + Time*Group (final model with REML = TRUE)
  model2 <- lmer(as.formula(paste(var, "~ Time + Group + Time*Group + (1 | ID)")), data = data_temp, REML = TRUE)
  cat("\n### Model 2 Summary for", var, " (Time + Group + Interaction) ###\n")
  print(summary(model2))
  cat("\nR2 Values for", var, ":\n")
  print(r2(model2))
  print(performance::icc(model2))
  
  # Bootstrapping
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

# Close output file
sink()
