library(readxl)
library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(performance)

# Load data
file_path <- "./CLBPEO.xlsx"
data <- read_excel(file_path)
data$Group <- droplevels(relevel(factor(data$Group), ref = "Videos"))

# Output file
sink("modelMEPg_output.txt")

# List of variables
variables <- c("MeP_Balance", "MeP_chairst", "MeP_TUG", "MeP_Takai", "MeP_WT4", "MEP")

for (var in variables) {
  cat("\n### Analysis for", var, "###\n")
  
  # Remove NAs for this variable and drop unused factor levels
  data_temp <- droplevels(data[!is.na(data[[var]]), ])
  
  # Null model
  null_model <- lmer(as.formula(paste(var, "~ 1 + (1|ID)")), data = data_temp, REML = FALSE)
  cat("\n### Null Model ICC for", var, "###\n")
  print(performance::icc(null_model))
  
  # Time-only model
  model1 <- lmer(as.formula(paste(var, "~ Time + (1 | ID)")), data = data_temp, REML = FALSE)
  cat("\n### Model 1 (Time only) for", var, "###\n")
  print(summary(model1))
  print(performance::icc(model1))
  
  # Final model with Time + Group + interaction (REML = TRUE)
  model2 <- lmer(as.formula(paste(var, "~ Time + Group + Time*Group + (1 | ID)")), data = data_temp, REML = TRUE)
  cat("\n### Final Model (Group interaction, REML=TRUE) for", var, "###\n")
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
