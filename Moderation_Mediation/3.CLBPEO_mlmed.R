####### Combined Multilevel Mediation Analysis: PCS and Chairst30 with Tx (PE vs WL) #########
### Includes main analysis (t-values, df) and bootstrap analysis (p-values) ###

# Install necessary packages
install.packages("devtools", repos = "https://cran.rstudio.com/")
install.packages("multilevelmediation", repos = "https://cran.rstudio.com/")
install.packages("readxl", repos = "https://cran.rstudio.com/")

library(devtools)
library("multilevelmediation")
library(tidyr)
library(nlme)
library(MCMCpack)
library(boot)
options(scipen=999) # remove R's notation

# Set seed for reproducibility
set.seed(1234)

# Import data
library(readxl)
data <- read_excel("./CLBPEO.xlsx")
data <- as.data.frame(data) # Convert to dataframe
data$ID <- as.factor(data$ID) # Set SubjectID as factor

# Remove rows with missing values only for essential columns (Pain)
data <- subset(data, !is.na(Pain))

# Set up binary predictor variable for Tx (1 for PE, 0 for WL)
data$Tx_bin <- ifelse(data$Tx == "PE", 1, 0)

# List of mediators to analyze
mediators <- c("PCS", "Chairst30")

# Open a file connection for comprehensive output
sink("model_mlmed_main.txt")
cat("=== COMBINED MULTILEVEL MEDIATION ANALYSIS: PCS and Chairst30 ===\n")
cat("Seed set to: 1234 for reproducibility\n")
cat("Includes main analysis (t-values, df) and bootstrap analysis (p-values)\n\n")

# Initialize a list to store results for each mediator
results <- list()

# Loop over each mediator and perform the analysis
for (mediator in mediators) {
  
  cat("========================================\n")
  cat("PROCESSING MEDIATOR:", mediator, "\n")
  cat("========================================\n\n")
  
  # Create a temporary dataset that removes rows with NA for the current mediator
  temp_data <- subset(data, !is.na(data[[mediator]]))
  
  # Mean-center the mediator variable in the temporary dataset
  temp_data$M_centered <- scale(temp_data[[mediator]], scale = FALSE)
  
  # Prepare data for modeling
  tmp <- data.frame(
    Tx_bin = temp_data$Tx_bin,
    Y = temp_data$Pain,
    M = temp_data[[mediator]],       # Use the current mediator
    L2id = temp_data$ID,
    Md = temp_data$M_centered        # Mean-centered mediator
  )
  
  # Restructure data for modmed function
  tmp <- pivot_longer(tmp, cols = c(Y, M), names_to = "Outcome", values_to = "Z")
  
  # Create variables as per Bauer et al. syntax
  tmp$Sy <- ifelse(tmp$Outcome == "Y", 1, 0)
  tmp$Sm <- ifelse(tmp$Outcome == "M", 1, 0)
  tmp$SmTx <- tmp$Sm * tmp$Tx_bin
  tmp$SyTx <- tmp$Sy * tmp$Tx_bin
  tmp$SyM <- tmp$Sy * tmp$Md
  
  # Define the fixed and random formulas
  fixed.formula <- "Z ~ 0 + Sm + Sy + SmTx + SyTx + SyM"
  random.formula <- "~ 0 + Sm + Sy | L2id"
  
  # Fit the initial model and display summary
  control <- lmeControl(opt = "optim", maxIter = 100, msMaxIter = 100)
  mod_med_tmp <- lme(
    fixed = as.formula(fixed.formula),
    random = as.formula(random.formula),
    weights = varIdent(form = ~ 1 | Sm),
    data = tmp,
    control = control
  )
  
  # Print the summary of the initial model for the current mediator (INCLUDES T-VALUES AND DF)
  cat("### MAIN ANALYSIS - Model Summary for Mediator:", mediator, "###\n")
  cat("(This includes t-values, degrees of freedom, and p-values from the original model)\n\n")
  print(summary(mod_med_tmp))
  cat("\n")
  
  # Extract initial model effects for indirect paths
  extract.model <- extract.modmed.mlm(list(model = mod_med_tmp, conv = TRUE))
  initial_indirect <- extract.model[["SmTx"]] * extract.model[["SyM"]]  # Calculate initial indirect effect for Tx_bin
  
  # Print extracted effects
  cat("### EXTRACTED EFFECTS for Mediator:", mediator, "###\n")
  cat("a-path (Treatment -> Mediator):", extract.model[["SmTx"]], "\n")
  cat("b-path (Mediator -> Pain):", extract.model[["SyM"]], "\n")
  cat("c'-path (Direct effect):", extract.model[["SyTx"]], "\n")
  cat("ab-path (Indirect effect):", initial_indirect, "\n\n")
  
  # Define the bootstrap function
  bootout = function(data, indices) {
    # Bootstrap sample
    rdat <- data[indices, ]
    
    # Prepare data for bootstrap model
    tmp <- data.frame(
      Tx_bin = ifelse(rdat$Tx == "PE", 1, 0),
      Y = rdat$Pain,
      M = rdat[[mediator]],  # Use current mediator in bootstrapping as well
      L2id = as.factor(rdat$ID),
      Md = scale(rdat[[mediator]], scale = FALSE) # Mean-center the mediator in the bootstrap sample
    )
    
    tmp <- pivot_longer(tmp, cols = c(Y, M), names_to = "Outcome", values_to = "Z")
    tmp$Sy <- ifelse(tmp$Outcome == "Y", 1, 0)
    tmp$Sm <- ifelse(tmp$Outcome == "M", 1, 0)
    tmp$SmTx <- tmp$Sm * tmp$Tx_bin
    tmp$SyTx <- tmp$Sy * tmp$Tx_bin
    tmp$SyM <- tmp$Sy * tmp$Md
    
    # Fit the model with error handling
    control <- lmeControl(opt = "optim", maxIter = 100, msMaxIter = 100)
    mod_med_tmp <- tryCatch({
      lme(fixed = as.formula(fixed.formula),
          random = as.formula(random.formula),
          weights = varIdent(form = ~ 1 | Sm),
          data = tmp,
          control = control)
    }, error = function(e) {
      message("Convergence issue: ", e$message)
      return(NULL)
    })
    
    if (is.null(mod_med_tmp) || inherits(mod_med_tmp, "try-error")) {
      return(rep(NA, 4))
    }
    
    extract.model <- tryCatch({
      extract.modmed.mlm(list(model = mod_med_tmp, conv = TRUE))
    }, error = function(e) {
      message("Error extracting model: ", e$message)
      return(rep(NA, 4))
    })
    
    # Return coefficients
    a_path <- extract.model[["SmTx"]]
    b_path <- extract.model[["SyM"]]
    c_prime <- extract.model[["SyTx"]]
    indirect <- a_path * b_path
    
    return(c(a_path, b_path, c_prime, indirect))
  }
  
  # Perform bootstrapping
  cat("### BOOTSTRAP ANALYSIS for Mediator:", mediator, "###\n")
  cat("(This provides bootstrap p-values and confidence intervals)\n\n")
  bootmlm <- boot(data = temp_data, statistic = bootout, R = 5000)
  print(bootmlm)
  cat("\n")
  
  # Confidence intervals for paths
  ci_a_path <- boot.ci(bootmlm, index = 1, type = "perc")
  ci_b_path <- boot.ci(bootmlm, index = 2, type = "perc")
  ci_c_prime <- boot.ci(bootmlm, index = 3, type = "perc")
  ci_indirect <- boot.ci(bootmlm, index = 4, type = "perc")
  
  # Print confidence intervals for each mediator
  cat("### BOOTSTRAP CONFIDENCE INTERVALS for Mediator:", mediator, "###\n")
  cat("a-path CI:\n")
  print(ci_a_path)
  cat("\nb-path CI:\n")
  print(ci_b_path)
  cat("\nc'-path CI:\n")
  print(ci_c_prime)
  cat("\nIndirect effect CI:\n")
  print(ci_indirect)
  cat("\n")
  
  # Calculate bootstrap p-values for all paths
  boot_mlm_pvalue <- function(boot_results, param_index) {
    estimates <- na.omit(boot_results$t[, param_index])
    p_value <- 2 * min(mean(estimates >= 0), mean(estimates <= 0))
    return(p_value)
  }
  
  p_value_a_path <- boot_mlm_pvalue(bootmlm, 1)
  p_value_b_path <- boot_mlm_pvalue(bootmlm, 2)
  p_value_c_prime <- boot_mlm_pvalue(bootmlm, 3)
  p_value_indirect <- boot_mlm_pvalue(bootmlm, 4)
  
  # Print bootstrap p-values
  cat("### BOOTSTRAP P-VALUES for Mediator:", mediator, "###\n")
  cat("a-path p-value:", p_value_a_path, "\n")
  cat("b-path p-value:", p_value_b_path, "\n")
  cat("c'-path p-value:", p_value_c_prime, "\n")
  cat("ab-path (indirect) p-value:", p_value_indirect, "\n\n")
  
  # Store results for each mediator
  results[[mediator]] <- list(
    initial_model_summary = summary(mod_med_tmp),
    extracted_effects = extract.model,
    bootstrapping_results = bootmlm,
    ci_a_path = ci_a_path,
    ci_b_path = ci_b_path,
    ci_c_prime = ci_c_prime,
    ci_indirect = ci_indirect,
    bootstrap_p_values = list(
      a_path = p_value_a_path,
      b_path = p_value_b_path,
      c_prime = p_value_c_prime,
      indirect = p_value_indirect
    )
  )
  
  cat("========================================\n")
  cat("COMPLETED ANALYSIS FOR:", mediator, "\n")
  cat("========================================\n\n")
}

# Final summary
cat("========================================\n")
cat("FINAL SUMMARY - ALL MEDIATORS\n")
cat("========================================\n\n")

for (mediator in mediators) {
  cat("MEDIATOR:", mediator, "\n")
  cat("a-path coefficient:", results[[mediator]]$extracted_effects[["SmTx"]], "\n")
  cat("b-path coefficient:", results[[mediator]]$extracted_effects[["SyM"]], "\n")
  cat("c'-path coefficient:", results[[mediator]]$extracted_effects[["SyTx"]], "\n")
  cat("ab-path (indirect) coefficient:", results[[mediator]]$extracted_effects[["SmTx"]] * results[[mediator]]$extracted_effects[["SyM"]], "\n")
  cat("Bootstrap p-values - a:", results[[mediator]]$bootstrap_p_values$a_path, "b:", results[[mediator]]$bootstrap_p_values$b_path, "c':", results[[mediator]]$bootstrap_p_values$c_prime, "ab:", results[[mediator]]$bootstrap_p_values$indirect, "\n\n")
}

sink() # Close sink connection
cat("Combined analysis complete! Results saved to model_mlmed_main.txt\n")