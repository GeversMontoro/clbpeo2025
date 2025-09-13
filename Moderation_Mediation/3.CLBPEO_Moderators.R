# Load required libraries
library(readxl)
library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(performance)

# Load data
file_path <- "./CLBPEO.xlsx"
data <- read_excel(file_path)
data$Tx <- relevel(factor(data$Tx), ref = "WL")

# Identify continuous variables and scale them
continuous_vars <- c("Age", "BMI_BSL", "EDD_BSL", "SLD_BSL", "Other_pain", "SUD",
                     "Extrav", "Agreea", "Consci", "Neuro", "Open", "PCS_BSL",
                     "TSK_BSL", "TAI_BSL", "SAI_BSL", "ISI_BSL",
                     "BDI_BSL", "PILL_BSL", "RAPA_BSL", "MEP_BSL")

data[continuous_vars] <- lapply(data[continuous_vars], function(x) scale(x, center = TRUE, scale = FALSE))

# Set reference levels for categorical moderators
data$Sex <- relevel(factor(data$Sex), ref = "M")
data$Race <- relevel(factor(data$Race), ref = "White")
data$Education <- relevel(factor(data$Education), ref = "HS_CEGEP_Prof")
data$Income <- relevel(factor(data$Income), ref = "Over50K")
data$Job <- relevel(factor(data$Job), ref = "Employed")
data$Absenteeism <- relevel(factor(data$Absenteeism), ref = "No")
data$Smoking <- relevel(factor(data$Smoking), ref = "1")
data$Compensation <- relevel(factor(data$Compensation), ref = "No")
data$Opioid_use_BSL <- relevel(factor(data$Opioid_use_BSL), ref = "No")
data$Opioid_Hx <- relevel(factor(data$Opioid_Hx), ref = "Yes")
data$Leg_Pain <- relevel(factor(data$Leg_Pain), ref = "Yes")
data$Pain_duration <- relevel(factor(data$Pain_duration), ref = "over5Y")
data$Pain_Frequency <- relevel(factor(data$Pain_Frequency), ref = "100")
data$Opioid_use_BSL <- relevel(factor(data$Opioid_use_BSL), ref = "Yes")


# Define the list of moderators
moderators <- c("Sex", "Age", "Race", "Education", "Income", "Absenteeism", "Smoking",
                "Job", "Compensation", "Opioid_use_BSL", "ISI_BSL", "SLD_BSL", "BDI_BSL", 
                "RAPA_BSL", "Pain_duration", "BMI_BSL", "Opioid_Hx", "Leg_Pain", "SUD",  
                "Pain_Frequency", "Other_pain_BSL", "PILL_BSL", "EDD_BSL", "MEP_BSL", "Extrav",
                "Agreea", "Consci", "Neuro", "Open", "PCS_BSL", "TSK_BSL", "TAI_BSL", "SAI_BSL")

# Open a file connection for output
sink("modelModerator_output_inter.txt")

# Initialize a list to store model summaries and CIs
model_summaries <- list()
bootstrap_cis <- list()

# Loop over each moderator to fit the MLM and add interaction with Time:Tx
for (moderator in moderators) {
  # Define model formula with interaction term
  formula <- as.formula(paste("Pain ~ Time * Tx *", moderator, "+ (1 | ID)"))
  
  # Fit the model
  model <- lmer(formula, data = data, REML = TRUE)
  
  # Store the summary in the list
  model_summaries[[moderator]] <- summary(model)
  
  # Bootstrap confidence intervals
  boot_model <- bootMer(model, FUN = fixef, nsim = 5000, seed = 1234)  # Adjust nsim for more iterations
  cis <- confint(boot_model, level = 0.95)
  
  # Store bootstrap CIs
  bootstrap_cis[[moderator]] <- cis
  
  # Print the model summary and bootstrap CIs
  print(paste("Model Summary for Moderator:", moderator))
  print(model_summaries[[moderator]])
  print(paste("Bootstrap CIs for Moderator:", moderator))
  print(cis)
}

# Close the file connection
sink()
