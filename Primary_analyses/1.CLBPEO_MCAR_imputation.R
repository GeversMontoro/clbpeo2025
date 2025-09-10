# Set CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Load libraries
library(devtools)
library(BaylorEdPsych)
library(readxl)
library(naniar)
library(mice)

# Load the Excel file
data <- read_excel("./ITT_paindata.xlsx")

# MCAR Analysis
# Create a missingness indicator for PainPost
data$missing_painpost <- is.na(data$PainPost)

# Convert categorical variables to factors
data$Group <- as.factor(data$Group)
data$Tx <- as.factor(data$Tx)
data$Sex <- as.factor(data$Sex)

# Run logistic regression to predict missingness in PainPost
mcar_model <- glm(missing_painpost ~ Tx + Sex + Age + PainPre, 
                  data = data, 
                  family = binomial)

# Summarize the model to check for significant predictors of missingness
summary(mcar_model)

# Visualize missing data
vis_miss(data)

# Multiple Imputation
# Perform multiple imputation using MICE
imputed_data <- mice(data, m = 5, method = 'pmm', maxit = 50, seed = 123)

# Extract the imputed values for PainPost
imputed_values <- imputed_data$imp$PainPost

# Aggregate the imputed values (take the mean of all imputations)
aggregated_values <- rowMeans(as.data.frame(imputed_values))

# Replace missing values in the original dataset with the aggregated values
data$PainPost[is.na(data$PainPost)] <- aggregated_values

# Save the completed dataset to a CSV file
write.csv(data, "completed_data_aggregated.csv", row.names = FALSE)

# View the first rows of the completed dataset
head(data)
