library(readxl)
library(lme4)
library(lmerTest)
library(performance)
library(ggplot2)
library(dplyr)

file_path <- "./ITT_data.xlsx"
data <- read_excel(file_path)

# Ensure correct data types
data$Pain <- as.numeric(data$Pain)
data$Tx <- relevel(factor(data$Tx), ref = "WL")
data$Group <- relevel(factor(data$Group), ref = "Videos")
data$Sex <- factor(data$Sex)
data$Age <- as.numeric(data$Age)
data$ID <- factor(data$ID)

# Subset
data_Pain <- data[!is.na(data$Pain), ]

# Output
sink("modelITT_output.txt")

cat("### PAIN MODELS (Tx) ###\n")

# Null model
null_model <- lmer(Pain ~ 1 + (1 | ID), data = data_Pain, REML = FALSE)
print(performance::icc(null_model))

# Time only
model1 <- lmer(Pain ~ Time + (1 | ID), data = data_Pain, REML = FALSE)
print(summary(model1))
print(performance::icc(model1))

# Tx model (ML)
model2_ML <- lmer(Pain ~ Time + Tx + Time*Tx + (1 | ID), data = data_Pain, REML = FALSE)
print(summary(model2_ML))
print(performance::icc(model2_ML))
print(r2(model2_ML))
print(anova(model1, model2_ML))

# Tx model (REML)
model2 <- lmer(Pain ~ Time + Tx + Time*Tx + (1 | ID), data = data_Pain, REML = TRUE)
print(summary(model2))
print(performance::icc(model2))
print(r2(model2))

cat("\nBootstrapped CIs for Pain ~ Tx:\n")
set.seed(123)
boot_results <- bootMer(model2, FUN = fixef, nsim = 5000, use.u = TRUE, type = "parametric")
boot_CI <- confint(boot_results, method = "boot", level = 0.95)
print(boot_CI)

# Residuals
res <- resid(model2)
qqnorm(res); qqline(res, col = "blue")
hist(res, breaks = 20, main = "Histogram of Residuals", xlab = "Residuals")

cat("\n### PAIN MODELS (Group) ###\n")

modelG <- lmer(Pain ~ Time + Group + Time*Group + (1 | ID), data = data_Pain, REML = TRUE)
print(summary(modelG))
print(performance::icc(modelG))

cat("\nBootstrapped CIs for Pain ~ Group:\n")
set.seed(123)
boot_results <- bootMer(modelG, FUN = fixef, nsim = 5000, use.u = TRUE, type = "parametric")
boot_CI <- confint(boot_results, method = "boot", level = 0.95)
print(boot_CI)

cat("\n### ADDING COVARIATES (Tx Models) ###\n")

# Add Sex
model2_sex <- lmer(Pain ~ Time + Tx + Time*Tx + Sex + (1 | ID), data = data_Pain, REML = TRUE)
cat("\nModel with Sex:\n")
print(summary(model2_sex))
print(anova(model2, model2_sex))

# Add Age
model2_age <- lmer(Pain ~ Time + Tx + Time*Tx + Age + (1 | ID), data = data_Pain, REML = TRUE)
cat("\nModel with Age:\n")
print(summary(model2_age))
print(anova(model2, model2_age))

# Add Sex + Age
model2_sex_age <- lmer(Pain ~ Time + Tx + Time*Tx + Sex + Age + (1 | ID), data = data_Pain, REML = TRUE)
cat("\nModel with Sex and Age:\n")
print(summary(model2_sex_age))
print(anova(model2, model2_sex_age))

cat("\n### ADDING COVARIATES (Group Models) ###\n")

# Add Sex
modelG_sex <- lmer(Pain ~ Time + Group + Time*Group + Sex + (1 | ID), data = data_Pain, REML = TRUE)
cat("\nGroup model with Sex:\n")
print(summary(modelG_sex))
print(anova(modelG, modelG_sex))

# Add Age
modelG_age <- lmer(Pain ~ Time + Group + Time*Group + Age + (1 | ID), data = data_Pain, REML = TRUE)
cat("\nGroup model with Age:\n")
print(summary(modelG_age))
print(anova(modelG, modelG_age))

# Add Sex + Age
modelG_sex_age <- lmer(Pain ~ Time + Group + Time*Group + Sex + Age + (1 | ID), data = data_Pain, REML = TRUE)
cat("\nGroup model with Sex and Age:\n")
print(summary(modelG_sex_age))
print(anova(modelG, modelG_sex_age))

sink()