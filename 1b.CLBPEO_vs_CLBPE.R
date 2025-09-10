library(readxl)
library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(performance)

# Load data
file_path <- "./CLBPEO_CLBPE.xlsx"
data <- read_excel(file_path)
data$Group <- relevel(factor(data$Group), ref = "In_person")

# Redirect output to console AND file
sink("model_remoteeffect_output.txt", split = TRUE)

cat("## Null Model\n")
null_model <- lmer(Pain ~ 1 + (1 | ID), data = data, REML = FALSE)
print(summary(null_model))
print(performance::icc(null_model))

cat("\n## Model 1 (Time only)\n")
model1 <- lmer(Pain ~ Time + (1 | ID), data = data, REML = FALSE)
print(summary(model1))
print(performance::icc(model1))

cat("\n## Model 2 (Time + Group + Time*Group, ML)\n")
modelPain1 <- lmer(Pain ~ Time + Group + Time*Group + (1 | ID), data = data, REML = FALSE)
print(summary(modelPain1))
print(performance::icc(modelPain1))
print(r2(modelPain1))

cat("\n## Model Comparison: Time vs. Time+Group\n")
print(anova(model1, modelPain1))

cat("\n## Final model with REML\n")
modelPain_final <- lmer(Pain ~ Time + Group + Time*Group + (1 | ID), data = data, REML = TRUE)
print(summary(modelPain_final))
print(performance::icc(modelPain_final))
print(r2(modelPain_final))

cat("\n## Bootstrapped Confidence Intervals\n")
set.seed(123)
boot_results <- bootMer(modelPain_final,
                        FUN = fixef,
                        nsim = 5000,
                        use.u = TRUE,
                        type = "parametric")

boot_CI <- confint(boot_results, method = "boot", level = 0.95)
print(boot_CI)

cat("\n## Residual Diagnostics\n")
residuals_level1 <- resid(modelPain1)
acf(residuals_level1, main = "ACF of Residuals")
qqnorm(residuals_level1); qqline(residuals_level1, col = "blue")
hist(residuals_level1, breaks = 20, main = "Histogram of Residuals", xlab = "Residuals")

# Close sink
sink()