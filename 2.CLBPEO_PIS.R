library(readxl)
library(lme4)
library(lmerTest)
library(performance)
library(ggplot2)
library(dplyr)

file_path <- "./CLBPEO.xlsx"
data <- read_excel(file_path)
data$Tx <- relevel(factor(data$Tx), ref = "WL")
data$Group <- relevel(factor(data$Group), ref = "Videos")

# Subsets
data_PIS <- data[!is.na(data$PIS), ]

# Output file
sink("modelPIS_output.txt")

cat("\n\n### PIS MODELS (Tx) ###\n")

null_model <- lmer(PIS ~ 1 + (1 | ID), data = data_PIS, REML = FALSE)
print(performance::icc(null_model))

model1 <- lmer(PIS ~ Time + (1 | ID), data = data_PIS, REML = FALSE)
print(summary(model1))
print(performance::icc(model1))

model2_ML <- lmer(PIS ~ Time + Tx + Time*Tx + (1 | ID), data = data_PIS, REML = FALSE)
print(summary(model2_ML))
print(performance::icc(model2_ML))
print(r2(model2_ML))

print(anova(model1, model2_ML))

model2 <- lmer(PIS ~ Time + Tx + Time*Tx + (1 | ID), data = data_PIS, REML = TRUE)
print(summary(model2))
print(performance::icc(model2))
print(r2(model2))

cat("\nBootstrapped CIs for PIS ~ Tx:\n")
set.seed(123)
boot_results <- bootMer(model2, FUN = fixef, nsim = 5000, use.u = TRUE, type = "parametric")
boot_CI <- confint(boot_results, method = "boot", level = 0.95)
print(boot_CI)

res <- resid(model2)
qqnorm(res); qqline(res, col = "blue")
hist(res, breaks = 20, main = "Histogram of Residuals", xlab = "Residuals")

cat("\n\n### PIS MODELS (Group) ###\n")

modelG <- lmer(PIS ~ Time + Group + Time*Group + (1 | ID), data = data_PIS, REML = TRUE)
print(summary(modelG))
print(performance::icc(modelG))

cat("\nBootstrapped CIs for PIS ~ Group:\n")
set.seed(123)
boot_results <- bootMer(modelG, FUN = fixef, nsim = 5000, use.u = TRUE, type = "parametric")
boot_CI <- confint(boot_results, method = "boot", level = 0.95)
print(boot_CI)

sink()