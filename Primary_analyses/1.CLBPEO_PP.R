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
data_Pain <- data[!is.na(data$Pain), ]

# Output file
sink("modelPP_output.txt")

cat("### PAIN MODELS (Tx) ###\n")

# Pain ~ Tx
null_model <- lmer(Pain ~ 1 + (1 | ID), data = data_Pain, REML = FALSE)
print(performance::icc(null_model))

model1 <- lmer(Pain ~ Time + (1 | ID), data = data_Pain, REML = FALSE)
print(summary(model1))
print(performance::icc(model1))

model2_ML <- lmer(Pain ~ Time + Tx + Time*Tx + (1 | ID), data = data_Pain, REML = FALSE)
print(summary(model2_ML))
print(performance::icc(model2_ML))
print(r2(model2_ML))

print(anova(model1, model2_ML))

model2 <- lmer(Pain ~ Time + Tx + Time*Tx + (1 | ID), data = data_Pain, REML = TRUE)
print(summary(model2))
print(performance::icc(model2))
print(r2(model2))

cat("\nBootstrapped CIs for Pain ~ Tx:\n")
set.seed(123)
boot_results <- bootMer(model2, FUN = fixef, nsim = 5000, use.u = TRUE, type = "parametric")
boot_CI <- confint(boot_results, method = "boot", level = 0.95)
print(boot_CI)

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

sink()
