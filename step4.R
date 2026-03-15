# ============================================================
# Step 4: Linear Modelling
# ============================================================
library(car)  # for vif()

# 确保mvpa_yn已创建
dat_sub$mvpa_yn <- factor(ifelse(dat_sub$num_mvpa_bouts > 0, "Yes", "No"))

# --- 4.1 拟合线性模型 ---
fit <- lm(cholesterol ~ gender + age + ethnic + educ + waist + cpm + mvpa_yn,
          data = dat_sub)

# 模型摘要
summary(fit)

# 完整案例数
cat("Number of complete cases used in model:", nobs(fit), "\n")

# --- 4.2 模型诊断 ---
# ggplot风格的诊断图
## install.packages("ggfortify")
library(ggfortify)
autoplot(fit, which = 1:4, ncol = 2,
         colour = "#4E79A7", alpha = 0.4,
         smooth.colour = "#E15759",
         label.n = 3, label.size = 3, label.repel = TRUE) +
  theme_minimal(base_size = 10)
p_diag <- autoplot(fit, which = 1:4, ncol = 2,
                   colour = "#4E79A7", alpha = 0.4,
                   smooth.colour = "#E15759",
                   label.n = 3, label.size = 3, label.repel = TRUE) +
  theme_minimal(base_size = 10)
ggsave("model_diag.pdf", plot = p_diag, width = 10, height = 8)


# --- 4.3 VIF检查共线性 ---
cat("\n=== Variance Inflation Factors ===\n")
print(vif(fit))
# 注意：因子变量会显示GVIF，看GVIF^(1/(2*Df))列，>2有问题，>5严重

# --- 4.4 系数表提取（方便写报告）---
coef_table <- summary(fit)$coefficients
coef_df <- data.frame(
  Term = rownames(coef_table),
  Estimate = round(coef_table[, 1], 3),
  SE = round(coef_table[, 2], 3),
  t_value = round(coef_table[, 3], 3),
  p_value = round(coef_table[, 4], 4)
)
print(coef_df, row.names = FALSE)

# R-squared
cat("\nR-squared:", round(summary(fit)$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(summary(fit)$adj.r.squared, 4), "\n")
cat("Residual SE:", round(summary(fit)$sigma, 3), "\n")

# --- 4.5 置信区间 ---
cat("\n=== 95% Confidence Intervals ===\n")
print(round(confint(fit), 3))

