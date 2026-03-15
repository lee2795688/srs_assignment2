# ============================================================
# Step 3: Descriptive Statistics & EDA
# ============================================================
library(ggplot2)
library(patchwork)
## install.packages("GGally")
library(GGally)

# 选择cholesterol作为响应变量，先去掉cholesterol缺失的行
dat_sub <- dat[!is.na(dat$cholesterol), ]
cat("Sample size after removing cholesterol NA:", nrow(dat_sub), "\n")

# 创建mvpa二分变量
dat_sub$mvpa_yn <- factor(ifelse(dat_sub$num_mvpa_bouts > 0, "Yes", "No"))

# --- 3.1 响应变量分布 ---
p_resp <- ggplot(dat_sub, aes(x = cholesterol)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, 
                 fill = "#4E79A7", alpha = 0.7) +
  geom_density(linewidth = 0.8) +
  labs(title = "Distribution of LDL-Cholesterol", x = "LDL-Cholesterol (mg/dL)", y = "Density") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
ggsave("eda_chol_dist.pdf", plot = p_resp, width = 6, height = 4)

# --- 3.2 连续预测变量 vs cholesterol 散点图 ---
p_age <- ggplot(dat_sub, aes(x = age, y = cholesterol)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "lm", color = "#E15759", se = FALSE) +
  labs(x = "Age", y = "Cholesterol") +
  theme_minimal(base_size = 11)

p_waist <- ggplot(dat_sub, aes(x = waist, y = cholesterol)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "lm", color = "#E15759", se = FALSE) +
  labs(x = "Waist (cm)", y = "Cholesterol") +
  theme_minimal(base_size = 11)

p_bmi <- ggplot(dat_sub, aes(x = bmi, y = cholesterol)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "lm", color = "#E15759", se = FALSE) +
  labs(x = "BMI", y = "Cholesterol") +
  theme_minimal(base_size = 11)

p_cpm <- ggplot(dat_sub, aes(x = cpm, y = cholesterol)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "lm", color = "#E15759", se = FALSE) +
  labs(x = "CPM", y = "Cholesterol") +
  theme_minimal(base_size = 11)

p_scatter <- (p_age | p_waist) / (p_bmi | p_cpm) +
  plot_annotation(title = "Cholesterol vs Continuous Predictors",
                  theme = theme(plot.title = element_text(face = "bold", size = 13)))
ggsave("eda_scatter.pdf", plot = p_scatter, width = 10, height = 8)

# --- 3.3 分类变量 vs cholesterol boxplot ---
p_gender <- ggplot(dat_sub, aes(x = gender, y = cholesterol, fill = gender)) +
  geom_boxplot(alpha = 0.7, width = 0.5) +
  scale_fill_manual(values = c("male" = "#4E79A7", "female" = "#E15759")) +
  labs(x = "Gender", y = "Cholesterol") +
  theme_minimal(base_size = 11) + theme(legend.position = "none")

p_ethnic <- ggplot(dat_sub, aes(x = ethnic, y = cholesterol, fill = ethnic)) +
  geom_boxplot(alpha = 0.7, width = 0.6) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Ethnicity", y = "Cholesterol") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 25, hjust = 1, size = 8))

p_educ <- ggplot(dat_sub, aes(x = educ, y = cholesterol, fill = educ)) +
  geom_boxplot(alpha = 0.7, width = 0.6) +
  scale_fill_brewer(palette = "Set3") +
  labs(x = "Education", y = "Cholesterol") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 25, hjust = 1, size = 8))

p_mvpa <- ggplot(dat_sub, aes(x = mvpa_yn, y = cholesterol, fill = mvpa_yn)) +
  geom_boxplot(alpha = 0.7, width = 0.5) +
  scale_fill_manual(values = c("No" = "#F28E2B", "Yes" = "#59A14F")) +
  labs(x = "MVPA Bouts (Yes/No)", y = "Cholesterol") +
  theme_minimal(base_size = 11) + theme(legend.position = "none")

p_cat <- (p_gender | p_ethnic) / (p_educ | p_mvpa) +
  plot_annotation(title = "Cholesterol by Categorical Predictors",
                  theme = theme(plot.title = element_text(face = "bold", size = 13)))
ggsave("eda_boxplots.pdf", plot = p_cat, width = 10, height = 8)

# --- 3.4 连续变量相关矩阵（检查共线性）---
cont_vars <- dat_sub[, c("cholesterol", "age", "waist", "bmi", "height", "cpm")]
p_corr <- ggpairs(cont_vars,
                  upper = list(continuous = wrap("cor", size = 3.5)),
                  lower = list(continuous = wrap("points", alpha = 0.2, size = 0.5)),
                  diag = list(continuous = wrap("densityDiag", alpha = 0.6))) +
  theme_minimal(base_size = 9)
ggsave("eda_corrmatrix.pdf", plot = p_corr, width = 10, height = 10)

# --- 3.5 数值摘要表 ---
cat("\n=== Summary of continuous variables ===\n")
cont_names <- c("age", "height", "bmi", "waist", "cholesterol", "cpm", "num_mvpa_bouts")
summ <- data.frame(
  Variable = cont_names,
  N = sapply(cont_names, function(v) sum(!is.na(dat_sub[[v]]))),
  Mean = sapply(cont_names, function(v) round(mean(dat_sub[[v]], na.rm = TRUE), 2)),
  SD = sapply(cont_names, function(v) round(sd(dat_sub[[v]], na.rm = TRUE), 2)),
  Median = sapply(cont_names, function(v) round(median(dat_sub[[v]], na.rm = TRUE), 2)),
  Min = sapply(cont_names, function(v) round(min(dat_sub[[v]], na.rm = TRUE), 2)),
  Max = sapply(cont_names, function(v) round(max(dat_sub[[v]], na.rm = TRUE), 2))
)
print(summ, row.names = FALSE)

cat("\n=== Frequency tables for categorical variables ===\n")
cat("\nGender:\n"); print(table(dat_sub$gender))
cat("\nEthnicity:\n"); print(table(dat_sub$ethnic))
cat("\nEducation:\n"); print(table(dat_sub$educ))
cat("\nMarital:\n"); print(table(dat_sub$marital))
cat("\nIncome:\n"); print(table(dat_sub$income, useNA = "ifany"))
cat("\nMVPA (Yes/No):\n"); print(table(dat_sub$mvpa_yn))

# --- 3.6 waist vs bmi 共线性检查 ---
cat("\nCorrelation between waist and bmi:", 
    round(cor(dat_sub$waist, dat_sub$bmi, use = "complete.obs"), 3), "\n")
cat("Correlation between waist and height:", 
    round(cor(dat_sub$waist, dat_sub$height, use = "complete.obs"), 3), "\n")
cat("Correlation between bmi and height:", 
    round(cor(dat_sub$bmi, dat_sub$height, use = "complete.obs"), 3), "\n")

