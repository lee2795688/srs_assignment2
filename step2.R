library(ggplot2)
library(patchwork)
library(mice)
library(VIM)

# set the working directory to the location where the data file is

dat<-dget(file="NHANESvIDA.rda")
str(dat) # concise info on size and variables
dat$id <- NULL # delete observation id as not required
head(dat) # first six observations
summary(dat) # summary for all variables 

# ============================================================
# Step 2: Missing Data Exploration - 完整绘图代码
# ============================================================

# --- 2.2a 每个变量缺失比例条形图（替代aggr左半部分）---
miss_df <- data.frame(
  Variable = names(dat),
  Pct_Missing = colSums(is.na(dat)) / nrow(dat) * 100
)
miss_df <- miss_df[miss_df$Pct_Missing > 0, ]
miss_df$Variable <- factor(miss_df$Variable, 
                           levels = miss_df$Variable[order(miss_df$Pct_Missing)])

ggplot(miss_df, aes(x = Variable, y = Pct_Missing)) +
  geom_bar(stat = "identity", fill = "#E15759", alpha = 0.8, width = 0.6) +
  geom_text(aes(label = paste0(round(Pct_Missing, 1), "%")), 
            hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(title = "Percentage of Missing Values by Variable",
       x = NULL, y = "Missing (%)") +
  ylim(0, max(miss_df$Pct_Missing) * 1.2) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# --- 2.2b 缺失模式（md.pattern 保留原样即可，这个本身就够清晰）---
md.pattern(dat, rotate.names = TRUE)

# --- 2.3 Cholesterol missingness vs observed variables ---
dat$miss_chol <- factor(ifelse(is.na(dat$cholesterol), "Missing", "Observed"),
                        levels = c("Observed", "Missing"))

my_fill <- scale_fill_manual(values = c("Observed" = "#4E79A7", "Missing" = "#E15759"))

p1 <- ggplot(dat, aes(x = miss_chol, y = age, fill = miss_chol)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1, width = 0.5) +
  my_fill + labs(x = NULL, y = "Age", subtitle = "p = 0.518") +
  theme_minimal(base_size = 11) + theme(legend.position = "none")

p2 <- ggplot(dat, aes(x = miss_chol, y = bmi, fill = miss_chol)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1, width = 0.5) +
  my_fill + labs(x = NULL, y = "BMI", subtitle = "p = 0.147") +
  coord_cartesian(ylim = c(15, 60)) +  # 去掉极端离群值干扰
  theme_minimal(base_size = 11) + theme(legend.position = "none")

p3 <- ggplot(dat, aes(x = miss_chol, y = cpm, fill = miss_chol)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1, width = 0.5) +
  my_fill + labs(x = NULL, y = "CPM", subtitle = "p = 0.370") +
  theme_minimal(base_size = 11) + theme(legend.position = "none")

# Gender: 用分组条形图替代堆叠图，更直观
gender_chol <- as.data.frame(table(dat$gender, dat$miss_chol))
colnames(gender_chol) <- c("Gender", "Status", "Count")
gender_chol_pct <- gender_chol |>
  transform(Pct = ave(Count, Gender, FUN = function(x) x / sum(x) * 100))
gender_chol_miss <- gender_chol_pct[gender_chol_pct$Status == "Missing", ]

p4 <- ggplot(gender_chol_miss, aes(x = Gender, y = Pct, fill = Gender)) +
  geom_bar(stat = "identity", alpha = 0.8, width = 0.5) +
  scale_fill_manual(values = c("male" = "#4E79A7", "female" = "#E15759")) +
  labs(x = NULL, y = "Missing Rate (%)", subtitle = "p = 0.027*") +
  theme_minimal(base_size = 11) + theme(legend.position = "none")

(p1 | p2 | p3 | p4) +
  plot_annotation(title = "Cholesterol: Missing vs Observed",
                  theme = theme(plot.title = element_text(face = "bold", size = 13)))

# --- 2.3 Apolipoprotein missingness vs observed variables ---
dat$miss_apol <- factor(ifelse(is.na(dat$apoliprot), "Missing", "Observed"),
                        levels = c("Observed", "Missing"))

q1 <- ggplot(dat, aes(x = miss_apol, y = age, fill = miss_apol)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1, width = 0.5) +
  my_fill + labs(x = NULL, y = "Age", subtitle = "p = 0.526") +
  theme_minimal(base_size = 11) + theme(legend.position = "none")

q2 <- ggplot(dat, aes(x = miss_apol, y = bmi, fill = miss_apol)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1, width = 0.5) +
  my_fill + labs(x = NULL, y = "BMI", subtitle = "p = 0.035*") +
  coord_cartesian(ylim = c(15, 60)) +
  theme_minimal(base_size = 11) + theme(legend.position = "none")

q3 <- ggplot(dat, aes(x = miss_apol, y = cpm, fill = miss_apol)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1, width = 0.5) +
  my_fill + labs(x = NULL, y = "CPM", subtitle = "p = 0.997") +
  theme_minimal(base_size = 11) + theme(legend.position = "none")

gender_apol <- as.data.frame(table(dat$gender, dat$miss_apol))
colnames(gender_apol) <- c("Gender", "Status", "Count")
gender_apol_pct <- gender_apol |>
  transform(Pct = ave(Count, Gender, FUN = function(x) x / sum(x) * 100))
gender_apol_miss <- gender_apol_pct[gender_apol_pct$Status == "Missing", ]

q4 <- ggplot(gender_apol_miss, aes(x = Gender, y = Pct, fill = Gender)) +
  geom_bar(stat = "identity", alpha = 0.8, width = 0.5) +
  scale_fill_manual(values = c("male" = "#4E79A7", "female" = "#E15759")) +
  labs(x = NULL, y = "Missing Rate (%)", subtitle = "p = 0.873") +
  theme_minimal(base_size = 11) + theme(legend.position = "none")

(q1 | q2 | q3 | q4) +
  plot_annotation(title = "Apolipoprotein: Missing vs Observed",
                  theme = theme(plot.title = element_text(face = "bold", size = 13)))

# 清理
dat$miss_chol <- NULL
dat$miss_apol <- NULL

# === ggplot图用ggsave ===
# 先把图存到变量里，再保存
p_bar <- ggplot(miss_df, aes(x = Variable, y = Pct_Missing)) +
  geom_bar(stat = "identity", fill = "#E15759", alpha = 0.8, width = 0.6) +
  geom_text(aes(label = paste0(round(Pct_Missing, 1), "%")), 
            hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(title = "Percentage of Missing Values by Variable",
       x = NULL, y = "Missing (%)") +
  ylim(0, max(miss_df$Pct_Missing) * 1.2) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave("miss_bar.pdf", plot = p_bar, width = 8, height = 4)

# cholesterol missingness图
p_chol <- (p1 | p2 | p3 | p4) +
  plot_annotation(title = "Cholesterol: Missing vs Observed",
                  theme = theme(plot.title = element_text(face = "bold", size = 13)))
ggsave("miss_chol.pdf", plot = p_chol, width = 12, height = 4)

# apoliprotein missingness图
p_apol <- (q1 | q2 | q3 | q4) +
  plot_annotation(title = "Apolipoprotein: Missing vs Observed",
                  theme = theme(plot.title = element_text(face = "bold", size = 13)))
ggsave("miss_apol.pdf", plot = p_apol, width = 12, height = 4)

# === 基础R图用pdf() ===
# md.pattern是基础R图，不能用ggsave
pdf("mdpattern.pdf", width = 8, height = 6)
md.pattern(dat, rotate.names = TRUE)
dev.off()
