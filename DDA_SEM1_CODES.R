# Mutual Fund : Skill vs. Luck Analysis Using the Fama-French Model & FDR control
# Description: This script estimates alpha for mutual funds using Fama-French 3 and 4-factor models,
# assesses statistical significance, applies multiple testing correction, and visualizes the results.

# --- 1. Setup and Load Data ---

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(reshape2)


DATA <- read_xlsx("C:/Users/mayur/OneDrive/Desktop/DDA/DDA/DDA Final Data Set.xlsx")
head(DATA)

# --- 2. Estimating Alphas and p-values for Both Models ---

n_funds <- 77 # Number of funds
alphas_3f <- numeric(n_funds)
pvals_3f <- numeric(n_funds)
alphas_4f <- numeric(n_funds)
pvals_4f <- numeric(n_funds)

for (i in 1:n_funds) {
  Ri <- as.numeric(DATA[[paste0("R", i)]])
  RF <- as.numeric(DATA$RF)
  rit <- Ri - RF
  
  # 3-Factor Model
  model3 <- lm(rit ~ MKT + SMB + HML, data = DATA)
  alphas_3f[i] <- coef(summary(model3))["(Intercept)", "Estimate"]
  pvals_3f[i]  <- coef(summary(model3))["(Intercept)", "Pr(>|t|)"]
  
  # 4-Factor Model
  model4 <- lm(rit ~ MKT + SMB + HML + WML, data = DATA)
  alphas_4f[i] <- coef(summary(model4))["(Intercept)", "Estimate"]
  pvals_4f[i]  <- coef(summary(model4))["(Intercept)", "Pr(>|t|)"]
}
# Significant fund names

DATA2=read_xlsx("C:/Users/mayur/OneDrive/Desktop/DDA/DDA/DDA Final Data Set.xlsx",sheet = "NAMES")

fund_names <- DATA2$Name   
# for 3 factor
sig_indices_3f <- which(pvals_3f < 0.1)
significant_funds_3f <- fund_names[sig_indices_3f]
print(significant_funds_3f)

# for 4-factor model
sig_indices_4f <- which(pvals_4f < 0.05)
significant_funds_4f <- fund_names[sig_indices_4f]
print(significant_funds_4f)



# --- 3. Multiple Testing Correction (Benjamini-Hochberg FDR) ---

# 3-Factor FDR
fdr_3f <- p.adjust(pvals_3f, method = "BH")
n_sig_3f <- sum(fdr_3f < 0.5)

# 4-Factor FDR
fdr_4f <- p.adjust(pvals_4f, method = "BH")
n_sig_4f <- sum(fdr_4f < 0.05)

cat("Number of significant alphas (3-factor, FDR<0.05):", n_sig_3f, "\n")
cat("Number of significant alphas (4-factor, FDR<0.05):", n_sig_4f, "\n")


# --- 4. Visualizations ---

# a) P-Values by Fund
result_df <- data.frame(Fund = paste0("R", 1:n_funds),
                        p_value = pvals_3f,
                        Significance = ifelse(pvals_3f < 0.05, "Significant", "Not Significant"))

ggplot(result_df, aes(x = reorder(Fund, p_value), y = p_value, color = Significance)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  labs(title = "P-Values by Fund (3-Factor Model)", x = "Fund", y = "P-Value") +
  coord_flip() +
  scale_color_manual(values = c("Not Significant" = "gray", "Significant" = "blue")) +
  theme_minimal()

# FOR FAMA FRENCH FOUR FACTOR 
result_df <- data.frame(Fund = paste0("R", 1:n_funds),
                        p_value = pvals_4f,
                        Significance = ifelse(pvals_4f < 0.05, "Significant", "Not Significant"))

ggplot(result_df, aes(x = reorder(Fund, p_value), y = p_value, color = Significance)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  labs(title = "P-Values by Fund (3-Factor Model)", x = "Fund", y = "P-Value") +
  coord_flip() +
  scale_color_manual(values = c("Not Significant" = "gray", "Significant" = "blue")) +
  theme_minimal()

# b) Boxplot of Alphas by Fund Category 

fund_category <- c(rep("Contra",5), rep("LargeCap",5), rep("MidCap",10), ...) # length = n_funds


if (exists("fund_category") && length(fund_category) == n_funds) {
  df_alpha <- data.frame(Alpha = alphas_3f, Category = fund_category)
  ggplot(df_alpha, aes(x = Category, y = Alpha, fill = Category)) +
    geom_boxplot() +
    labs(title = "Alpha Estimates by Fund Category For 3 Factor Model", x = "Fund Category", y = "Alpha Estimate") +
    theme_minimal() +
    theme(legend.position = "none")
}


if (exists("fund_category") && length(fund_category) == n_funds) {
  df_alpha <- data.frame(Alpha = alphas_4f, Category = fund_category)
  ggplot(df_alpha, aes(x = Category, y = Alpha, fill = Category)) +
    geom_boxplot() +
    labs(title = "Alpha Estimates by Fund Category For 4 Factor Model", x = "Fund Category", y = "Alpha Estimate") +
    theme_minimal() +
    theme(legend.position = "none")
}

# c) Correlation Heatmap of Factors
cor_matrix <- cor(DATA[, c("SMB", "HML", "WML", "MKT")], use = "complete.obs")
melted_cor <- melt(cor_matrix)
ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Correlation Heatmap of Fama-French Factors", x = "", y = "") +
  theme_minimal() +
  coord_fixed()

# d) Q-Q plot for residuals (using the first fund)
Ri_example = as.numeric(DATA$R1)
RF_example = as.numeric(DATA$RF)
rit_example = Ri_example - RF_example
model_example = lm(rit_example ~ MKT + SMB + HML, data = DATA)
qqnorm(residuals(model_example))
qqline(residuals(model_example), col="red")

# --- End of Script ---

