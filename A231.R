# Load necessary libraries
library(ggplot2)
library(dplyr)
library(car)

# Load the dataset
data <- read.csv("C:/Users/bikas/OneDrive/Desktop/2020 November General Election - Turnout Rates.csv")

# Data Cleaning
# Remove percentage symbol and convert VEP Turnout Rate to numeric
data$VEP_Turnout_Rate <- as.numeric(gsub("%", "", data$VEP.Turnout.Rate))

# Filter out any rows with missing or non-numeric values in VEP Turnout Rate
clean_data <- data %>% filter(!is.na(VEP_Turnout_Rate))

table(clean_data$State, useNA = "ifany")


# 1. Summary Table of Variables
summary_table <- clean_data %>% select(State, VEP_Turnout_Rate)
print(summary_table)

# 2. Boxplot: Voter Turnout Rate by State
ggplot(clean_data, aes(x = State, y = VEP_Turnout_Rate)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = "Voter Turnout Rate by State (2020)",
    x = "State",
    y = "Voter Turnout Rate (%)"
  )

# 3. Histogram with Normal Curve Overlay
ggplot(clean_data, aes(x = VEP_Turnout_Rate)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "skyblue", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  labs(
    title = "Distribution of Voter Turnout Rates",
    x = "Voter Turnout Rate (%)",
    y = "Density"
  )

# 4. One-Way ANOVA
anova_result <- aov(VEP_Turnout_Rate ~ State, data = clean_data)

# Print complete ANOVA summary
anova_summary <- summary(anova_result)
print(anova_summary)

# Extract F-value and p-value
f_value <- anova_summary[[1]][["F value"]][1]
p_value <- anova_summary[[1]][["Pr(>F)"]][1]

cat("\nF-value:", f_value, "\n")
cat("P-value:", p_value, "\n")


# Check for issues with the model fitting
anova_result


# Residual Variance
residual_summary <- anova_summary[[1]]  # Access ANOVA details
print(residual_summary)

# Extract residual statistics
residual_ss <- residual_summary[["Sum Sq"]][2]
residual_df <- residual_summary[["Df"]][2]
mean_square_residual <- residual_ss / residual_df

cat("\nResidual Sum of Squares:", residual_ss, "\n")
cat("Residual Degrees of Freedom:", residual_df, "\n")
cat("Mean Square Residual:", mean_square_residual, "\n")

