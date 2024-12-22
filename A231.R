# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyverse)

# Load the dataset
data <- read.csv("C:/Users/wolfr/Downloads/2020 November General Election - Turnout Rates (2).csv")

# Inspect the data
head(data)
str(data)

# Clean and preprocess the VEP.Turnout.Rate column
# Remove the percentage sign and convert it to numeric
data$Voter_Turnout_Rate <- as.numeric(gsub("%", "", data$VEP.Turnout.Rate))

# Check for missing or invalid data
summary(data$Voter_Turnout_Rate)

# Verify the new column
head(data$Voter_Turnout_Rate)

# Research Question:
# "Is there a difference in the mean voter turnout rate (dependent interval variable) between states (independent nominal variable)?"

# Hypotheses:
# Null Hypothesis (H0): The average rate of voter turnout is not much different in every state.
# Alternative Hypothesis (H1): The average voter turnout rate differs across states.

# Checking if the data is normally distributed
ggplot(data, aes(x = Voter_Turnout_Rate)) +
  geom_histogram(bins = 20, fill = "blue", color = "black") +
  ggtitle("Histogram of Voter Turnout Rates") +
  xlab("Voter Turnout Rate") +
  ylab("Frequency")

# If data is not normally distributed, use a non-parametric test
# Perform Kruskal-Wallis Test (non-parametric equivalent of ANOVA)
kruskal_test <- kruskal.test(Voter_Turnout_Rate ~ State, data = data)
print(kruskal_test)

# If the p-value < 0.05, we reject the null hypothesis and conclude that
# there is a difference in voter turnout rates across states.

# Boxplot to visualize differences in voter turnout rates by state
ggplot(data, aes(x = State, y = Voter_Turnout_Rate)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  ggtitle("Boxplot of Voter Turnout Rates by State") +
  xlab("State") +
  ylab("Voter Turnout Rate") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Summary statistics by state
state_summary <- data %>%
  group_by(State) %>%
  summarise(
    Mean_Turnout = mean(Voter_Turnout_Rate, na.rm = TRUE),
    Median_Turnout = median(Voter_Turnout_Rate, na.rm = TRUE),
    SD_Turnout = sd(Voter_Turnout_Rate, na.rm = TRUE)
  )
print(state_summary)

# Save the summary statistics to a CSV file
write.csv(state_summary, "State_Turnout_Summary.csv", row.names = FALSE)

# Conclusion: Interpret results based on p-value and summary statistics.

