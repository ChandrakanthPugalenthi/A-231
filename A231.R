# libraries
library(ggplot2)
library(dplyr)

# dataset
data <- read.csv("C:/Users/wolfr/Downloads/2020 November General Election - Turnout Rates.csv")

data$State <- as.factor(data$State) 
data$VoterTurnoutRate <- as.numeric(sub("%", "", data$VEP.Turnout.Rate))

# Remove rows with missing values in VoterTurnoutRate
data <- data[!is.na(data$VoterTurnoutRate), ]

# Check for missing or invalid data
summary(data$VoterTurnoutRate)

# Histogram with updated bar formatting and density curve
ggplot(data, aes(x = VoterTurnoutRate)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "#4682B4", color = "black") + 
  geom_density(color = "red", size = 1) + # Add density curve in red
  labs(title = "Voter Turnout Rate Distribution with Density Curve",
       x = "Voter Turnout Rate (%)",
       y = "Density") +
  theme_minimal() 
