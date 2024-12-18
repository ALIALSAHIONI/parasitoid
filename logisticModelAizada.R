
#code 1. This script analyzes the relationship between host weight, 
# thrashing frequency, and escape probability for M. mediator 
# using logistic regression.

# Load libraries
library(ggplot2)  # Visualization
library(dplyr)    # Data manipulation
library(readxl)   # For reading Excel files

# Load the data
file_path <- "data/parasitoid.xlsx"
df <- read_excel(file_path)

# Inspect the data
head(df)
str(df)
colnames(df)

# Convert 'host.escape' to factor
df$host.escape <- as.factor(df$host.escape)  # Replace with correct column name if needed

# Fit logistic regression
model <- glm(host.escape ~ host.weight + thrash.frequency, 
             data = df, family = binomial)

# View model summary
summary(model)

# Calculate odds ratios
exp(coef(model))

# Plot the relationship
ggplot(df, aes(x = host.weight, y = as.numeric(as.character(host.escape)), 
               color = thrash.frequency)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(title = "Escape Probability vs Host Weight & Thrashing Frequency",
       x = "Host Weight",
       y = "Escape Probability") +
  theme_minimal()





#code 2 A logistic regression analysis was conducted to evaluate 
# the relationship between host weight, thrashing frequency, 
# and the likelihood of a host exhibiting drop behavior in M. mediator.

# Load necessary libraries
library(readxl)   # For reading Excel files
library(ggplot2)  # For visualization
library(dplyr)    # For data manipulation

# Load the data
file_path <- "data/parasitoid.xlsx"
df <- read_excel(file_path)

# Step 2: Inspect the data structure
head(df)
str(df)
colnames(df)

# Step 3: Data Preparation
# Assuming the column `drop` represents the binary drop behavior (0 = no drop, 1 = drop)
# Ensure it's a factor
df$host.drop <- as.factor(df$host.drop)

# Ensure other predictors (host_weight and thrashing_frequency) are numeric
df$host.weight <- as.numeric(df$host.weight)
df$thrash.frequency <- as.numeric(df$thrash.frequency)

# Step 4: Logistic Regression Model
# Model drop behavior based on host weight and thrashing frequency
model <- glm(host.drop ~ host.weight + thrash.frequency, 
             data = df, family = binomial)

# Step 5: Model Summary
summary(model)

# Step 6: Odds Ratios
# Convert coefficients to odds ratios for interpretation
odds_ratios <- exp(coef(model))
print("Odds Ratios:")
print(odds_ratios)

# Step 7: Visualization
# Plot the relationship between host weight and likelihood of drop behavior
ggplot(df, aes(x = host.weight, y = as.numeric(as.character(host.drop)), 
                     color = thrash.frequency)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(title = "Drop Behavior vs Host Weight & Thrashing Frequency",
       x = "Host Weight (mg)",
       y = "Probability of Drop Behavior") +
  theme_minimal()

