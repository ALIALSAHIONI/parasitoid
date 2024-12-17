
#THRASHING FREQUENCY OF ESCAPE AND NO ESCAPE OF M.MEDIATOR
# Load necessary libraries
library(ggplot2)   # For plotting
library(dplyr)     # For data manipulation
library(readxl)    # For reading Excel files
library(sandwich)  # For robust standard errors
library(lmtest)    # For robust standard errors

# Set file path (replace with your actual file path)
file_path <- "data/parasitoid.xlsx"

getwd()

# --- Filtering data for M. mediator (mm) and Escape vs No Escape ---
mediator_data_escape <- data %>% 
  filter(wasp.species == "mm") %>%
  select(host.weight, thrash.frequency, host.escape) %>%
  mutate(Escape = ifelse(host.escape == 1, "Escape", "No Escape"))

# --- Logistic Regression Model: Escape vs No Escape ---
# GLM for logistic regression (binary outcome: Escape vs No Escape)
logit_model_escape <- glm(host.escape ~ host.weight + thrash.frequency, 
                          data = mediator_data_escape, family = binomial(link = "logit"))

# --- Compute Robust Standard Errors ---
robust_se <- sqrt(diag(vcovHC(logit_model_escape, type = "HC3")))  # Using HC3 for robust standard errors

# Display the results with robust standard errors
coeftest(logit_model_escape, vcov = vcovHC(logit_model_escape, type = "HC3"))

# --- Predicted probabilities from the logistic regression model ---
mediator_data_escape$predicted_prob <- predict(logit_model_escape, type = "response")

# --- Plot predicted probabilities vs actual Escape status ---
ggplot(mediator_data_escape, aes(x = predicted_prob, fill = Escape)) +
  geom_histogram(position = "dodge", binwidth = 0.05, alpha = 0.7) + 
  labs(title = "Predicted Probability of Escape vs Actual Escape for M. mediator",
       x = "Predicted Probability", y = "Frequency") +
  scale_fill_manual(values = c("Escape" = "blue", "No Escape" = "red")) +
  theme_minimal()




# --- Filter data for M. mediator (mm) and Drop vs No Drop ---
mediator_data_drop <- data %>% 
  filter(wasp.species == "mm") %>%
  select(host.weight, thrash.frequency, host.drop) %>%
  mutate(Drop = ifelse(host.drop == 1, "Drop", "No Drop"))

# --- GLM for Drop vs No Drop ---
# Logistic regression model for Drop vs No Drop
logit_model_drop <- glm(host.drop ~ host.weight + thrash.frequency, 
                        data = mediator_data_drop, family = binomial(link = "logit"))

# Computing robust standard errors for the Drop model
robust_se_drop <- sqrt(diag(vcovHC(logit_model_drop, type = "HC3")))

# Displaying results with robust standard errors
coeftest(logit_model_drop, vcov = vcovHC(logit_model_drop, type = "HC3"))

# Predicted probabilities from the Drop vs No Drop model
mediator_data_drop$predicted_prob <- predict(logit_model_drop, type = "response")

# Ploting predicted probabilities vs actual Drop status
ggplot(mediator_data_drop, aes(x = predicted_prob, fill = Drop)) +
  geom_histogram(position = "dodge", binwidth = 0.05, alpha = 0.7) + 
  labs(title = "Predicted Probability of Drop vs Actual Drop for M. mediator",
       x = "Predicted Probability", y = "Frequency") +
  scale_fill_manual(values = c("Drop" = "blue", "No Drop" = "red")) +
  theme_minimal()



#THRASHING FREQUENCY OF DROP AND NO DROP OF M.MEDIATOR



library(ggplot2)   # For plotting
library(dplyr)     # For data manipulation
library(readxl)    # For reading Excel files
library(sandwich)  # For robust standard errors
library(lmtest)    # For robust standard errors

# Set file path (replace with your actual file path)
file_path <- "C:/Users/ogang/Downloads/rawdata.xlsx"

# Read the data from the Excel file
data <- read_excel(file_path)

# --- Convert handling.time to numeric if it's not already ---
data$host.handling.time <- as.numeric(data$host.handling.time)

# --- Filter data for the two parasitoid species ---
parasitoid_data <- data %>%
  filter(wasp.species %in% c("mm", "mp")) %>%
  select(host.weight, host.handling.time, wasp.species)

# --- Fit Generalized Linear Model (GLM) with Robust Standard Errors ---
glm_model <- glm(host.handling.time ~ host.weight + wasp.species, 
                 data = parasitoid_data, family = gaussian(link = "identity"))

# Compute robust standard errors using the sandwich package
robust_se <- sqrt(diag(vcovHC(glm_model, type = "HC3")))  # Using HC3 for robust standard errors

# --- Summary of the GLM with Robust Standard Errors ---
summary(glm_model)  # Standard summary
coeftest(glm_model, vcov = vcovHC(glm_model, type = "HC3"))  # Coefficients with robust SE

# --- Scatter Plot with Regression Line (GLM) ---
ggplot(parasitoid_data, aes(x = host.weight, y = host.handling.time, color = wasp.species)) +
  geom_point(size = 4) +  # Scatter plot with points
  geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE, linetype = "solid", size = 1) +  # GLM curve
  labs(title = "Handling Time vs Host Weight for Two Parasitoid Species (GLM)",
       x = "Host Weight (mg)", 
       y = "Handling Time (seconds)") +
  scale_color_manual(values = c("mm" = "blue", "mp" = "red")) +  # Custom colors for each species
  theme_minimal() + 
  theme(
    legend.title = element_blank(),  # Remove legend title
    legend.position = "top",         # Position the legend at the top
    panel.grid.major = element_line(color = "gray", size = 0.5),  # Gridlines
    panel.grid.minor = element_blank()   # Remove minor gridlines
  )



#HANDLING TIME VS hOST WEIGHT FOR THE TWO PARASITOID SPECIES

# Load necessary libraries
library(ggplot2)   # For plotting
library(dplyr)     # For data manipulation
library(readxl)    # For reading Excel files

# Set file path (replace with your actual file path)
file_path <- "C:/Users/ogang/Downloads/rawdata.xlsx"

# Read the data from the Excel file
data <- read_excel(file_path)

# --- Convert handling.time to numeric if it's not already ---
data$handling.time <- as.numeric(data$host.handling.time)

# --- Filter data for the two parasitoid species ---
parasitoid_data <- data %>%
  filter(wasp.species %in% c("mm", "mp")) %>%
  select(host.weight, host.handling.time, wasp.species)

# --- Scatter Plot with GLM Regression Line, Confidence Interval, and Gridlines ---
ggplot(parasitoid_data, aes(x = host.weight, y = host.handling.time, color = wasp.species)) +
  geom_point(size = 1) +  # Scatter plot with points
  geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE, linetype = "solid", size = 1) +  # GLM curve with CI
  labs(title = "Handling Time vs Host Weight for Two Parasitoid Species (GLM)",
       x = "Host Weight (mg)", 
       y = "Handling Time (seconds)") +
  scale_color_manual(values = c("mm" = "blue", "mp" = "red")) +  # Custom colors for each species
  theme_minimal() + 
  theme(
    legend.title = element_blank(),  # Remove legend title
    legend.position = "top",         # Position the legend at the top
    panel.grid.major = element_line(color = "gray", size = 0.5),  # Gridlines
    panel.grid.minor = element_blank()   # Remove minor gridlines
  )


#HANDLING TIME VS THRASHING FREQUENCY OF THE TWO PARASITOIDS SPECIES

# Loading the necessary libraries
library(ggplot2)   # For plotting
library(dplyr)     # For data manipulation
library(readxl)    # For reading Excel files

# Set file path (replace with your actual file path)
file_path <- "C:/Users/ogang/Downloads/rawdata.xlsx"

# Read the data from the Excel file
data <- read_excel(file_path)

# --- Convert handling.time and thrash.frequency to numeric if they're not already ---
data$host.handling.time <- as.numeric(data$host.handling.time)
data$thrash.frequency <- as.numeric(data$thrash.frequency)

# --- Filter data for the two parasitoid species ---
parasitoid_data <- data %>%
  filter(wasp.species %in% c("mm", "mp")) %>%
  select(thrash.frequency, host.handling.time, wasp.species)

# --- Scatter Plot with Regression Line (Handling Time vs Thrashing Frequency) ---
ggplot(parasitoid_data, aes(x = thrash.frequency, y = host.handling.time, color = wasp.species)) +
  geom_point(size = 1) +  # Scatter plot with points
  geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE, linetype = "solid", size = 1) +  # GLM curve with CI
  labs(title = "Handling Time vs Thrashing Frequency for Two Parasitoid Species",
       x = "Thrashing Frequency", 
       y = "Handling Time (seconds)") +
  scale_color_manual(values = c("mm" = "blue", "mp" = "red")) +  # Custom colors for each species
  theme_minimal() + 
  theme(
    legend.title = element_blank(),  # Remove legend title
    legend.position = "top",         # Position the legend at the top
    panel.grid.major = element_line(color = "gray", size = 0.5),  # Gridlines
    panel.grid.minor = element_blank()   # Remove minor gridlines
  )




#PROBABILITY OF STNGING VS HOST WEIGHT AND THRASHING FREQUENCY

# Load necessary libraries
#library(ggplot2)  # For basic plotting
#library(dplyr)    # For data manipulation
#library(readxl)   # For reading Excel files
#library(plotly)   # For creating 3D scatter plots

# Set file path (replace with your actual file path)
file_path <- "C:/Users/ogang/Downloads/rawdata.xlsx"

# Read the data from the Excel file
data <- read_excel(file_path)

# --- Logistic Regression for Stinging vs Host Weight and Thrashing Frequency (mp) ---
# Filter data for M. pulchricornis (mp) and select relevant columns
pulchricornis_data_sting <- data %>%
  filter(wasp.species == "mp") %>%
  select(host.weight, thrash.frequency, wasp.sting)

# Fit a logistic regression model to predict stinging (1 = stinging, 0 = no stinging)
logistic_model_pulchricornis_sting <- glm(wasp.sting ~ host.weight + thrash.frequency, 
                                          family = binomial(link = "logit"), data = pulchricornis_data_sting)

# Predict the probability of stinging (1) for different host weights and thrashing frequencies
pulchricornis_data_sting$predicted_prob <- predict(logistic_model_pulchricornis_sting, type = "response")

# --- Create 3D Scatter Plot for M. pulchricornis ---
plot_ly(pulchricornis_data_sting, x = ~host.weight, y = ~thrash.frequency, z = ~predicted_prob, 
        type = "scatter3d", mode = "markers", 
        marker = list(size = 5, color = ~predicted_prob, colorscale = "Viridis")) %>%
  layout(title = "3D Scatter Plot of Probability of Stinging vs Host Weight and Thrashing Frequency for M. pulchricornis",
         scene = list(
           xaxis = list(title = "Host Weight (mg)"),
           yaxis = list(title = "Thrashing Frequency"),
           zaxis = list(title = "Predicted Probability of Stinging")
         ))

file_path <- "C:/Users/ogang/Downloads/rawdata.xlsx"

# Read the data from the Excel file
data <- read_excel(file_path)

# --- Linear Regression for Stinging Intensity vs Host Weight and Thrashing Frequency ---
# Filter data for M. mediator (mm) and select relevant columns
mediator_data_sting <- data %>%
  filter(wasp.species == "mm") %>%
  select(host.weight, thrash.frequency, wasp.sting)

# Fit a linear regression model to predict stinging intensity (continuous outcome)
linear_model_sting <- lm(wasp.sting ~ host.weight + thrash.frequency, 
                         data = mediator_data_sting)

# Summary of the linear regression model
summary(linear_model_sting)

# Predict the stinging intensity (continuous value) for different host weights and thrashing frequencies
mediator_data_sting$predicted_intensity <- predict(linear_model_sting)

# --- Create 3D Scatter Plot ---
# Use plotly to create the 3D scatter plot
plot_ly(mediator_data_sting, x = ~host.weight, y = ~thrash.frequency, z = ~predicted_intensity, 
        type = "scatter3d", mode = "markers", 
        marker = list(size = 5, color = ~predicted_intensity, colorscale = "Viridis")) %>%
  layout(title = "3D Scatter Plot of Predicted Stinging Intensity vs Host Weight and Thrashing Frequency",
         scene = list(
           xaxis = list(title = "Host Weight (mg)"),
           yaxis = list(title = "Thrashing Frequency"),
           zaxis = list(title = "Predicted Stinging Intensity")
         ))



#INTENSITY OF THRASHING VS HOST WEIGHT OF THE TWO PARASITIC SPECIES

# Set file path (replace with your actual file path)
file_path <- "C:/Users/ogang/Downloads/rawdata.xlsx"

# Read the data from the Excel file
data <- read_excel(file_path)

# Filter data for M. mediator (mm) and M. pulchricornis (mp)
thrashing_data <- data %>%
  filter(wasp.species %in% c("mm", "mp")) %>%
  select(host.weight, thrash.frequency, wasp.species)

# Scatter plot of Thrashing Time (Frequency) vs Host Weight
ggplot(thrashing_data, aes(x = host.weight, y = thrash.frequency, color = wasp.species)) +
  geom_point(size = 1) +  # Scatter plot with points
  labs(title = "Intensity of Thrashing vs Host Weight",
       x = "Host Weight (mg)",
       y = "Thrashing Frequency (Time)") +
  scale_color_manual(values = c("mm" = "blue", "mp" = "red")) +  # Custom colors for each species
  theme_minimal()


ANOTHER RELATIONSHIP BETWEEN ESCAPE AND NO ESCAPE FOR M.MEDIATOR

# Set file path (replace with your actual file path)
file_path <- "C:/Users/ogang/Downloads/rawdata.xlsx"

# Read the data from the Excel file
data <- read_excel(file_path)

# --- Filter data for M. mediator (mm) and Escape vs No Escape ---
mediator_data_escape <- data %>% 
  filter(wasp.species == "mm") %>%
  select(host.weight, thrash.frequency, host.escape) %>%
  mutate(Escape = ifelse(host.escape == 1, "Escape", "No Escape"))

# --- Logistic Regression Model: Escape vs No Escape ---
# GLM for logistic regression (binary outcome: Escape vs No Escape)
logit_model_escape <- glm(host.escape ~ host.weight + thrash.frequency, 
                          data = mediator_data_escape, family = binomial(link = "logit"))

# --- Compute Robust Standard Errors ---
robust_se <- sqrt(diag(vcovHC(logit_model_escape, type = "HC3")))  # Using HC3 for robust standard errors

# Display the results with robust standard errors
coeftest(logit_model_escape, vcov = vcovHC(logit_model_escape, type = "HC3"))

# --- Predicted probabilities from the logistic regression model ---
mediator_data_escape$predicted_prob <- predict(logit_model_escape, type = "response")

# --- Plot predicted probabilities vs actual Escape status ---
ggplot(mediator_data_escape, aes(x = predicted_prob, fill = Escape)) +
  geom_histogram(position = "dodge", binwidth = 0.05, alpha = 0.7) + 
  labs(title = "Predicted Probability of Escape vs Actual Escape for M. mediator",
       x = "Predicted Probability", y = "Frequency") +
  scale_fill_manual(values = c("Escape" = "blue", "No Escape" = "red")) +
  theme_minimal()



#PROBABILITY OF DROP VS NO DROP


# --- GLM for Drop vs No Drop ---
# Logistic regression model for Drop vs No Drop
logit_model_drop <- glm(host.drop ~ host.weight + thrash.frequency, 
                        data = mediator_data_drop, family = binomial(link = "logit"))

# Compute robust standard errors for the Drop model
robust_se_drop <- sqrt(diag(vcovHC(logit_model_drop, type = "HC3")))

# Display results with robust standard errors
coeftest(logit_model_drop, vcov = vcovHC(logit_model_drop, type = "HC3"))

# Predicted probabilities from the Drop vs No Drop model
mediator_data_drop$predicted_prob <- predict(logit_model_drop, type = "response")

# Plot predicted probabilities vs actual Drop status
ggplot(mediator_data_drop, aes(x = predicted_prob, fill = Drop)) +
  geom_histogram(position = "dodge", binwidth = 0.05, alpha = 0.7) + 
  labs(title = "Predicted Probability of Drop vs Actual Drop for M. mediator",
       x = "Predicted Probability", y = "Frequency") +
  scale_fill_manual(values = c("Drop" = "blue", "No Drop" = "red")) +
  theme_minimal()








data <- read_excel(file_path)

# --- M. mediator (mm) - Escape vs No Escape ---
# Filter data for M. mediator (mm) and select relevant columns
mediator_data_escape <- data %>% 
  filter(wasp.species == "mm") %>%
  select(host.weight, thrash.frequency, host.escape) %>%
  mutate(Escape = ifelse(host.escape == 1, "Escape", "No Escape"))

# Create a histogram comparing host weight for Escape vs No Escape for M. mediator
ggplot(mediator_data_escape, aes(x = host.weight, fill = Escape)) +
  geom_histogram(binwidth = 3, position = "dodge", alpha = 0.7, color = "black") +  # Histogram for host weight
  labs(title = "Host Weight Distribution for M. mediator (Escape vs No Escape)",
       x = "Host Weight (mg)", y = "Frequency") +
  scale_fill_manual(values = c("Escape" = "blue", "No Escape" = "red")) +
  theme_minimal()

# --- M. pulchricornis (mp) - Escape vs No Escape ---
# Filter data for M. pulchricornis (mp) and select relevant columns
pulchricornis_data_escape <- data %>% 
  filter(wasp.species == "mp") %>%
  select(host.weight, thrash.frequency, host.escape) %>%
  mutate(Escape = ifelse(host.escape == 1, "Escape", "No Escape"))

# Create a histogram comparing host weight for Escape vs No Escape for M. pulchricornis
ggplot(pulchricornis_data_escape, aes(x = host.weight, fill = Escape)) +
  geom_histogram(binwidth = 3, position = "dodge", alpha = 0.7, color = "black") +  # Histogram for host weight
  labs(title = "Host Weight Distribution for M. pulchricornis (Escape vs No Escape)",
       x = "Host Weight (mg)", y = "Frequency") +
  scale_fill_manual(values = c("Escape" = "blue", "No Escape" = "red")) +
  theme_minimal()

# --- M. mediator (mm) - Drop vs No Drop ---
# Filter data for M. mediator (mm) and select relevant columns
mediator_data_drop <- data %>% 
  filter(wasp.species == "mm") %>%
  select(host.weight, thrash.frequency, host.drop) %>%
  mutate(Drop = ifelse(host.drop == 1, "Drop", "No Drop"))

# Create a histogram comparing host weight for Drop vs No Drop for M. mediator
ggplot(mediator_data_drop, aes(x = host.weight, fill = Drop)) +
  geom_histogram(binwidth = 3, position = "dodge", alpha = 0.7, color = "black") +  # Histogram for host weight
  labs(title = "Host Weight Distribution for M. mediator (Drop vs No Drop)",
       x = "Host Weight (mg)", y = "Frequency") +
  scale_fill_manual(values = c("Drop" = "green", "No Drop" = "orange")) +
  theme_minimal()

# --- M. pulchricornis (mp) - Drop vs No Drop ---
# Filter data for M. pulchricornis (mp) and select relevant columns
pulchricornis_data_drop <- data %>% 
  filter(wasp.species == "mp") %>%
  select(host.weight, thrash.frequency, host.drop) %>%
  mutate(Drop = ifelse(host.drop == 1, "Drop", "No Drop"))

# Create a histogram comparing host weight for Drop vs No Drop for M. pulchricornis
ggplot(pulchricornis_data_drop, aes(x = host.weight, fill = Drop)) +
  geom_histogram(binwidth = 3, position = "dodge", alpha = 0.7, color = "black") +  # Histogram for host weight
  labs(title = "Host Weight Distribution for M. pulchricornis (Drop vs No Drop)",
       x = "Host Weight (mg)", y = "Frequency") +
  scale_fill_manual(values = c("Drop" = "green", "No Drop" = "orange")) +
  theme_minimal()





#HOST WEIGHT DISTRIBUTION OF M PURCHICONIS(DROPVSNODROP$ ESCAPE VS NOESCAPE)


data <- read_excel(file_path)

# --- M. mediator (mm) - Escape vs No Escape ---
# Filter data for M. mediator (mm) and select relevant columns
mediator_data_escape <- data %>% 
  filter(wasp.species == "mm") %>%
  select(host.weight, thrash.frequency, host.escape) %>%
  mutate(Escape = ifelse(host.escape == 1, "Escape", "No Escape"))

# Create a histogram comparing host weight for Escape vs No Escape for M. mediator
ggplot(mediator_data_escape, aes(x = host.weight, fill = Escape)) +
  geom_histogram(binwidth = 3, position = "dodge", alpha = 0.7, color = "black") +  # Histogram for host weight
  labs(title = "Host Weight Distribution for M. mediator (Escape vs No Escape)",
       x = "Host Weight (mg)", y = "Frequency") +
  scale_fill_manual(values = c("Escape" = "blue", "No Escape" = "red")) +
  theme_minimal()

# --- M. pulchricornis (mp) - Escape vs No Escape ---
# Filter data for M. pulchricornis (mp) and select relevant columns
pulchricornis_data_escape <- data %>% 
  filter(wasp.species == "mp") %>%
  select(host.weight, thrash.frequency, host.escape) %>%
  mutate(Escape = ifelse(host.escape == 1, "Escape", "No Escape"))

# Create a histogram comparing host weight for Escape vs No Escape for M. pulchricornis
ggplot(pulchricornis_data_escape, aes(x = host.weight, fill = Escape)) +
  geom_histogram(binwidth = 3, position = "dodge", alpha = 0.7, color = "black") +  # Histogram for host weight
  labs(title = "Host Weight Distribution for M. pulchricornis (Escape vs No Escape)",
       x = "Host Weight (mg)", y = "Frequency") +
  scale_fill_manual(values = c("Escape" = "blue", "No Escape" = "red")) +
  theme_minimal()

# --- M. mediator (mm) - Drop vs No Drop ---
# Filter data for M. mediator (mm) and select relevant columns
mediator_data_drop <- data %>% 
  filter(wasp.species == "mm") %>%
  select(host.weight, thrash.frequency, host.drop) %>%
  mutate(Drop = ifelse(host.drop == 1, "Drop", "No Drop"))

# Create a histogram comparing host weight for Drop vs No Drop for M. mediator
ggplot(mediator_data_drop, aes(x = host.weight, fill = Drop)) +
  geom_histogram(binwidth = 3, position = "dodge", alpha = 0.7, color = "black") +  # Histogram for host weight
  labs(title = "Host Weight Distribution for M. mediator (Drop vs No Drop)",
       x = "Host Weight (mg)", y = "Frequency") +
  scale_fill_manual(values = c("Drop" = "green", "No Drop" = "orange")) +
  theme_minimal()

# --- M. pulchricornis (mp) - Drop vs No Drop ---
# Filter data for M. pulchricornis (mp) and select relevant columns
pulchricornis_data_drop <- data %>% 
  filter(wasp.species == "mp") %>%
  select(host.weight, thrash.frequency, host.drop) %>%
  mutate(Drop = ifelse(host.drop == 1, "Drop", "No Drop"))

# Create a histogram comparing host weight for Drop vs No Drop for M. pulchricornis
ggplot(pulchricornis_data_drop, aes(x = host.weight, fill = Drop)) +
  geom_histogram(binwidth = 3, position = "dodge", alpha = 0.7, color = "black") +  # Histogram for host weight
  labs(title = "Host Weight Distribution for M. pulchricornis (Drop vs No Drop)",
       x = "Host Weight (mg)", y = "Frequency") +
  scale_fill_manual(values = c("Drop" = "green", "No Drop" = "orange")) +
  theme_minimal()


# --- Convert handling.time and thrash.frequency to numeric if they're not already ---
data$host.handling.time <- as.numeric(data$host.handling.time)
data$thrash.frequency <- as.numeric(data$thrash.frequency)

# --- Filter data for the two parasitoid species ---
parasitoid_data <- data %>%
  filter(wasp.species %in% c("mm", "mp")) %>%
  select(thrash.frequency, host.handling.time, wasp.species)

# Convert species to a binary variable (0 = mm, 1 = mp)
parasitoid_data <- parasitoid_data %>%
  mutate(wasp.species_binary = ifelse(wasp.species == "mp", 1, 0))

# --- Logistic Regression Model ---
logit_model <- glm(wasp.species_binary ~ thrash.frequency + host.handling.time, 
                   data = parasitoid_data, 
                   family = binomial(link = "logit"))

# Summary of the logistic regression model
summary(logit_model)

# Predict the probability of being M. pulchricornis (mp)
parasitoid_data$predicted_prob <- predict(logit_model, type = "response")

# --- Scatter Plot with Predicted Probabilities ---
ggplot(parasitoid_data, aes(x = thrash.frequency, y = host.handling.time, color = predicted_prob)) +
  geom_point(size = 3, alpha = 0.7) +  # Scatter plot with points
  scale_color_gradient(low = "blue", high = "red", name = "Probability of mp") +
  labs(title = "Probability of M. pulchricornis vs Handling Time and Thrashing Frequency",
       x = "Thrashing Frequency", 
       y = "Handling Time (seconds)") +
  theme_minimal() +
  theme(
    legend.position = "right",       # Position the legend on the right
    panel.grid.major = element_line(color = "gray", size = 0.5),  # Gridlines
    panel.grid.minor = element_blank()   # Remove minor gridlines
  )


# --- Convert handling.time and thrash.frequency to numeric if they're not already ---
data$host.handling.time <- as.numeric(data$host.handling.time)
data$thrash.frequency <- as.numeric(data$thrash.frequency)

# --- Filter data for the two parasitoid species and prepare binary variable ---
parasitoid_data <- data %>%
  filter(wasp.species %in% c("mm", "mp")) %>%
  select(thrash.frequency, host.handling.time, wasp.species) %>%
  mutate(wasp.species_binary = ifelse(wasp.species == "mp", 1, 0))

# --- Logistic Regression Model ---
logit_model <- glm(wasp.species_binary ~ thrash.frequency + host.handling.time, 
                   data = parasitoid_data, 
                   family = binomial(link = "logit"))

# --- Predict the probability of being M. pulchricornis (mp) ---
parasitoid_data <- parasitoid_data %>%
  mutate(predicted_prob = predict(logit_model, newdata = parasitoid_data, type = "response"))

# --- Scatter Plot with Predicted Probabilities ---
ggplot(parasitoid_data, aes(x = thrash.frequency, y = host.handling.time, color = predicted_prob)) +
  geom_point(size = 3, alpha = 0.7) +  # Scatter plot with points
  scale_color_gradient(low = "blue", high = "red", name = "Probability of mp") +
  labs(title = "Probability of M. pulchricornis vs Handling Time and Thrashing Frequency",
       x = "Thrashing Frequency", 
       y = "Handling Time (seconds)") +
  theme_minimal() +
  theme(
    legend.position = "right",       # Position the legend on the right
    panel.grid.major = element_line(color = "gray", size = 0.5),  # Gridlines
    panel.grid.minor = element_blank()   # Remove minor gridlines
  )
# Ensure handling time and thrashing frequency are numeric
data$host.handling.time <- as.numeric(data$host.handling.time)
data$thrash.frequency <- as.numeric(data$thrash.frequency)

# Filter and prepare dataset
parasitoid_data <- data %>%
  filter(wasp.species %in% c("mm", "mp")) %>%
  select(thrash.frequency, host.handling.time, wasp.species) %>%
  mutate(wasp.species_binary = ifelse(wasp.species == "mp", 1, 0)) %>%
  filter(!is.na(thrash.frequency) & !is.na(host.handling.time))  # Remove rows with NA

# Logistic Regression Model
logit_model <- glm(wasp.species_binary ~ thrash.frequency + host.handling.time, 
                   data = parasitoid_data, 
                   family = binomial(link = "logit"))

# Add predicted probabilities to the dataset
parasitoid_data <- parasitoid_data %>%
  mutate(predicted_prob = predict(logit_model, newdata = parasitoid_data, type = "response"))

# Scatter Plot With Predicted Probabilities
ggplot(parasitoid_data, aes(x = thrash.frequency, y = host.handling.time, color = predicted_prob)) +
  geom_point(size = 3, alpha = 0.7) +  # Scatter plot with points
  scale_color_gradient(low = "blue", high = "red", name = "Probability of mp") +
  labs(title = "Probability of M. pulchricornis vs Handling Time and Thrashing Frequency",
       x = "Thrashing Frequency", 
       y = "Handling Time (seconds)") +
  theme_minimal() +
  theme(
    legend.position = "right",       # Position the legend on the right
    panel.grid.major = element_line(color = "gray", size = 0.5),  # Gridlines
    panel.grid.minor = element_blank()   # Remove minor gridlines
  )




