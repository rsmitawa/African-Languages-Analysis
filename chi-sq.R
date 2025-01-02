# Answer - 2

# Load necessary libraries
library(tidyverse)
library(readxl)

# Set the working directory (adjust path if necessary)
setwd('~/Data-Science/R-Projects/African-Languages-Analysis/')

# Load the dataset
main_df <- read_excel("African-Language-Dataset-Cleaned.xlsx", sheet = 1)

# Define parameters for analysis
language_columns <- c(
  "mother_tongue",
  "language_with_spouse",
  "language_with_children",
  "friends_speak_native_language",
  "friends_african_immigrants"
)

# Split the dataset by gender
male_data <- main_df %>% filter(gender == 1)
female_data <- main_df %>% filter(gender == 2)

# Function to perform Chi-square test on each language parameter
perform_chisq_test <- function(column) {
  # Filter rows with non-missing values for the selected column
  data_filtered <- main_df %>% filter(!is.na(.data[[column]]))
  
  # Create contingency table between gender and the language parameter
  contingency_table <- table(data_filtered$gender, data_filtered[[column]])
  
  # Check if table is valid for Chi-square (at least 2x2)
  if (all(dim(contingency_table) >= 2)) {
    chisq_result <- chisq.test(contingency_table)
    return(list(parameter = column, result = chisq_result))
  } else {
    return(list(parameter = column, result = "Insufficient data for Chi-square test"))
  }
}

# Run Chi-square tests for specified parameters
chisq_results <- map(language_columns, perform_chisq_test)

# Display the Chi-square test results
for (result in chisq_results) {
  cat("Chi-square test for", result$parameter, ":\n")
  print(result$result)
  cat("\n--------------------------\n")
}



# Solution - 3

# Load the dataset
main_df <- read_excel("African-Language-Dataset-Cleaned.xlsx", sheet = 1)

# Define columns for acculturation parameters
acculturation_columns_before <- c(
  "adaptation_to_canadian_culture", 
  "maintaining_native_culture", 
  "maintaining_native_language", 
  "learning_english", 
  "learning_french"
)

acculturation_columns_current <- c(
  "adaptation_to_canadian_culture_current", 
  "maintaining_native_culture_current", 
  "maintaining_native_language_current", 
  "learning_english_current", 
  "learning_french_current"
)

# Split the dataset by gender (1 for Male, 2 for Female)
male_data <- main_df %>% filter(gender == 1)
female_data <- main_df %>% filter(gender == 2)

# Perform t-tests to compare before vs current values for each parameter
acculturation_t_tests <- lapply(1:length(acculturation_columns_before), function(i) {
  param_before <- main_df[[acculturation_columns_before[i]]]
  param_current <- main_df[[acculturation_columns_current[i]]]
  
  # Remove NA values before performing the t-test
  valid_data <- complete.cases(param_before, param_current)
  param_before_clean <- param_before[valid_data]
  param_current_clean <- param_current[valid_data]
  
  # Perform paired t-test only if there are valid pairs
  if(length(param_before_clean) > 1 && length(param_current_clean) > 1) {
    t_test_result <- t.test(param_before_clean, param_current_clean, paired = TRUE)
    return(list(parameter = acculturation_columns_before[i], t_test_result = t_test_result))
  } else {
    return(list(parameter = acculturation_columns_before[i], t_test_result = "Insufficient valid data for paired t-test"))
  }
})

# Display the t-test results
for (result in acculturation_t_tests) {
  cat("T-test for", result$parameter, ":\n")
  print(result$t_test_result)
  cat("\n--------------------------\n")
}

# Correlation analysis for adaptation to Canadian culture, maintaining native culture and language, and learning English/French (before and current)
correlation_data <- main_df %>%
  select(adaptation_to_canadian_culture, maintaining_native_culture, maintaining_native_language, 
         learning_english, learning_french, 
         adaptation_to_canadian_culture_current, maintaining_native_culture_current, 
         maintaining_native_language_current, learning_english_current, learning_french_current)

# Compute correlation matrix for the before and current values
correlation_matrix <- cor(correlation_data, use = "complete.obs", method = "pearson")

# Display the correlation matrix
cat("Correlation Matrix:\n")
print(correlation_matrix)

# Create a heatmap for the correlation matrix
library(ggplot2)
library(reshape2)
correlation_matrix_melt <- melt(correlation_matrix)

ggplot(correlation_matrix_melt, aes(Var1, Var2, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) + 
  theme_minimal() + 
  labs(title = "Correlation Heatmap", x = "Variables", y = "Variables")

