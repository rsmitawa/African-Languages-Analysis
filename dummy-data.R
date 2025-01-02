# Load required libraries
library(tidyverse)
library(readxl)
library(writexl)
library(ggplot2)
library(reshape2)

# Set the working directory (adjust path if necessary)
setwd('~/Data-Science/R-Projects/African-Languages-Analysis/')

# Read the Excel file (skipping first row)
lang_usage_dummy <- read_excel("African-Language-Dataset-Cleaned.xlsx", 
                   sheet = 2)

# Data Cleaning

# Filling zeros in place of null and missing values
lang_usage_dummy[is.na(lang_usage_dummy)] <- 0


# 1. Chi-Square Test of Independence
# First, let's reshape the data for the chi-square test
# We'll focus on the main languages across different contexts

# Create a contingency table
# Combining all contexts for each language type
lang_contexts <- lang_usage_dummy %>%
  select(starts_with(c("English_", "French_", "Arabic_", "Native_language_"))) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  gather() %>%
  separate(key, into = c("Language", "Context"), sep = "_") %>%
  spread(Language, value)

# Perform Chi-square test
chi_test <- chisq.test(as.matrix(lang_contexts[,-1]))

# Print results
print("Chi-Square Test Results:")
print(chi_test)

# 2. Descriptive Statistics and Visualization

# Calculate proportions for each language in different contexts
lang_proportions <- lang_usage_dummy %>%
  select(starts_with(c("English_", "French_", "Arabic_", "Native_language_", "Other_", "N/A_"))) %>%
  summarise(across(everything(), function(x) mean(x > 0, na.rm = TRUE))) %>%
  gather() %>%
  separate(key, into = c("Language", "Context"), sep = "_", extra = "merge")

# Create a heatmap visualization
ggplot(lang_proportions, aes(x = Context, y = Language, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue", name = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Language Usage Proportions Across Different Contexts",
       x = "Context",
       y = "Language") +
  coord_fixed()

# Create a bar plot for overall language usage
lang_summary <- lang_proportions %>%
  group_by(Language) %>%
  summarise(avg_usage = mean(value, na.rm = TRUE))

ggplot(lang_summary, aes(x = reorder(Language, -avg_usage), y = avg_usage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Average Language Usage Across All Contexts",
       x = "Language",
       y = "Average Proportion of Usage") +
  scale_y_continuous(labels = scales::percent)

# Additional descriptive statistics
desc_stats <- lang_proportions %>%
  group_by(Language) %>%
  summarise(
    mean_usage = mean(value, na.rm = TRUE),
    sd_usage = sd(value, na.rm = TRUE),
    median_usage = median(value, na.rm = TRUE),
    min_usage = min(value, na.rm = TRUE),
    max_usage = max(value, na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric), round, 2))

print("Descriptive Statistics:")
print(desc_stats)

# Save results to a file
write.csv(desc_stats, "language_usage_statistics.csv", row.names = FALSE)
