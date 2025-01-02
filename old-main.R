# Load necessary libraries
library(tidyverse)
library(readxl)
library(writexl)
library(ggcorrplot)

# Set the working directory (adjust path if necessary)
setwd('~/Data-Science/R-Projects/African-Analysis-Autopilot/')

# Load the dataset
main_df <- read_excel("African-Language-Dataset-Cleaned.xlsx", sheet = 1)


# Questions 1- 
## Does the desire to maintain home languages or the degree of its maintenance (forgetting) correlate
## with education level and socio-economic status (Spearman correlation)?

# Check the structure of the dataset
str(main_df)



# Select relevant columns for analysis
correlation_data <- main_df %>%
  select(
    maintaining_native_language,
    native_language_weakening,
    years_of_weakening,
    native_language_difficulties,
    daily_native_language_interaction,
    native_language_speaking_now,
    native_language_reading_now,
    native_language_writing_now,
    native_language_listening_now,
    pass_on_native_language,
    pass_on_native_culture,
    highest_education_home_country,
    education_in_canada,
    major_in_canada,
    currently_employed,
    current_occupation,
    previous_occupation,
    economic_status_comparison
  )


# Check the structure of the selected data
str(correlation_data)

# Convert non-numeric columns to numeric
# This step assumes that categorical variables can be represented as numeric factors.
# Modify this conversion according to your data's structure and needs.
correlation_data <- correlation_data %>%
  mutate(across(everything(), ~ as.numeric(as.factor(.)), .names = "numeric_{col}"))

# Remove original columns if needed
correlation_data <- correlation_data %>%
  select(starts_with("numeric_"))

# Calculate Spearman correlation coefficients
correlation_results <- cor(correlation_data, method = "spearman", use = "pairwise.complete.obs")

print(correlation_results)

ggcorrplot(correlation_results, method = "circle", type = "upper", lab = TRUE)

# Insight - 


# Load required libraries
library(tidyverse)
library(ggplot2)
library(scales)

# 1. Integration Score Analysis
# Create an integration score based on key variables
integration_analysis <- df %>%
  mutate(
    integration_score = (
      as.numeric(adaptation_to_canadian_culture) +
      as.numeric(maintaining_native_culture) +
      as.numeric(overall_integration)
    )/3
  ) %>%
  select(
    sr_no,
    integration_score,
    age_at_immigration,
    year_of_immigration,
    adaptation_to_canadian_culture,
    maintaining_native_culture,
    overall_integration
  )

# 2. Visualization of Acculturation Patterns
ggplot(df, aes(x = adaptation_to_canadian_culture, y = maintaining_native_culture)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.6) +
  geom_hline(yintercept = mean(as.numeric(df$maintaining_native_culture)), linetype = "dashed") +
  geom_vline(xintercept = mean(as.numeric(df$adaptation_to_canadian_culture)), linetype = "dashed") +
  labs(
    title = "Acculturation Patterns",
    x = "Adaptation to Canadian Culture",
    y = "Maintenance of Native Culture",
    caption = "Dashed lines represent mean values"
  ) +
  theme_minimal()

# 3. Chi-Square Test for Cultural Identity
# Testing relationship between overall_integration and comfort_with_integration
cultural_identity_table <- table(df$overall_integration, df$comfort_with_integration)
chi_sq_test <- chisq.test(cultural_identity_table)
print(chi_sq_test)

# 4. Correlation Analysis
correlation_vars <- df %>%
  select(
    age_at_immigration,
    adaptation_to_canadian_culture,
    maintaining_native_culture,
    overall_integration
  ) %>%
  mutate(across(everything(), as.numeric))

cor_matrix <- cor(correlation_vars, use = "complete.obs")
print(cor_matrix)

# 5. Descriptive Statistics for Key Variables
summary_stats <- df %>%
  summarise(
    mean_age_immigration = mean(as.numeric(age_at_immigration)),
    mean_adaptation = mean(as.numeric(adaptation_to_canadian_culture)),
    mean_maintenance = mean(as.numeric(maintaining_native_culture)),
    mean_integration = mean(as.numeric(overall_integration))
  )

# 6. Categorization of Acculturation Strategies
df <- df %>%
  mutate(
    acculturation_strategy = case_when(
      as.numeric(adaptation_to_canadian_culture) >= 3 & 
      as.numeric(maintaining_native_culture) >= 3 ~ "Integration",
      as.numeric(adaptation_to_canadian_culture) >= 3 & 
      as.numeric(maintaining_native_culture) < 3 ~ "Assimilation",
      as.numeric(adaptation_to_canadian_culture) < 3 & 
      as.numeric(maintaining_native_culture) >= 3 ~ "Separation",
      TRUE ~ "Marginalization"
    )
  )

# Visualization of Acculturation Strategies
ggplot(df, aes(x = acculturation_strategy)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Distribution of Acculturation Strategies",
    x = "Strategy",
    y = "Count"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

