# Load required libraries
library(ggplot2)
library(scales)
library(readxl)
library(writexl)
library(ggcorrplot)
library(tidyverse)

# Set the working directory (adjust path if necessary)
setwd('~/Data-Science/R-Projects/African-Languages-Analysis/')

# Load the dataset
african_language_data <- read_excel("African-Language-Dataset-Cleaned.xlsx", sheet = 1)

# Section 1: Integration Score Analysis --------------------------------------

# Calculate an integration score as the mean of relevant variables
african_language_data <- african_language_data %>%
  mutate(
    integration_score = rowMeans(select(., 
                                        adaptation_to_canadian_culture, 
                                        maintaining_native_culture, 
                                        overall_integration), 
                                 na.rm = TRUE)
  )

# Section 2: Acculturation Strategy Categorization ---------------------------

# Categorize acculturation strategies based on key variables
african_language_data <- african_language_data %>%
  mutate(
    acculturation_strategy = case_when(
      adaptation_to_canadian_culture >= 3 & maintaining_native_culture >= 3 ~ "Integration",
      adaptation_to_canadian_culture >= 3 & maintaining_native_culture < 3 ~ "Assimilation",
      adaptation_to_canadian_culture < 3 & maintaining_native_culture >= 3 ~ "Separation",
      TRUE ~ "Marginalization"
    )
  )

# Visualize the distribution of acculturation strategies
ggplot(african_language_data, aes(x = acculturation_strategy)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Distribution of Acculturation Strategies",
    x = "Acculturation Strategy",
    y = "Count"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Section 3: Acculturation Patterns ------------------------------------------

# Scatter plot of adaptation vs. maintenance with mean reference lines
ggplot(african_language_data, aes(x = adaptation_to_canadian_culture, y = maintaining_native_culture)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.6) +
  geom_hline(yintercept = mean(african_language_data$maintaining_native_culture, na.rm = TRUE), 
             linetype = "dashed") +
  geom_vline(xintercept = mean(african_language_data$adaptation_to_canadian_culture, na.rm = TRUE), 
             linetype = "dashed") +
  labs(
    title = "Acculturation Patterns",
    x = "Adaptation to Canadian Culture",
    y = "Maintenance of Native Culture",
    caption = "Dashed lines represent mean values"
  ) +
  theme_minimal()

# Section 4: Chi-Square Test for Cultural Identity ---------------------------

# Test for relationship between overall integration and comfort with integration
cultural_identity_table <- table(african_language_data$overall_integration, 
                                 african_language_data$comfort_with_integration)
chi_sq_test <- chisq.test(cultural_identity_table)
print(chi_sq_test)

# Section 5: Correlation Analysis --------------------------------------------

# Select relevant numeric variables and compute the correlation matrix
correlation_vars <- african_language_data %>%
  select(
    age_at_immigration,
    adaptation_to_canadian_culture,
    maintaining_native_culture,
    overall_integration
  ) %>%
  mutate(across(everything(), as.numeric))

cor_matrix <- cor(correlation_vars, use = "complete.obs")

# Visualize the correlation matrix
ggcorrplot(cor_matrix, lab = TRUE, lab_size = 3, colors = c("red", "white", "blue")) +
  ggtitle("Correlation Matrix for Key Variables")

# Section 6: Descriptive Statistics ------------------------------------------

# Compute summary statistics for key variables
summary_stats <- african_language_data %>%
  summarise(
    mean_age_immigration = mean(age_at_immigration, na.rm = TRUE),
    mean_adaptation = mean(adaptation_to_canadian_culture, na.rm = TRUE),
    mean_maintenance = mean(maintaining_native_culture, na.rm = TRUE),
    mean_integration = mean(overall_integration, na.rm = TRUE)
  )
print(summary_stats)
