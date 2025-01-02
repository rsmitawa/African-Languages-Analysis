# Load necessary libraries
library(tidyverse)
library(readxl)
library(writexl)
library(ggcorrplot)
# Load required libraries
library(dplyr)
library(ggplot2)

# Set the working directory (adjust path if necessary)
setwd('~/Data-Science/R-Projects/African-Languages-Analysis/')

# Note: You'll need to save your data as a CSV first
data <- read_excel("African-Language-Dataset-Cleaned.xlsx", sheet = 1)

# 1. Basic city distribution analysis
city_distribution <- data %>%
  count(city_in_canada) %>%
  arrange(desc(n))

print("Distribution of respondents across Canadian cities:")
print(city_distribution)

# 2. Employment status analysis
employment_status <- data %>%
  count(currently_employed) %>%
  mutate(percentage = n/sum(n)*100)

print("\nEmployment status distribution:")
print(employment_status)

# 3. Location size and integration analysis
# First, let's categorize cities by size
data <- data %>%
  mutate(
    current_location_size = case_when(
      grepl("Metropolitan", present_locality) ~ "Metropolitan",
      grepl("City", present_locality) ~ "City",
      grepl("Town", present_locality) ~ "Town",
      TRUE ~ "Other"
    )
  )

# Analyze integration patterns by location size
integration_by_location <- data %>%
  group_by(current_location_size) %>%
  summarize(
    well_integrated = mean(overall_integration == "1", na.rm = TRUE),
    goals_achieved = mean(immigration_goals_achieved == "Yes", na.rm = TRUE),
    n = n()
  )

print("\nIntegration patterns by location size:")
print(integration_by_location)

# Visualization of integration patterns
ggplot(integration_by_location, aes(x = current_location_size)) +
  geom_bar(aes(y = well_integrated, fill = "Well Integrated"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = goals_achieved, fill = "Goals Achieved"), stat = "identity", position = "dodge") +
  labs(title = "Integration Patterns by Location Size",
       x = "Location Size",
       y = "Proportion",
       fill = "Measure") +
  theme_minimal()

# 4. Compare previous vs current location size
location_comparison <- data %>%
  select(previous_locality, present_locality) %>%
  count(previous_locality, present_locality) %>%
  spread(present_locality, n, fill = 0)

print("\nComparison of previous vs current location size:")
print(location_comparison)

# Create a transition analysis
location_transition <- data %>%
  mutate(
    moved_to_larger = case_when(
      previous_locality == "Village" & present_locality %in% c("Town", "City", "Metropolitan City") ~ "Yes",
      previous_locality == "Town" & present_locality %in% c("City", "Metropolitan City") ~ "Yes",
      previous_locality == "City" & present_locality == "Metropolitan City" ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  count(moved_to_larger) %>%
  mutate(percentage = n/sum(n)*100)

print("\nPercentage of respondents who moved to larger locations:")
print(location_transition)

# Create visualization for location transitions
ggplot(data, aes(x = previous_locality, fill = present_locality)) +
  geom_bar(position = "dodge") +
  labs(title = "Previous vs Current Location Size",
       x = "Previous Location",
       y = "Count",
       fill = "Current Location") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Previous code remains the same...

# Age Analysis
# Create age categories
data <- data %>%
  mutate(age_category = case_when(
    age <= 25 ~ "18-25",
    age <= 35 ~ "26-35",
    age <= 45 ~ "36-45",
    age <= 55 ~ "46-55",
    age <= 65 ~ "56-65",
    age <= 75 ~ "66-75",
    TRUE ~ "75+"
  ))

# Descriptive statistics for age
age_stats <- list(
  mean_age = mean(data$age, na.rm = TRUE),
  median_age = median(data$age, na.rm = TRUE),
  mode_age = as.numeric(names(sort(table(data$age), decreasing = TRUE)[1]))
)

print("\nAge Statistics:")
print(age_stats)

# Age category distribution
age_distribution <- data %>%
  count(age_category) %>%
  mutate(percentage = n/sum(n)*100)

print("\nAge Category Distribution:")
print(age_distribution)

# Visualization of age distribution
ggplot(data, aes(x = age_category)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Immigrants by Age Category",
       x = "Age Category",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Language Proficiency Analysis by Age
# Convert proficiency levels to numeric scores
proficiency_to_numeric <- function(x) {
  case_when(
    x == "Advanced" ~ 4,
    x == "Intermediate" ~ 3,
    x == "Beginner" ~ 2,
    x == "None" ~ 1,
    TRUE ~ NA_real_
  )
}

# Calculate language proficiency scores
data <- data %>%
  mutate(
    english_improvement = proficiency_to_numeric(english_proficiency_now_speak) - 
      proficiency_to_numeric(english_proficiency_before_canada_speak),
    french_improvement = proficiency_to_numeric(french_proficiency_now_speak) - 
      proficiency_to_numeric(french_proficiency_before_canada_speak)
  )

# Analyze language improvement by age category
language_by_age <- data %>%
  group_by(age_category) %>%
  summarize(
    avg_english_improvement = mean(english_improvement, na.rm = TRUE),
    avg_french_improvement = mean(french_improvement, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(age_category)

print("\nLanguage Improvement by Age Category:")
print(language_by_age)

# Native Language Retention Analysis
native_language_retention <- data %>%
  group_by(age_category) %>%
  summarize(
    strong_retention = mean(native_language_weakening == "1", na.rm = TRUE),
    moderate_retention = mean(native_language_weakening == "2", na.rm = TRUE),
    weak_retention = mean(native_language_weakening == "4", na.rm = TRUE),
    n = n()
  )

print("\nNative Language Retention by Age Category:")
print(native_language_retention)

# Visualization of language improvements by age
ggplot(language_by_age) +
  geom_bar(aes(x = age_category, y = avg_english_improvement, fill = "English"), 
           stat = "identity", position = "dodge") +
  geom_bar(aes(x = age_category, y = avg_french_improvement, fill = "French"), 
           stat = "identity", position = "dodge") +
  labs(title = "Language Improvement by Age Category",
       x = "Age Category",
       y = "Average Improvement Score",
       fill = "Language") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualization of native language retention
ggplot(native_language_retention) +
  geom_bar(aes(x = age_category, y = strong_retention, fill = "Strong Retention"), 
           stat = "identity", position = "dodge") +
  geom_bar(aes(x = age_category, y = weak_retention, fill = "Weak Retention"), 
           stat = "identity", position = "dodge") +
  labs(title = "Native Language Retention by Age Category",
       x = "Age Category",
       y = "Proportion",
       fill = "Retention Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Statistical tests for age-language relationships
# Correlation test between age and language improvement
age_english_cor <- cor.test(data$age, data$english_improvement, 
                            method = "spearman", use = "complete.obs")
age_french_cor <- cor.test(data$age, data$french_improvement, 
                           method = "spearman", use = "complete.obs")

print("\nCorrelation Tests:")
print("Age vs English Improvement:")
print(age_english_cor)
print("\nAge vs French Improvement:")
print(age_french_cor)


# Previous code remains the same...

# 1. Age correlation with cultural participation and belonging
cultural_by_age <- data %>%
  group_by(age_category) %>%
  summarize(
    event_participation = mean(attends_ethnic_group_events == "Often" | 
                                 attends_ethnic_group_events == "All the time", na.rm = TRUE),
    sense_of_belonging = mean(canada_sense_of_belonging == "Yes", na.rm = TRUE),
    n = n()
  )

print("\nCultural Participation and Belonging by Age:")
print(cultural_by_age)

# Visualization
ggplot(cultural_by_age) +
  geom_line(aes(x = age_category, y = event_participation, group = 1, color = "Event Participation")) +
  geom_line(aes(x = age_category, y = sense_of_belonging, group = 1, color = "Sense of Belonging")) +
  labs(title = "Cultural Integration by Age Category",
       x = "Age Category",
       y = "Proportion",
       color = "Measure") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. Employment analysis by age
employment_by_age <- data %>%
  group_by(age_category) %>%
  summarize(
    employment_rate = mean(currently_employed == "Yes, full time" | 
                             currently_employed == "Yes, part-time", na.rm = TRUE),
    n = n()
  )

# Occupation types by age
occupation_by_age <- data %>%
  group_by(age_category, current_occupation) %>%
  summarize(
    count = n(),
    .groups = 'drop'
  ) %>%
  group_by(age_category) %>%
  mutate(proportion = count/sum(count))

print("\nEmployment Rates by Age:")
print(employment_by_age)
print("\nOccupation Types by Age:")
print(occupation_by_age)

# 3. Social Integration by Age
social_integration <- data %>%
  group_by(age_category) %>%
  summarize(
    high_local_integration = mean(local_community_integration == "Yes", na.rm = TRUE),
    high_comfort = mean(comfort_with_integration == "1", na.rm = TRUE),
    n = n()
  )

print("\nSocial Integration by Age:")
print(social_integration)

# 4. Age at Immigration Analysis
immigration_age <- data %>%
  group_by(age_category) %>%
  summarize(
    avg_immigration_age = mean(age_at_immigration, na.rm = TRUE),
    n = n()
  )

print("\nAverage Age at Immigration by Current Age Category:")
print(immigration_age)

# 5. Location Preferences by Age
location_by_age <- data %>%
  group_by(age_category, present_locality) %>%
  summarize(
    count = n(),
    .groups = 'drop'
  ) %>%
  group_by(age_category) %>%
  mutate(proportion = count/sum(count))

print("\nLocation Preferences by Age:")
print(location_by_age)

# 6. Location Change Analysis
location_change <- data %>%
  group_by(age_category) %>%
  summarize(
    moved_to_larger = mean(
      case_when(
        previous_locality == "Village" & present_locality %in% c("Town", "City", "Metropolitan City") ~ TRUE,
        previous_locality == "Town" & present_locality %in% c("City", "Metropolitan City") ~ TRUE,
        previous_locality == "City" & present_locality == "Metropolitan City" ~ TRUE,
        TRUE ~ FALSE
      ),
      na.rm = TRUE
    ),
    n = n()
  )

print("\nProportion Moving to Larger Locations by Age:")
print(location_change)

# Visualization of location changes
ggplot(location_change) +
  geom_bar(aes(x = age_category, y = moved_to_larger), stat = "identity", fill = "steelblue") +
  labs(title = "Proportion Moving to Larger Locations by Age Category",
       x = "Age Category",
       y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Statistical tests
# Chi-square test for age category and location size
location_chi <- chisq.test(table(data$age_category, data$present_locality))
print("\nChi-square test for Age and Location Size:")
print(location_chi)

# Correlation between age and social integration
integration_cor <- cor.test(as.numeric(factor(data$age_category)), 
                            as.numeric(factor(data$local_community_integration)),
                            method = "spearman")
print("\nCorrelation between Age and Social Integration:")
print(integration_cor)

