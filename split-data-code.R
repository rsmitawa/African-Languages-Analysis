# Load necessary libraries
library(tidyverse)
library(readxl)
library(writexl)
library(ggcorrplot)
# Load necessary libraries
library(dplyr)
library(ggplot2)


# Set the working directory (adjust path if necessary)
setwd('~/Data-Science/R-Projects/African-Analysis-Autopilot/')

# Load the dataset
data <- read_excel("African-Language-Dataset-Cleaned.xlsx", sheet = 1)

# Create a subdirectory for saving files
if (!dir.exists("filtered_data")) {
  dir.create("filtered_data")
}

# Filter data for each problem area and save as CSV files

# 1. Geographical Distribution and Location Analysis
geo_data <- data %>%
  select(city_in_canada, currently_employed)
write.csv(geo_data, "filtered_data/geo_data.csv", row.names = FALSE)

# 2. Location Size and Integration Patterns
location_integration_data <- data %>%
  select(city_in_canada, immigration_goals_achieved, overall_integration)
write.csv(location_integration_data, "filtered_data/location_integration_data.csv", row.names = FALSE)

# 3. Comparison of Hometown and Current Location Size
location_comparison_data <- data %>%
  select(city_in_canada, previous_locality)
write.csv(location_comparison_data, "filtered_data/location_comparison_data.csv", row.names = FALSE)

# 4. Age Analysis
age_data <- data %>%
  select(age, english_proficiency_now_speak, english_proficiency_now_read, english_proficiency_now_listen, english_proficiency_now_write,
         french_proficiency_now_speak, french_proficiency_now_read, french_proficiency_now_listen, french_proficiency_now_write,
         native_language_speaking_now, native_language_reading_now, native_language_writing_now, native_language_listening_now,
         canada_sense_of_belonging, economic_status_comparison)
write.csv(age_data, "filtered_data/age_data.csv", row.names = FALSE)

# 5. Language Proficiency and Retention
language_data <- data %>%
  select(mother_tongue, home_country_languages, languages_learned_in_home_country, languages_spoken_fluently_before_canada,
         native_language_weakening, years_of_weakening, native_language_difficulties)
write.csv(language_data, "filtered_data/language_data.csv", row.names = FALSE)

# 6. Marital Status and Language Use
marital_language_data <- data %>%
  select(marital_status, partner_languages, partner_other_languages, language_with_spouse, language_with_children)
write.csv(marital_language_data, "filtered_data/marital_language_data.csv", row.names = FALSE)

# 7. Occupation and Economic Outcomes
occupation_data <- data %>%
  select(previous_occupation, current_occupation, economic_status_comparison)
write.csv(occupation_data, "filtered_data/occupation_data.csv", row.names = FALSE)

# 8. Immigration History and Integration
immigration_history_data <- data %>%
  select(previous_countries, previous_countries_name_duration, overall_integration, immigration_goals_achieved)
write.csv(immigration_history_data, "filtered_data/immigration_history_data.csv", row.names = FALSE)

# 9. Immigration Pathways and Outcomes
immigration_pathways_data <- data %>%
  select(immigration_process, year_of_immigration, overall_integration, immigration_goals_achieved)
write.csv(immigration_pathways_data, "filtered_data/immigration_pathways_data.csv", row.names = FALSE)

# 10. Frequency of Home Country Visits
home_visits_data <- data %>%
  select(frequency_of_home_visits, native_language_speaking_difficulty, age)
write.csv(home_visits_data, "filtered_data/home_visits_data.csv", row.names = FALSE)

# 11. Education and Occupation Alignment
education_occupation_data <- data %>%
  select(highest_education_home_country, education_in_canada, major_in_canada, current_occupation)
write.csv(education_occupation_data, "filtered_data/education_occupation_data.csv", row.names = FALSE)

# 12. Language Proficiency Analysis
language_proficiency_data <- data %>%
  select(english_proficiency_before_canada_speak, english_proficiency_before_canada_read, english_proficiency_before_canada_listen, english_proficiency_before_canada_write,
         english_proficiency_now_speak, english_proficiency_now_read, english_proficiency_now_listen, english_proficiency_now_write,
         french_proficiency_before_canada_speak, french_proficiency_before_canada_read, french_proficiency_before_canada_listen, french_proficiency_before_canada_write,
         french_proficiency_now_speak, french_proficiency_now_read, french_proficiency_now_listen, french_proficiency_now_write,
         improvement_in_english, improvement_in_french)
write.csv(language_proficiency_data, "filtered_data/language_proficiency_data.csv", row.names = FALSE)

# Print confirmation message
cat("All filtered datasets have been saved in the 'filtered_data' subdirectory.\n")