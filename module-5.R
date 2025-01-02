# Load required libraries
library(dplyr)    # For data manipulation
library(ggplot2)  # For visualization
library(vcd)      # For chi-square tests

marital_language_data <- read.csv("filtered_data/marital_language_data.csv")

# 1. Marital Status Distribution
marital_status_distribution <- marital_language_data %>%
  group_by(marital_status) %>%
  summarise(count = n())

print("Marital Status Distribution:")
print(marital_status_distribution)

# 2. Language Use Among Spouses
spouse_language_use <- marital_language_data %>%
  filter(marital_status == "Married") %>%
  mutate(spouse_speaks_partner_language = ifelse(
    partner_languages %in% languages_learned_in_home_country |
      partner_languages %in% other_languages_learned_home_country, "Yes", "No"
  )) %>%
  summarise(spouse_speaks_partner_language = sum(spouse_speaks_partner_language == "Yes"))

print("Number of Spouses Who Speak Their Partner’s Native Language:")
print(spouse_language_use)

# 3. Bilingualism Among Single Participants
single_bilingual <- marital_language_data %>%
  filter(marital_status == "Single") %>%
  mutate(is_bilingual = ifelse(
    !is.na(languages_learned_in_home_country) | !is.na(other_languages_learned_home_country), "Yes", "No"
  )) %>%
  summarise(single_bilingual_count = sum(is_bilingual == "Yes"))

print("Number of Single Bilingual Participants:")
print(single_bilingual)

# 4. Bilingualism Among Married Participants
married_bilingual <- marital_language_data %>%
  filter(marital_status == "Married") %>%
  mutate(is_bilingual = ifelse(
    !is.na(languages_learned_in_home_country) | !is.na(other_languages_learned_home_country), "Yes", "No"
  )) %>%
  summarise(married_bilingual_count = sum(is_bilingual == "Yes"))

print("Number of Married Bilingual Participants:")
print(married_bilingual)

# 5. Visualization of Bilingualism by Marital Status
bilingual_data <- data.frame(
  group = c("Single", "Married"),
  count = c(single_bilingual$single_bilingual_count, married_bilingual$married_bilingual_count)
)

ggplot(bilingual_data, aes(x = "", y = count, fill = group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  labs(title = "Bilingual Participants: Married vs. Single")

# 6. Multilingualism Among Single Participants
single_multilingual <- marital_language_data %>%
  filter(marital_status == "Single") %>%
  mutate(is_multilingual = ifelse(
    lengths(strsplit(languages_learned_in_home_country, ",")) > 1 |
      lengths(strsplit(other_languages_learned_home_country, ",")) > 1, "Yes", "No"
  )) %>%
  summarise(single_multilingual_count = sum(is_multilingual == "Yes"))

print("Number of Single Multilingual Participants:")
print(single_multilingual)

# 7. Multilingualism Among Married Participants
married_multilingual <- marital_language_data %>%
  filter(marital_status == "Married") %>%
  mutate(is_multilingual = ifelse(
    lengths(strsplit(languages_learned_in_home_country, ",")) > 1 |
      lengths(strsplit(other_languages_learned_home_country, ",")) > 1, "Yes", "No"
  )) %>%
  summarise(married_multilingual_count = sum(is_multilingual == "Yes"))

print("Number of Married Multilingual Participants:")
print(married_multilingual)

# 8. Visualization of Multilingualism by Marital Status
multilingual_data <- data.frame(
  group = c("Single", "Married"),
  count = c(single_multilingual$single_multilingual_count, married_multilingual$married_multilingual_count)
)

ggplot(multilingual_data, aes(x = "", y = count, fill = group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  labs(title = "Multilingual Participants: Married vs. Single")

# 9. Native Language and Cultural Maintenance
native_language_maintenance <- marital_language_data %>%
  group_by(marital_status) %>%
  summarise(
    mean_native_language_attitude = mean(native_language_attitude_change == "Yes", na.rm = TRUE)
  )

print("Native Language and Cultural Maintenance by Marital Status:")
print(native_language_maintenance)