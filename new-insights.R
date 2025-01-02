# Load necessary libraries
library(tidyverse)
library(readxl)
library(writexl)
library(ggcorrplot)
# Load required libraries
library(dplyr)
library(ggplot2)
library(dplyr)
library(ggplot2)
library(tidyr)

# Set the working directory (adjust path if necessary)
setwd('~/Data-Science/R-Projects/African-Languages-Analysis/')

# Note: You'll need to save your data as a CSV first
dataset <- read_excel("African-Language-Dataset-Cleaned.xlsx", sheet = 1)


# Load necessary libraries
library(dplyr)
library(ggplot2)

# Assuming the dataset is named 'dataset' and it's a data frame
library(readr)

# Selecting relevant columns using dplyr
selected_data <- dataset %>%
  select(
    home_country, 
    official_home_country_languages,
    languages_learned_in_home_country,
    other_languages_learned_home_country,
    marital
  )

# Print the selected data
print(selected_data)
