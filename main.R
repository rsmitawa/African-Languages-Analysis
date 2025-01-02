# Load necessary libraries
library(tidyverse)
library(readxl)
library(writexl)
library(ggcorrplot)

# Set the working directory (adjust path if necessary)
setwd('~/Data-Science/R-Projects/African-Analysis-Autopilot/')

# Load the dataset
main_df <- read_excel("African-Language-Dataset-Cleaned.xlsx", sheet = 1)

colnames(main_df)

### Problem: Categorize responses by city/town to explore geographical distribution patterns.
table(main_df$present_locality)
