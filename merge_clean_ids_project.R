# ============================================================================
# Introduction to Data Science - Group Assignment
# Data Cleaning and Merging
# ============================================================================
# 
# Instructions for running this script:
# 1. Ensure all CSV files are in the same directory as this R script
# 2. In RStudio: Session > Set Working Directory > To Source File Location
# 3. Run this script
#
# ============================================================================

# Load required packages
library(tidyverse)
library(dplyr)
library(readr)

# ============================================================================
# PART 1: Load Data
# ============================================================================
# Note: All CSV files should be in the same directory as this R script

# Load the three required datasets
gdp <- read_csv("gdp-per-capita-worldbank.csv")
neet <- read_csv("youth-not-in-education-employment-training.csv")
continent <- read_csv("continents-according-to-our-world-in-data.csv")

# Load the fourth additional dataset (Government expenditure on education as % of GDP)
# Data source: World Bank - Government expenditure on education, total (% of GDP)
edu_raw <- read_csv("government_expenditure_on_education.csv", skip = 4)

# ============================================================================
# PART 2: Data Cleaning and Renaming
# ============================================================================

# Clean GDP data
gdp_clean <- gdp %>%
  rename(
    country = Entity, 
    code = Code, 
    year = Year, 
    gdp_per_capita = `GDP per capita, PPP (constant 2017 international $)`
  ) %>%
  distinct(code, year, .keep_all = TRUE)  # Remove duplicate rows

# Clean NEET data
neet_clean <- neet %>%
  rename(
    country = Entity, 
    code = Code, 
    year = Year, 
    neet_share = `Share of youth not in education, employment or training, total (% of youth population)`
  ) %>%
  distinct(code, year, .keep_all = TRUE)  # Remove duplicate rows

# Clean continent classification data
continent_clean <- continent %>%
  rename(
    country = Entity, 
    code = Code, 
    year = Year, 
    continent = Continent
  ) %>%
  filter(continent != "Antarctica") %>%  # Exclude Antarctica (as per assignment requirements)
  select(code, continent) %>%
  distinct()  # Keep only country code and continent, remove duplicates

# Clean education expenditure data (convert from wide to long format)
edu_long <- edu_raw %>%
  # Select country name, country code, and year columns (year columns are 4-digit numbers)
  select(`Country Name`, `Country Code`, matches("^\\d{4}$")) %>%
  # Convert from wide to long format
  pivot_longer(
    cols = -c(`Country Name`, `Country Code`),
    names_to = "year", 
    values_to = "edu_expenditure_gdp"
  ) %>%
  # Rename columns
  rename(country = `Country Name`, code = `Country Code`) %>%
  # Convert year to integer format
  mutate(year = as.integer(year)) %>%
  # Remove duplicate rows
  distinct(code, year, .keep_all = TRUE)

# ============================================================================
# PART 3: Merge All Datasets
# ============================================================================

# Explanation of merging logic:
# 1. Start with GDP data as the base
# 2. Merge NEET and education expenditure data by country code and year
# 3. Finally add continent classification (only by country code, as continent doesn't change over time)
# 4. Use left_join to retain all GDP data, even if other data is missing

master_data <- gdp_clean %>%
  # Merge NEET data
  left_join(
    neet_clean, 
    by = c("code", "year"), 
    suffix = c("_gdp", "_neet")
  ) %>%
  # Merge education expenditure data
  left_join(
    edu_long, 
    by = c("code", "year")
  ) %>%
  # Merge continent classification
  left_join(
    continent_clean, 
    by = "code"
  ) %>%
  # Handle duplicate country columns created by merging
  # coalesce selects the first non-NA value
  mutate(country = coalesce(country_gdp, country_neet, country)) %>%
  # Remove duplicate columns with suffixes and reorganize column order
  select(
    country, code, year, continent, 
    gdp_per_capita, neet_share, edu_expenditure_gdp
  ) %>%
  # Keep only observations with at least one piece of data
  filter(!is.na(gdp_per_capita) | !is.na(neet_share) | !is.na(edu_expenditure_gdp)) %>%
  # Keep only data with continent classification (excludes Antarctica and unclassifiable regions)
  filter(!is.na(continent))

# ============================================================================
# PART 4: Data Quality Check
# ============================================================================

# View the structure of cleaned data
glimpse(master_data)

# Check data range
cat("\nYear range:", min(master_data$year, na.rm = TRUE), "to", 
    max(master_data$year, na.rm = TRUE), "\n")

# Check data completeness by continent
data_coverage <- master_data %>%
  group_by(continent) %>%
  summarise(
    n_countries = n_distinct(code),
    n_observations = n(),
    gdp_coverage = round(sum(!is.na(gdp_per_capita)) / n() * 100, 1),
    neet_coverage = round(sum(!is.na(neet_share)) / n() * 100, 1),
    edu_coverage = round(sum(!is.na(edu_expenditure_gdp)) / n() * 100, 1),
    .groups = "drop"
  )

print("Data completeness by continent:")
print(data_coverage)

# Check for missing values
cat("\nMissing value statistics for each variable:\n")
master_data %>%
  summarise(
    gdp_missing = sum(is.na(gdp_per_capita)),
    neet_missing = sum(is.na(neet_share)),
    edu_missing = sum(is.na(edu_expenditure_gdp)),
    total_obs = n()
  ) %>%
  print()

# ============================================================================
# PART 5: Save Cleaned Data
# ============================================================================

# Save as CSV file for subsequent analysis
write_csv(master_data, "master_dataset.csv")

cat("\nData cleaning and merging completed!\n")
cat("Cleaned data has been saved as 'master_dataset.csv'\n")

# Display first 10 rows of data
cat("\nPreview of first 10 rows:\n")
head(master_data, 10)

# ============================================================================
# End of Data Cleaning and Merging Code
# ============================================================================
