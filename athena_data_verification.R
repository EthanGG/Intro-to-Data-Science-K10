# ============================================================================
# DATA ANALYSIS FOR TARGET 1 REPORT
# Quick checks and summary statistics
# ============================================================================

# Setup
rm(list = ls())
library(tidyverse)
library(readr)

# Load data
master_data <- read_csv("master_dataset.csv", show_col_types = FALSE)

# Calculate growth rates
analysis_data <- master_data %>%
  filter(!is.na(gdp_per_capita)) %>%
  arrange(code, year) %>%
  group_by(code) %>%
  mutate(gdp_growth_rate = (gdp_per_capita / lag(gdp_per_capita) - 1) * 100) %>%
  ungroup()

# Focus on 2015-2023
analysis_period <- analysis_data %>%
  filter(year >= 2015, year <= 2023, !is.na(gdp_growth_rate))

# ============================================================================
# SUMMARY STATS
# ============================================================================

cat("\n=== BASIC STATS ===\n")
cat("Period:", min(analysis_period$year), "-", max(analysis_period$year), "\n")
cat("Total countries:", n_distinct(analysis_period$code), "\n")
cat("LDCs:", sum(analysis_period$is_ldc), "observations\n\n")

# ============================================================================
# TARGET ACHIEVEMENT RATES
# ============================================================================

cat("=== TARGET ACHIEVEMENT (7% threshold) ===\n\n")

# By continent
achievement_by_continent <- analysis_period %>%
  filter(is_ldc == TRUE) %>%
  group_by(continent) %>%
  summarise(
    n = n(),
    above_7 = sum(gdp_growth_rate >= 7, na.rm = TRUE),
    pct = round(above_7/n * 100, 1)
  ) %>%
  arrange(desc(pct))

print(achievement_by_continent)

# Overall
overall <- analysis_period %>%
  filter(is_ldc == TRUE) %>%
  summarise(
    total = n(),
    above_7 = sum(gdp_growth_rate >= 7, na.rm = TRUE),
    pct = round(above_7/total * 100, 1)
  )

cat("\nOverall LDCs:", overall$pct, "% of observations ≥7%\n")

# ============================================================================
# COUNTRY AVERAGES
# ============================================================================

cat("\n=== AVERAGE GROWTH BY COUNTRY (2015-2023) ===\n\n")

country_avg <- analysis_period %>%
  filter(is_ldc == TRUE) %>%
  group_by(country, code) %>%
  summarise(
    avg = round(mean(gdp_growth_rate, na.rm = TRUE), 2),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 3)

# How many above 7%?
above_7_countries <- country_avg %>% filter(avg >= 7)
cat("LDCs with average ≥7%:", nrow(above_7_countries), "\n")
if(nrow(above_7_countries) > 0) print(above_7_countries)

# Top 5
cat("\nTop 5:\n")
top5 <- country_avg %>% arrange(desc(avg)) %>% head(5)
print(top5)

# Bottom 5
cat("\nBottom 5:\n")
bottom5 <- country_avg %>% arrange(avg) %>% head(5)
print(bottom5)

# ============================================================================
# COVID IMPACT
# ============================================================================

cat("\n=== COVID IMPACT ===\n\n")

covid_impact <- analysis_period %>%
  filter(is_ldc == TRUE) %>%
  mutate(period = case_when(
    year <= 2019 ~ "Pre-COVID",
    year == 2020 ~ "COVID-2020",
    year >= 2021 ~ "Post-COVID"
  )) %>%
  group_by(continent, period) %>%
  summarise(avg = round(mean(gdp_growth_rate, na.rm = TRUE), 1), .groups = "drop")

# Asia
cat("Asia:\n")
print(covid_impact %>% filter(continent == "Asia"))

# Africa
cat("\nAfrica:\n")
print(covid_impact %>% filter(continent == "Africa"))

# Check 2020
cat("\n2020 by continent:\n")
covid_2020 <- analysis_period %>%
  filter(is_ldc == TRUE, year == 2020) %>%
  group_by(continent) %>%
  summarise(avg_2020 = round(mean(gdp_growth_rate, na.rm = TRUE), 1))
print(covid_2020)

# ============================================================================
# VARIATION ANALYSIS
# ============================================================================

cat("\n=== WITHIN VS BETWEEN CONTINENT VARIATION ===\n\n")

# Add continent to country averages
country_avg_cont <- country_avg %>%
  left_join(
    analysis_period %>% filter(is_ldc == TRUE) %>% 
      select(code, continent) %>% distinct(),
    by = "code"
  )

# Within-continent SD
within_sd <- country_avg_cont %>%
  filter(!is.na(continent)) %>%
  group_by(continent) %>%
  summarise(sd = round(sd(avg, na.rm = TRUE), 2))

cat("Within-continent SD:\n")
print(within_sd)
cat("Average:", round(mean(within_sd$sd, na.rm = TRUE), 2), "\n")

# Between-continent SD
continent_means <- country_avg_cont %>%
  filter(!is.na(continent)) %>%
  group_by(continent) %>%
  summarise(mean = mean(avg, na.rm = TRUE))

between_sd <- sd(continent_means$mean, na.rm = TRUE)
cat("\nBetween-continent SD:", round(between_sd, 2), "\n")

# ============================================================================
# SDI ANALYSIS
# ============================================================================

cat("\n=== SDI RANGE ===\n\n")

sdi_stats <- analysis_period %>%
  filter(is_ldc == TRUE, !is.na(sdi_score)) %>%
  group_by(continent) %>%
  summarise(
    min = round(min(sdi_score), 2),
    q25 = round(quantile(sdi_score, 0.25), 2),
    median = round(median(sdi_score), 2),
    q75 = round(quantile(sdi_score, 0.75), 2),
    max = round(max(sdi_score), 2),
    n = n()
  )

print(sdi_stats)

# ============================================================================
# NON-LDC COMPARISON
# ============================================================================

cat("\n=== NON-LDC GROWTH (Europe & N.America) ===\n\n")

non_ldc <- analysis_period %>%
  filter(is_ldc == FALSE, continent %in% c("Europe", "North America")) %>%
  summarise(
    median = round(median(gdp_growth_rate, na.rm = TRUE), 1),
    mean = round(mean(gdp_growth_rate, na.rm = TRUE), 1),
    sd = round(sd(gdp_growth_rate, na.rm = TRUE), 1),
    q25 = round(quantile(gdp_growth_rate, 0.25, na.rm = TRUE), 1),
    q75 = round(quantile(gdp_growth_rate, 0.75, na.rm = TRUE), 1)
  )

print(non_ldc)

cat("\n=== DONE ===\n")