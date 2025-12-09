library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)

master <- read.csv('master_dataset.csv')
view(master)

# Calculate population-weighted NEET for baseline and recent periods by continent
continent_neet_weighted <- country_target_achievement %>%
  filter(!is.na(baseline_neet) & !is.na(recent_neet)) %>%  # Only countries with both baseline and recent data
  group_by(continent) %>%
  summarise(
    # Population-weighted NEET values
    baseline_weighted_neet = sum(baseline_neet * baseline_pop, na.rm = TRUE) / sum(baseline_pop, na.rm = TRUE),
    recent_weighted_neet = sum(recent_neet * recent_pop, na.rm = TRUE) / sum(recent_pop, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    # Calculate percentage change
    weighted_neet_change = ((recent_weighted_neet - baseline_weighted_neet) / baseline_weighted_neet) * 100
  )

# Create bar chart
ggplot(continent_neet_weighted, aes(x = reorder(continent, -weighted_neet_change), 
                                    y = weighted_neet_change, 
                                    fill = weighted_neet_change < 0)) +
  geom_col(alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_text(aes(label = paste0(round(weighted_neet_change, 1), "%")), 
            vjust = ifelse(continent_neet_weighted$weighted_neet_change < 0, 1.0, -0.5),
            fontface = "bold",
            size = 4) +
  scale_fill_manual(
    values = c("TRUE" = "#2166ac", "FALSE" = "#b2182b"), 

  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey70"),
    panel.grid.minor = element_line(colour = "grey90", linewidth = 0.4),
    plot.title = element_text(hjust = 0.0, face = "bold", size = 16),
    plot.subtitle = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  labs(
    x = "Continent",
    y = "Population-Weighted NEET Change (%)",
    fill = "NEET Trend",
    title = "Figure 7: Population-Weighted NEET Change",
    subtitle = "Baseline: 2010-2015 vs Recent: 2016-2020 (Latest available)"
  )

# Print the table as well
print("Population-weighted NEET change by continent:")
print(continent_neet_weighted)

# Save to CSV
write_csv(continent_neet_weighted, "continent_population_weighted_neet_change.csv")


# Calculate year-over-year NEET percentage change for each country
# Only for CONSECUTIVE years
yoy_neet_change <- master %>%
  filter(!is.na(neet_share) & !is.na(population)) %>%
  group_by(country, continent) %>%
  arrange(year) %>%
  mutate(
    # Check if this year and previous year are consecutive
    year_diff = year - lag(year),
    prev_neet = lag(neet_share),
    prev_pop = lag(population),
    # Calculate YoY percentage change ONLY if years are consecutive
    yoy_pct_change = ifelse(year_diff == 1 & !is.na(prev_neet),
                            ((neet_share - prev_neet) / prev_neet) * 100,
                            NA_real_)
  ) %>%
  filter(!is.na(yoy_pct_change)) %>%  # Keep only consecutive year comparisons
  ungroup()

# Calculate global mean YoY percentage change (population-weighted)
# Only includes countries with consecutive year data
global_yoy_weighted <- yoy_neet_change %>%
  group_by(year) %>%
  summarise(
    total_pop = sum(population, na.rm = TRUE),
    weighted_yoy_pct_change = sum(yoy_pct_change * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    n_countries = n(),
    n_unique_countries = n_distinct(country),
    .groups = 'drop'
  )

# Overall global mean YoY percentage change across all years
global_mean_yoy_pct <- yoy_neet_change %>%
  summarise(
    # Simple average (unweighted)
    mean_yoy_pct_change_unweighted = mean(yoy_pct_change, na.rm = TRUE),
    
    # Population-weighted average
    mean_yoy_pct_change_weighted = sum(yoy_pct_change * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    
    median_yoy_pct_change = median(yoy_pct_change, na.rm = TRUE),
    sd_yoy_pct_change = sd(yoy_pct_change, na.rm = TRUE),
    n_consecutive_year_pairs = n(),
    n_countries_with_consecutive_data = n_distinct(country)
  )

print("Global mean year-over-year NEET percentage change (consecutive years only):")
print(global_mean_yoy_pct)

print("\nGlobal YoY percentage change by year (population-weighted, consecutive years only):")
print(global_yoy_weighted)

# Show which countries have consecutive year data
countries_with_consecutive <- yoy_neet_change %>%
  group_by(country, continent) %>%
  summarise(
    n_consecutive_pairs = n(),
    mean_yoy_pct = mean(yoy_pct_change, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(n_consecutive_pairs))

print("\nCountries with consecutive year data:")
print(head(countries_with_consecutive, 20))

# Visualize global YoY percentage trend
ggplot(global_yoy_weighted, aes(x = year, y = weighted_yoy_pct_change)) +
  geom_line(linewidth = 1.2, color = "#2166ac") +
  geom_point(size = 3, color = "#2166ac") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(colour = "grey70"),
    panel.grid.minor = element_line(colour = "grey90", linewidth = 0.4),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    x = "Year",
    y = "Population-Weighted YoY NEET Change (%)",
    title = "Global Year-over-Year NEET Percentage Change (Population-Weighted)",
    subtitle = "Only includes consecutive year comparisons\nNegative values indicate improvement"
  ) +
  geom_text(aes(label = n_countries), vjust = -1, size = 3, color = "grey50")

# Calculate by continent (consecutive years only)
continent_yoy_pct <- yoy_neet_change %>%
  group_by(continent) %>%
  summarise(
    mean_yoy_pct_change_weighted = sum(yoy_pct_change * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    mean_yoy_pct_change_unweighted = mean(yoy_pct_change, na.rm = TRUE),
    n_consecutive_pairs = n(),
    n_countries = n_distinct(country),
    .groups = 'drop'
  ) %>%
  arrange(mean_yoy_pct_change_weighted)

print("\nMean YoY NEET percentage change by continent (consecutive years only):")
print(continent_yoy_pct)

# Save results
write_csv(global_mean_yoy_pct, "global_mean_yoy_neet_pct_change_consecutive.csv")
write_csv(global_yoy_weighted, "global_yoy_pct_by_year_consecutive.csv")
write_csv(continent_yoy_pct, "continent_mean_yoy_neet_pct_change_consecutive.csv")
write_csv(countries_with_consecutive, "countries_with_consecutive_year_data.csv")