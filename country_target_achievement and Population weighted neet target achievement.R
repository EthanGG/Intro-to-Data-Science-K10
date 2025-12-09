library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)

master <- read.csv('master_dataset.csv')
view(master)

# Step 1: Get ALL countries with continent info (don't filter out NAs yet)
neet_population_weighted <- master %>%
  group_by(country, continent) %>%
  arrange(year) %>%
  summarise(
    # Baseline period (2010-2015)
    baseline_neet = last(neet_share[year >= 2010 & year <= 2015 & !is.na(neet_share)]),
    baseline_year = last(year[year >= 2010 & year <= 2015 & !is.na(neet_share)]),
    baseline_pop = last(population[year >= 2010 & year <= 2015 & !is.na(population) & !is.na(neet_share)]),
    # Recent period (2016-2020)
    recent_neet = ifelse(
      any(year == 2020 & !is.na(neet_share)),
      neet_share[year == 2020 & !is.na(neet_share)][1],
      last(neet_share[year >= 2016 & year < 2020 & !is.na(neet_share)])
    ),
    recent_year = ifelse(
      any(year == 2020 & !is.na(neet_share)),
      2020,
      last(year[year >= 2016 & year < 2020 & !is.na(neet_share)])
    ),
    recent_pop = ifelse(
      any(year == 2020 & !is.na(population) & !is.na(neet_share)),
      population[year == 2020 & !is.na(neet_share)][1],
      last(population[year >= 2016 & year < 2020 & !is.na(neet_share)])
    ),
    .groups = 'drop'
  )
# REMOVED the filter here - keep all countries including those with NA

# Step 2: Calculate percentage change and determine target for each country
country_target_achievement <- neet_population_weighted %>%
  mutate(
    pct_change_neet = ifelse(!is.na(baseline_neet) & !is.na(recent_neet),
                             ((recent_neet - baseline_neet) / baseline_neet) * 100,
                             NA_real_),
    # Determine target based on baseline NEET
    target_reduction = case_when(
      is.na(baseline_neet) ~ NA_real_,
      baseline_neet < 10 ~ -10,
      baseline_neet >= 10 & baseline_neet <= 30 ~ -20,
      baseline_neet > 30 ~ -10,
      TRUE ~ NA_real_
    ),
    # Determine if target was achieved
    target_status = case_when(
      is.na(baseline_neet) | is.na(recent_neet) | is.na(baseline_pop) | is.na(recent_pop) ~ "No Data",
      pct_change_neet <= target_reduction ~ "Achieved",
      TRUE ~ "Missed"
    )
  )

# Step 3: For "No Data" countries, we need to estimate their population
# Use the most recent available population data
country_target_achievement <- country_target_achievement %>%
  mutate(
    pop_for_calculation = coalesce(recent_pop, baseline_pop)
  )

# Step 3: Calculate total population by continent (including No Data countries)
continent_total_pop <- country_target_achievement %>%
  group_by(continent) %>%
  summarise(total_continent_pop = sum(pop_for_calculation, na.rm = TRUE), .groups = 'drop')

# Step 4: Calculate population-weighted percentages by target status for each continent
population_weighted_summary <- country_target_achievement %>%
  left_join(continent_total_pop, by = "continent") %>%
  group_by(continent, target_status) %>%
  summarise(
    total_pop_in_category = sum(pop_for_calculation, na.rm = TRUE),
    total_continent_pop = first(total_continent_pop),
    .groups = 'drop'
  ) %>%
  mutate(
    percentage = (total_pop_in_category / total_continent_pop) * 100
  )

print("Population-weighted target achievement by continent:")
print(population_weighted_summary)

# Step 5: Create stacked bar chart
ggplot(population_weighted_summary, aes(x = continent, y = percentage, fill = target_status)) +
  geom_col(position = "stack", alpha = 0.8) +
  scale_fill_manual(
    values = c(
      "Achieved" = "#2166ac",
      "Missed" = "#b2182b",
      "No Data" = "grey80"
    )
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey70"),
    panel.grid.minor = element_line(colour = "grey90", linewidth = 0.4),
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = "Continent",
    y = "Percentage of Population (%)",
    fill = "Target Achievement",
    title = "NEET Reduction Target Achievement by Continent (Population-Weighted)",
    subtitle = "Target: <10% → 10% reduction | 10-30% → 15% reduction | >30% → 10% reduction"
  ) +
  geom_text(
    aes(label = ifelse(percentage > 5, paste0(round(percentage, 1), "%"), "")),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 3
  )
