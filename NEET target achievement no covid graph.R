library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)

master <- read.csv('master_dataset.csv')
view(master)

# Step 1: Get ALL countries from master dataset (2010-2021) with their most recent population
all_countries_with_pop <- master %>%
  filter(year >= 2010 & year <= 2021) %>%
  group_by(country, continent) %>%
  arrange(desc(year)) %>%
  summarise(
    most_recent_pop = first(population[!is.na(population)]),
    .groups = 'drop'
  ) %>%
  filter(!is.na(most_recent_pop))  # Only keep countries with at least some population data

# Step 2: Calculate NEET change for countries WITH data
neet_country_change <- master %>%
  filter(!is.na(neet_share)) %>%
  group_by(country, continent) %>%
  arrange(year) %>%
  summarise(
    baseline_neet = first(neet_share[year >= 2010 & year <= 2015]),
    baseline_year = first(year[year >= 2010 & year <= 2015]),
    baseline_pop = first(population[year >= 2010 & year <= 2015 & !is.na(population)]),
    recent_neet = ifelse(
      any(year == 2018 & !is.na(neet_share)),
      neet_share[year == 2018][1],
      last(neet_share[year >= 2016 & year < 2018])
    ),
    recent_year = ifelse(
      any(year == 2018 & !is.na(neet_share)),
      2018,
      last(year[year >= 2016 & year < 2018])
    ),
    recent_pop = ifelse(
      any(year == 2018 & !is.na(population)),
      population[year == 2018][1],
      last(population[year >= 2016 & year < 2018 & !is.na(population)])
    ),
    .groups = 'drop'
  ) %>%
  mutate(
    pct_change_neet = ifelse(!is.na(baseline_neet) & !is.na(recent_neet),
                             ((recent_neet - baseline_neet) / baseline_neet) * 100,
                             NA_real_),
    target_reduction = case_when(
      is.na(baseline_neet) ~ NA_real_,
      baseline_neet < 10 ~ -10,
      baseline_neet >= 10 & baseline_neet <= 30 ~ -20,
      baseline_neet > 30 ~ -10,
      TRUE ~ NA_real_
    ),
    target_status = case_when(
      is.na(baseline_neet) | is.na(recent_neet) ~ "No Data",
      pct_change_neet <= target_reduction ~ "Achieved",
      TRUE ~ "Missed"
    )
  )

# Step 3: Join ALL countries with NEET change results
countries_complete <- all_countries_with_pop %>%
  left_join(
    neet_country_change %>% select(country, target_status, recent_pop),
    by = "country"
  ) %>%
  mutate(
    target_status = ifelse(is.na(target_status), "No Data", target_status),
    pop_for_calculation = coalesce(recent_pop, most_recent_pop)
  )

# Step 4: Calculate total population by continent (including No Data countries)
continent_total_pop <- countries_complete %>%
  group_by(continent) %>%
  summarise(total_continent_pop = sum(pop_for_calculation, na.rm = TRUE), .groups = 'drop')

# Step 5: Calculate population-weighted percentages INCLUDING "No Data"
population_weighted_summary_complete <- countries_complete %>%
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

print(population_weighted_summary_complete)

# Step 6: Create stacked bar chart with all three categories
ggplot(population_weighted_summary_complete, aes(x = continent, y = percentage, fill = target_status)) +
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
    y = "Percentage of Total Population (%)",
    fill = "Target Achievement",
    title = "NEET Reduction Target Achievement by Continent (Population-Weighted)",
    subtitle = "Target: <10% → 10% reduction | 10-30% → 15% reduction | >30% → 10% reduction\n(Including all countries with population data)"
  ) +
  geom_text(
    aes(label = ifelse(percentage > 5, paste0(round(percentage, 1), "%"), "")),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 3
  )


# Calculate percentage of NA for NEET data (2010-2021)
neet_na_analysis <- master %>%
  filter(year >= 2010 & year <= 2021) %>%
  group_by(continent) %>%
  summarise(
    total_observations = n(),
    neet_na_count = sum(is.na(neet_share)),
    neet_available_count = sum(!is.na(neet_share)),
    neet_na_percentage = round((sum(is.na(neet_share)) / n()) * 100, 1),
    neet_available_percentage = round((sum(!is.na(neet_share)) / n()) * 100, 1),
    .groups = 'drop'
  )


view(neet_na_analysis)

# Overall global statistics
global_neet_na <- master %>%
  filter(year >= 2010 & year <= 2021) %>%
  summarise(
    total_observations = n(),
    neet_na_count = sum(is.na(neet_share)),
    neet_available_count = sum(!is.na(neet_share)),
    neet_na_percentage = round((sum(is.na(neet_share)) / n()) * 100, 1),
    neet_available_percentage = round((sum(!is.na(neet_share)) / n()) * 100, 1)
  )

print("\nGlobal NEET data availability (2010-2021):")
print(global_neet_na)

# By year
neet_na_by_year <- master %>%
  filter(year >= 2010 & year <= 2021) %>%
  group_by(year) %>%
  summarise(
    total_countries = n(),
    neet_na_count = sum(is.na(neet_share)),
    neet_available_count = sum(!is.na(neet_share)),
    neet_na_percentage = round((sum(is.na(neet_share)) / n()) * 100, 1),
    .groups = 'drop'
  )

print("\nNEET data availability by year (2010-2021):")
print(neet_na_by_year)

# Visualize NA percentage by continent
ggplot(neet_na_analysis, aes(x = reorder(continent, -neet_na_percentage), y = neet_na_percentage, fill = continent)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = paste0(neet_na_percentage, "%")), vjust = -0.5, fontface = "bold") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey70"),
    panel.grid.minor = element_line(colour = "grey90", linewidth = 0.4),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = "Continent",
    y = "Missing NEET Data (%)",
    title = "Percentage of Missing NEET Data by Continent (2010-2021)"
  )

# Visualize NA percentage by year
ggplot(neet_na_by_year, aes(x = year, y = neet_na_percentage)) +
  geom_line(linewidth = 1.2, color = "#b2182b") +
  geom_point(size = 3, color = "#b2182b") +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(colour = "grey70"),
    panel.grid.minor = element_line(colour = "grey90", linewidth = 0.4),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    x = "Year",
    y = "Missing NEET Data (%)",
    title = "Trend of Missing NEET Data Over Time (2010-2021)"
  ) +
  scale_x_continuous(breaks = 2010:2021)
