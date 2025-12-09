library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)

master <- read.csv('master_dataset.csv')
view(master)

neet_country_change <- master %>%
  filter(!is.na(neet_share)) %>%
  group_by(country) %>%
  arrange(year) %>%
  summarise(
    baseline_neet = first(neet_share[year >= 2010 & year <= 2015]),
    baseline_year = first(year[year >= 2010 & year <= 2015]),
    recent_neet = ifelse(
      any(year == 2020 & !is.na(neet_share)),
      neet_share[year == 2020][1],
      last(neet_share[year >= 2016 & year < 2020])
    ),
    recent_year = ifelse(
      any(year == 2020 & !is.na(neet_share)),
      2020,
      last(year[year >= 2016 & year < 2020])
    ),
    .groups = 'drop'
  ) %>%
  filter(!is.na(baseline_neet) & !is.na(recent_neet)) %>%
  mutate(neet_pct_change = ((recent_neet - baseline_neet) / baseline_neet) * 100)

# Get world map data
world_map <- map_data("world")

# Match country names between datasets
neet_country_change <- neet_country_change %>%
  mutate(region = case_when(
    country == "United States" ~ "USA",
    country == "Czechia" ~ "Czech Republic",
    country == "Hong Kong" ~ "China",
    TRUE ~ country
  ))

# Join the data
map_data_joined <- world_map %>%
  left_join(neet_country_change, by = "region")

# Create the map
ggplot(map_data_joined, aes(x = long, y = lat, group = group, fill = neet_pct_change)) +
  geom_polygon(color = "white", linewidth = 0.1) +
  scale_fill_gradient2(
    low = "#2166ac",
    mid = "white",
    high = "#b2182b",
    midpoint = 0,
    limits = c(-50, 50),
    na.value = "grey80",
    name = "NEET Change (%)",
    oob = scales::squish
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.width = unit(2, "cm"),
    plot.title = element_text(hjust = 0.0, face = "bold")
  ) +
  coord_fixed(1.3) +
  labs(title = "Figure 8: Percentage Change in Youth NEET Share",
       subtitle = "Country Level Comparison")


# Summary table of missing data percentages by continent (2010-2021 only)
missing_data_summary <- master %>%
  filter(year >= 2010 & year <= 2021) %>%
  group_by(continent) %>%
  summarise(
    `GDP Missing (%)` = round(sum(is.na(gdp_per_capita)) / n() * 100, 1),
    `NEET Missing (%)` = round(sum(is.na(neet_share)) / n() * 100, 1),
    `Education Missing (%)` = round(sum(is.na(edu_expenditure_gdp)) / n() * 100, 1),
    .groups = 'drop'
  )

print(missing_data_summary)

# If you want to save it as a CSV
write_csv(missing_data_summary, "missing_data_by_continent_2010_2021.csv")

print(missing_data_summary)


# Get all countries in the master dataset (2010-2021)
all_countries <- master %>%
  filter(year >= 2010 & year <= 2021) %>%
  distinct(country, continent)

# Calculate NEET change (same logic as your map)
neet_country_change <- master %>%
  filter(!is.na(neet_share)) %>%
  group_by(country, continent) %>%
  arrange(year) %>%
  summarise(
    baseline_neet = first(neet_share[year >= 2010 & year <= 2015]),
    baseline_year = first(year[year >= 2010 & year <= 2015]),
    recent_neet = ifelse(
      any(year == 2020 & !is.na(neet_share)),
      neet_share[year == 2020][1],
      last(neet_share[year >= 2016 & year < 2020])
    ),
    recent_year = ifelse(
      any(year == 2020 & !is.na(neet_share)),
      2020,
      last(year[year >= 2016 & year < 2020])
    ),
    .groups = 'drop'
  ) %>%
  filter(!is.na(baseline_neet) & !is.na(recent_neet)) %>%
  mutate(neet_pct_change = ((recent_neet - baseline_neet) / baseline_neet) * 100)

# Join to see which countries have NEET change data
countries_with_data <- all_countries %>%
  left_join(neet_country_change %>% select(country, neet_pct_change), by = "country")

# Calculate missing percentages by continent
missing_neet_change_summary <- countries_with_data %>%
  group_by(continent) %>%
  summarise(
    `NEET Change Missing (%)` = round(sum(is.na(neet_pct_change)) / n() * 100, 1),
    .groups = 'drop'
  )

print(missing_neet_change_summary)

# If you want to save it
write_csv(missing_neet_change_summary, "missing_neet_change_by_continent.csv")


# #13 - aggregate by population Step 1: Calculate population-weighted NEET for each country in baseline and recent periods
neet_population_weighted <- master %>%
  group_by(country, continent) %>%
  arrange(year) %>%
  summarise(
    # Baseline period (2010-2015)
    baseline_neet = first(neet_share[year >= 2010 & year <= 2015 & !is.na(neet_share)]),
    baseline_year = first(year[year >= 2010 & year <= 2015 & !is.na(neet_share)]),
    baseline_pop = first(population[year >= 2010 & year <= 2015 & !is.na(population) & !is.na(neet_share)]),
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
  ) %>%
  filter(!is.na(baseline_neet) & !is.na(recent_neet) & !is.na(baseline_pop) & !is.na(recent_pop))

# Step 2: Calculate total population by continent for each period
continent_totals <- neet_population_weighted %>%
  group_by(continent) %>%
  summarise(
    total_baseline_pop = sum(baseline_pop),
    total_recent_pop = sum(recent_pop),
    .groups = 'drop'
  )

# Step 3: Join and calculate population-weighted NEET for each country
neet_with_weights <- neet_population_weighted %>%
  left_join(continent_totals, by = "continent") %>%
  mutate(
    # Population-weighted NEET = NEET share * (country pop / total continent pop)
    baseline_weighted_neet = baseline_neet * (baseline_pop / total_baseline_pop),
    recent_weighted_neet = recent_neet * (recent_pop / total_recent_pop)
  )

# Step 4: Calculate population-weighted average (sum for continent)
continent_weighted_avg <- neet_with_weights %>%
  group_by(continent) %>%
  summarise(
    baseline_pop_weighted_avg = sum(baseline_weighted_neet),
    recent_pop_weighted_avg = sum(recent_weighted_neet),
    .groups = 'drop'
  ) %>%
  mutate(
    # Change in population-weighted average
    change_pop_weighted = recent_pop_weighted_avg - baseline_pop_weighted_avg,
    pct_change_pop_weighted = ((recent_pop_weighted_avg - baseline_pop_weighted_avg) / baseline_pop_weighted_avg) * 100
  )

print("Population-weighted average NEET by continent:")
print(continent_weighted_avg)

# Step 5: Determine target achievement for each continent based on baseline NEET level
target_achievement <- continent_weighted_avg %>%
  mutate(
    # Determine target based on baseline population-weighted average
    target_reduction = case_when(
      baseline_pop_weighted_avg < 10 ~ -15,  # Target: 15% reduction
      baseline_pop_weighted_avg >= 10 & baseline_pop_weighted_avg <= 30 ~ -25,  # Target: 25% reduction
      baseline_pop_weighted_avg > 30 ~ -15,  # Target: 15% reduction
      TRUE ~ NA_real_
    ),
    # Determine if target was achieved
    target_status = case_when(
      is.na(target_reduction) ~ "NA",
      pct_change_pop_weighted <= target_reduction ~ "Achieved",
      TRUE ~ "Missed"
    ),
    neet_category = case_when(
      baseline_pop_weighted_avg < 10 ~ "<10% (15% reduction target)",
      baseline_pop_weighted_avg >= 10 & baseline_pop_weighted_avg <= 30 ~ "10-30% (25% reduction target)",
      baseline_pop_weighted_avg > 30 ~ ">30% (15% reduction target)",
      TRUE ~ "NA"
    )
  )

print("\nTarget achievement by continent:")
print(target_achievement %>% select(continent, baseline_pop_weighted_avg, recent_pop_weighted_avg, 
                                    pct_change_pop_weighted, target_reduction, target_status, neet_category))

# Step 6: Create summary for stacked bar chart (count continents by status)
continent_status <- target_achievement %>%
  select(continent, target_status, neet_category, pct_change_pop_weighted, target_reduction)

# Step 7: Create bar chart showing target achievement by continent
ggplot(continent_status, aes(x = continent, y = 1, fill = target_status)) +
  geom_col(alpha = 0.8, width = 0.7) +
  scale_fill_manual(
    values = c(
      "Achieved" = "#2166ac",
      "Missed" = "#b2182b",
      "NA" = "grey80"
    )
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(
    x = "Continent",
    y = "",
    fill = "Target Achievement",
    title = "Population-Weighted NEET Reduction Target Achievement by Continent",
    subtitle = "Target: <10% → 15% reduction | 10-30% → 25% reduction | >30% → 15% reduction"
  ) +
  geom_text(
    aes(label = paste0(target_status, "\n", 
                       "Change: ", round(pct_change_pop_weighted, 1), "%\n",
                       "Target: ", target_reduction, "%")),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 3
  ) +
  coord_flip()
