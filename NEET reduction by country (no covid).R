library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)

master <- read.csv('master_dataset.csv')
view(master)

# Calculate percentage change in NEET share for each country
# Using earliest available year between 2010-2015 as baseline
# and 2020 or closest year before 2020 (2016-2019) as endpoint
neet_country_change <- master %>%
  filter(!is.na(neet_share)) %>%
  group_by(country) %>%
  arrange(year) %>%
  summarise(
    baseline_neet = first(neet_share[year >= 2010 & year <= 2015]),
    baseline_year = first(year[year >= 2010 & year <= 2015]),
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
       subtitle = "Country Level Comparison (EXCLUDING COVID YEARS)")
