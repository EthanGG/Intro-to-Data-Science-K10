library(maps)
library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)

master <- read.csv('master_dataset.csv')
view(master)

# List of LDCs (as of recent UN classification)
# You'll need to check which countries in your dataset are classified as LDCs
ldc_countries <- c(
  "Afghanistan", "Angola", "Bangladesh", "Benin", "Bhutan", "Burkina Faso",
  "Burundi", "Cambodia", "Central African Republic", "Chad", "Comoros",
  "Congo, Dem. Rep.", "Djibouti", "Eritrea", "Ethiopia", "Gambia", "Guinea",
  "Guinea-Bissau", "Haiti", "Kiribati", "Laos", "Lesotho", "Liberia",
  "Madagascar", "Malawi", "Mali", "Mauritania", "Mozambique", "Myanmar",
  "Nepal", "Niger", "Rwanda", "Sao Tome and Principe", "Senegal",
  "Sierra Leone", "Solomon Islands", "Somalia", "South Sudan", "Sudan",
  "Tanzania", "Timor-Leste", "Togo", "Tuvalu", "Uganda", "Yemen", "Zambia"
)

# Get unique countries from your dataset
all_countries_in_data <- master %>%
  distinct(country, continent)

# Mark which countries in your dataset are LDCs
countries_ldc_status <- all_countries_in_data %>%
  mutate(
    is_ldc = country %in% ldc_countries,
    ldc_status = ifelse(is_ldc, "LDC", "Non-LDC")
  )

print("LDCs in your dataset:")
print(countries_ldc_status %>% filter(is_ldc))

# Get world map data
world_map <- map_data("world")

# Match country names for mapping
countries_ldc_status <- countries_ldc_status %>%
  mutate(region = case_when(
    country == "United States" ~ "USA",
    country == "United Kingdom" ~ "UK",
    country == "Czechia" ~ "Czech Republic",
    country == "Congo, Dem. Rep." ~ "Democratic Republic of the Congo",
    country == "Tanzania" ~ "Tanzania",
    country == "Laos" ~ "Laos",
    country == "Myanmar" ~ "Myanmar",
    country == "South Sudan" ~ "South Sudan",
    TRUE ~ country
  ))

# Join with map data
map_data_ldc <- world_map %>%
  left_join(countries_ldc_status, by = "region")

# Create heatmap
ggplot(map_data_ldc, aes(x = long, y = lat, group = group, fill = ldc_status)) +
  geom_polygon(color = "white", linewidth = 0.1) +
  scale_fill_manual(
    values = c(
      "LDC" = "#b2182b",
      "Non-LDC" = "#2166ac"
    ),
    na.value = "grey90"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.title = element_text(hjust = 0, face = "bold")
  ) +
  coord_fixed(1.3) +
  labs(
    fill = "Country Status",
    title = "Global Distribution of Least Developed Countries (LDCs)"
  )

# Alternative: Show only LDCs highlighted
ggplot(map_data_ldc, aes(x = long, y = lat, group = group, fill = is_ldc)) +
  geom_polygon(color = "white", linewidth = 0.1) +
  scale_fill_manual(
    values = c(
      "TRUE" = "#b2182b",
      "FALSE" = "grey90"
    ),
    labels = c("Non-LDC", "LDC"),
    na.value = "grey90"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.title = element_text(hjust = 0, face = "bold")
  ) +
  coord_fixed(1.3) +
  labs(
    fill = "",
    title = "Least Developed Countries (LDCs) Worldwide"
  )

# Summary statistics
ldc_summary <- countries_ldc_status %>%
  group_by(continent, ldc_status) %>%
  summarise(n_countries = n(), .groups = 'drop') %>%
  pivot_wider(names_from = ldc_status, values_from = n_countries, values_fill = 0)

print("\nLDC distribution by continent:")
print(ldc_summary)

# Save
write_csv(countries_ldc_status, "ldc_status_by_country.csv")