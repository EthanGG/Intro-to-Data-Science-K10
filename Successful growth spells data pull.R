library(dplyr)
library(ggplot2)

# Load your data
master <- read.csv('master_dataset.csv')

# List of LDCs (as of recent UN classification)
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

# Calculate GDP growth rates and identify LDC status
gdp_growth_data <- master %>%
  filter(!is.na(gdp_per_capita)) %>%
  mutate(is_ldc = country %in% ldc_countries) %>%
  group_by(country, continent, is_ldc) %>%
  arrange(year) %>%
  mutate(
    # Calculate year-over-year GDP growth rate
    year_diff = year - lag(year),
    gdp_growth = ifelse(year_diff == 1, 
                        ((gdp_per_capita - lag(gdp_per_capita)) / lag(gdp_per_capita)) * 100,
                        NA_real_)
  ) %>%
  filter(!is.na(gdp_growth)) %>%
  ungroup()

# Function to identify growth spells
identify_growth_spells <- function(data, min_growth, spell_length) {
  data %>%
    arrange(year) %>%
    mutate(
      # Check if growth meets threshold
      meets_threshold = gdp_growth >= min_growth,
      # Create groups of consecutive years meeting threshold
      spell_group = cumsum(!meets_threshold | is.na(meets_threshold))
    ) %>%
    group_by(spell_group) %>%
    mutate(
      spell_length_actual = ifelse(all(meets_threshold), n(), 0)
    ) %>%
    ungroup() %>%
    mutate(
      is_successful_spell = spell_length_actual >= spell_length
    )
}

# Identify growth spells for LDCs (7% for 3 years)
ldc_spells <- gdp_growth_data %>%
  filter(is_ldc == TRUE) %>%
  group_by(country) %>%
  identify_growth_spells(min_growth = 7, spell_length = 3) %>%
  ungroup()

# Identify growth spells for non-LDCs (2% for 5 years)
non_ldc_spells <- gdp_growth_data %>%
  filter(is_ldc == FALSE) %>%
  group_by(country) %>%
  identify_growth_spells(min_growth = 2, spell_length = 5) %>%
  ungroup()

# Count successful spells
ldc_spell_summary <- ldc_spells %>%
  filter(is_successful_spell == TRUE) %>%
  group_by(country, continent, spell_group) %>%
  summarise(
    spell_start_year = min(year),
    spell_end_year = max(year),
    spell_length = n(),
    avg_growth = mean(gdp_growth),
    .groups = 'drop'
  )

non_ldc_spell_summary <- non_ldc_spells %>%
  filter(is_successful_spell == TRUE) %>%
  group_by(country, continent, spell_group) %>%
  summarise(
    spell_start_year = min(year),
    spell_end_year = max(year),
    spell_length = n(),
    avg_growth = mean(gdp_growth),
    .groups = 'drop'
  )

# Overall summary
print("LDC Growth Spells (7% for 3+ consecutive years):")
view(ldc_spell_summary)
print(paste0("\nTotal LDC growth spells: ", nrow(ldc_spell_summary)))

print("\n\nNon-LDC Growth Spells (2% for 5+ consecutive years):")
view(non_ldc_spell_summary)
print(paste0("\nTotal non-LDC growth spells: ", nrow(non_ldc_spell_summary)))

# Count by country
ldc_by_country <- ldc_spell_summary %>%
  group_by(country, continent) %>%
  summarise(
    n_spells = n(),
    avg_spell_length = mean(spell_length),
    .groups = 'drop'
  ) %>%
  arrange(desc(n_spells))

non_ldc_by_country <- non_ldc_spell_summary %>%
  group_by(country, continent) %>%
  summarise(
    n_spells = n(),
    avg_spell_length = mean(spell_length),
    .groups = 'drop'
  ) %>%
  arrange(desc(n_spells))

print("\n\nLDCs with most growth spells:")
print(head(ldc_by_country, 10))

print("\n\nNon-LDCs with most growth spells:")
print(head(non_ldc_by_country, 10))

# Visualize number of growth spells by continent
all_spells <- bind_rows(
  ldc_spell_summary %>% mutate(group = "LDC (7% for 3yrs)"),
  non_ldc_spell_summary %>% mutate(group = "Non-LDC (2% for 5yrs)")
)

spell_by_continent <- all_spells %>%
  group_by(continent, group) %>%
  summarise(n_spells = n(), .groups = 'drop')

ggplot(spell_by_continent, aes(x = continent, y = n_spells, fill = group)) +
  geom_col(position = "dodge", alpha = 0.8) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey70"),
    panel.grid.minor = element_line(colour = "grey90", linewidth = 0.4),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = "Continent",
    y = "Number of Growth Spells",
    fill = "Country Type",
    title = "Successful Growth Spells by Continent",
    subtitle = "LDCs: 7% growth for 3+ years | Non-LDCs: 2% growth for 5+ years"
  )

# Save results
write_csv(ldc_spell_summary, "ldc_growth_spells.csv")
write_csv(non_ldc_spell_summary, "non_ldc_growth_spells.csv")
write_csv(ldc_by_country, "ldc_spells_by_country.csv")
write_csv(non_ldc_by_country, "non_ldc_spells_by_country.csv")