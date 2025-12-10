library(dplyr)
library(ggplot2)

# Load your data
master <- read.csv('master_dataset.csv')

# Calculate baseline and recent NEET for each country
neet_boxplot_data <- master %>%
  group_by(country, continent) %>%
  arrange(year) %>%
  summarise(
    # Baseline period (2010-2015) - take last available year
    baseline_neet = last(neet_share[year >= 2010 & year <= 2015 & !is.na(neet_share)]),
    baseline_year = last(year[year >= 2010 & year <= 2015 & !is.na(neet_share)]),
    # Recent period (2016-2020) - prioritize 2020, then take last available
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
    .groups = 'drop'
  ) %>%
  filter(!is.na(baseline_neet) & !is.na(recent_neet))


# Plot 1: Baseline NEET variance by continent
ggplot(neet_boxplot_data, aes(x = continent, y = baseline_neet, fill = continent)) +
  geom_boxplot(alpha = 0.7) +
  coord_cartesian(ylim = c(0, 70)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey70"),
    panel.grid.minor = element_line(colour = "grey90", linewidth = 0.4),
    legend.position = "none",
    plot.title = element_text(hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = "Continent",
    y = "Youth NEET Rate (%)",
    title = "Figure 10: Variance of Youth NEET Rates by Continent",
    subtitle = "Baseline Period: 2010-2015 (Last Available Year)"
  )

# Plot 2: Recent NEET variance by continent
ggplot(neet_boxplot_data, aes(x = continent, y = recent_neet, fill = continent)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey70"),
    panel.grid.minor = element_line(colour = "grey90", linewidth = 0.4),
    legend.position = "none",
    plot.title = element_text(hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = "Continent",
    y = "Youth NEET Rate (%)",
    title = "Figure 11: Variance of Youth NEET Rates by Continent",
    subtitle = "Comparison Period: 2016-2020 (Prioritize 2020, Then Last Available)"
  )

# Optional: Combined plot showing both periods side by side
library(tidyr)

neet_combined <- neet_boxplot_data %>%
  select(country, continent, baseline_neet, recent_neet) %>%
  pivot_longer(
    cols = c(baseline_neet, recent_neet),
    names_to = "period",
    values_to = "neet_rate"
  ) %>%
  mutate(
    period = ifelse(period == "baseline_neet", "Baseline (2010-2015)", "Recent (2016-2020)")
  )

ggplot(neet_combined, aes(x = continent, y = neet_rate, fill = period)) +
  geom_boxplot(alpha = 0.7, position = position_dodge(width = 0.8)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey70"),
    panel.grid.minor = element_line(colour = "grey90", linewidth = 0.4),
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.title = element_text(hjust = 0, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = "Continent",
    y = "Youth NEET Rate (%)",
    fill = "Period",
    title = "Comparison of Youth NEET Rate Variance by Continent",
    subtitle = "Baseline vs Recent Period"
  ) +
  scale_fill_manual(values = c("Baseline (2010-2015)" = "#fee08b", 
                               "Recent (2016-2020)" = "#2166ac"))

# Summary statistics
summary_stats <- neet_boxplot_data %>%
  group_by(continent) %>%
  summarise(
    baseline_median = median(baseline_neet, na.rm = TRUE),
    baseline_mean = mean(baseline_neet, na.rm = TRUE),
    baseline_sd = sd(baseline_neet, na.rm = TRUE),
    recent_median = median(recent_neet, na.rm = TRUE),
    recent_mean = mean(recent_neet, na.rm = TRUE),
    recent_sd = sd(recent_neet, na.rm = TRUE),
    n_countries = n(),
    .groups = 'drop'
  )

print("Summary statistics by continent:")
print(summary_stats)

library(dplyr)
library(ggplot2)

# Load your data
master <- read.csv('master_dataset.csv')

# Calculate NEET change for each country
neet_change_data <- master %>%
  group_by(country, continent) %>%
  arrange(year) %>%
  summarise(
    # Baseline period (2010-2015) - take last available year
    baseline_neet = last(neet_share[year >= 2010 & year <= 2015 & !is.na(neet_share)]),
    # Recent period (2016-2020) - prioritize 2020, then take last available
    recent_neet = ifelse(
      any(year == 2020 & !is.na(neet_share)),
      neet_share[year == 2020 & !is.na(neet_share)][1],
      last(neet_share[year >= 2016 & year < 2020 & !is.na(neet_share)])
    ),
    .groups = 'drop'
  ) %>%
  filter(!is.na(baseline_neet) & !is.na(recent_neet)) %>%
  mutate(
    neet_change = ((recent_neet - baseline_neet) / baseline_neet) * 100
  )

# Calculate median for each continent
median_values <- neet_change_data %>%
  group_by(continent) %>%
  summarise(median_change = median(neet_change, na.rm = TRUE), .groups = 'drop')

# Create box plot with median values at the base
ggplot(neet_change_data, aes(x = continent, y = neet_change, fill = continent)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_text(data = median_values,
            aes(x = continent, y = -40, label = paste0(round(median_change, 1), "%")),
            fontface = "bold", size = 3.5) +
  coord_cartesian(ylim = c(-50, 200)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey70"),
    panel.grid.minor = element_line(colour = "grey90", linewidth = 0.4),
    legend.position = "none",
    plot.title = element_text(hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = "Continent",
    y = "NEET Change (%)",
    title = "Figure 10: Variance of NEET Change by Continent",
    subtitle = "Baseline: 2010-2015 vs Recent: 2016-2020 "
  )

# Print median values
print("Median NEET change by continent:")
print(median_values)

#NEW PLOT: DOT PLOT
neet_ranges <- neet_change_data %>%
  mutate(
    baseline_range = cut(
      baseline_neet,
      breaks = c(-Inf, 10, 20, 30, 40, Inf),
      labels = c("<10", "10–20", "20–30", "30–40", ">40"),
      right = FALSE
    ),
    recent_range = cut(
      recent_neet,
      breaks = c(-Inf, 10, 20, 30, 40, Inf),
      labels = c("<10", "10–20", "20–30", "30–40", ">40"),
      right = FALSE
    )
  )

baseline_counts <- neet_ranges %>%
  filter(baseline_range %in% c("<10", ">40")) %>%
  group_by(continent, baseline_range) %>%
  summarise(n = n(), .groups = "drop")

recent_counts <- neet_ranges %>%
  filter(recent_range %in% c("<10", ">40")) %>%
  group_by(continent, recent_range) %>%
  summarise(n = n(), .groups = "drop")

common_breaks <- c(1,5,10,20)

baseline_counts_filtered <- baseline_counts %>%
  filter(continent != "North America")


ggplot(baseline_counts_filtered, aes(y = baseline_range, x = continent)) +
  geom_point(aes(size = n, color = continent)) +
  scale_size_continuous(range = c(3, 12),
  breaks = common_breaks,
  limits = c(0,max(common_breaks))
  ) +
  guides(color = "none") +   # ← removes continent colour legend
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.2),
    plot.subtitle = element_text(face = "bold")
  ) +
  labs(
    title = "Figure 12a: Distribution of Baseline NEET Levels by Continent (2010–2015)",
    x = "Continent",
    y = "Baseline NEET Range (%)",
    size = "Number of Countries"
  )

ggplot(recent_counts, aes(x = continent, y = recent_range)) +
  geom_point(aes(size = n, color = continent)) +
  scale_size_continuous(range = c(3, 12),
                        breaks = common_breaks,
                        limits = c(0,max(common_breaks))
  ) +
  guides(color = "none") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.2),
    plot.subtitle = element_text(face = "bold")
  ) +
  labs(
    title = "Figure 12b: Distribution of Comparison NEET Levels by Continent (2016–2020)",
    x = "Continent",
    y = "Recent NEET Range (%)",
    size = "Number of Countries"
  )

