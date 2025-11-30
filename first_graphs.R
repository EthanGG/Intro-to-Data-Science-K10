
master <- read.csv('master_dataset.csv')
master %>% mutate(gdp_per_capita = as.numeric(gdp_per_capita),
  year = as.numeric(year), neet_share = as.numeric(neet_share),
  edu_expenditure_gdp = as.numeric(edu_expenditure_gdp),
)
cont_master <- master %>% group_by(continent, year) %>%
  mutate(gdp_per_capita = mean(gdp_per_capita, na.rm = T),
    neet_share = mean(neet_share, na.rm = T),
    du_expenditure_gdp = mean(edu_expenditure_gdp, na.rm = T),
  ) %>%
  ungroup() %>% select(year:edu_expenditure_gdp) %>%
  distinct(year, continent, .keep_all = T)

# GDP per Capita vs Time
ggplot(cont_master, aes(year, log10(gdp_per_capita), colour = continent)) +
  theme_minimal() + theme(
    panel.grid.major = element_line(colour = "grey70"),
    panel.grid.minor = element_line(colour = "grey90", size = 0.4),
    plot.title = element_text(hjust = 0.5)
  ) +
  geom_point(alpha = 0.6) +
  geom_smooth(span = 0.4, se = F) +
  labs(x = 'Year', y = 'GDP per capita (log 10)', colour = 'Continent',
    title = 'Average GDP per Capita per Continent')

# NEET share vs Time
ggplot(cont_master, aes(year, neet_share, colour = continent)) +
  theme_minimal() + theme(
    panel.grid.major = element_line(colour = "grey70"),
    panel.grid.minor = element_line(colour = "grey90", size = 0.4),
    plot.title = element_text(hjust = 0.5)
  ) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = 'lm', span = 0.4, se = F) +
  labs(x = 'Year', y = 'NEET Share (%)', colour = 'Continent',
       title = 'NEET Share per Continent')

# Education Spending as % of GDP vs Time
ggplot(cont_master, aes(year, edu_expenditure_gdp, colour = continent)) +
  theme_minimal() + theme(
    panel.grid.major = element_line(colour = "grey70"),
    panel.grid.minor = element_line(colour = "grey90", size = 0.4),
    plot.title = element_text(hjust = 0.5)
  ) +
  geom_point(alpha = 0.4) +
  geom_smooth(span = 0.7, se = F) +
  labs(x = 'Year', y = 'Education Spending as % of GDP ', colour = 'Continent',
       title = 'Education Spending as % of GDP per Continent')
