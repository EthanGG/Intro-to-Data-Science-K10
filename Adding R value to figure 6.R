# Calculate R values for each continent and group
r_values <- sdi_growth %>%
  group_by(continent, group_label) %>%
  summarise(
    r = cor(sdi_score, gdp_growth_rate, use = "complete.obs"),
    .groups = "drop"
  ) %>%
  mutate(
    r_label = paste0("R: ", round(r, 3))
  )

# Split into LDCs and Non-LDCs for separate layers
r_ldcs <- r_values %>% 
  filter(group_label == "LDCs") %>%
  mutate(y_pos = 9.5, x_pos = 0.15)

r_non_ldcs <- r_values %>% 
  filter(group_label == "Non-LDCs") %>%
  mutate(y_pos = 8.5, x_pos = 0.15)

# Recreate Figure 6 with R values
fig6 <- ggplot(sdi_growth, aes(x = sdi_score, y = gdp_growth_rate, 
                               color = group_label)) +
  geom_point(alpha = 0.4, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.3) +
  
  # Reference lines: 7% target (red dashed) and 2% baseline (blue dashed)
  geom_hline(yintercept = 7, linetype = "dashed", color = "#e74c3c", linewidth = 1.2) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "#3498db", linewidth = 1.2) +
  
  # Add R value labels for LDCs (red)
  geom_text(data = r_ldcs, 
            aes(x = x_pos, y = y_pos, label = r_label),
            color = "#e74c3c",
            hjust = 0, vjust = 0, size = 4, fontface = "bold",
            inherit.aes = FALSE) +
  
  # Add R value labels for Non-LDCs (blue)
  geom_text(data = r_non_ldcs, 
            aes(x = x_pos, y = y_pos, label = r_label),
            color = "#3498db",
            hjust = 0, vjust = 0, size = 4, fontface = "bold",
            inherit.aes = FALSE) +
  
  facet_wrap(~continent, ncol = 3) +
  
  labs(
    title = "Figure 6: Sustainable Development vs Economic Growth",
    subtitle = paste0("SDI Score vs GDP Growth (", year_label, ") | Red dashed: 7% target; Blue dashed: 2% baseline"),
    x = "Sustainable Development Index (Higher = More Sustainable)",
    y = "GDP Growth Rate (%)",
    color = "Country Group"
  ) +
  scale_color_manual(values = c("LDCs" = "#e74c3c", "Non-LDCs" = "#3498db")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 13),
    strip.text = element_text(face = "bold", size = 14),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 12)
  ) +
  coord_cartesian(ylim = c(-10, 10))  

print(fig6)

ggsave("figure6_sdi_vs_growth.png", fig6, width = 14, height = 10, dpi = 300)
cat("  ✓ Saved: figure6_sdi_vs_growth.png with R values\n\n")