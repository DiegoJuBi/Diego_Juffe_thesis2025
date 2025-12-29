# This is a script created by Diego Juffe to pot the sensitivity analyses results of his PhD thesis. Script developed through August-September 2025.
# Load libraries
library(tidyverse)

# Load CSV
df <- read.csv("C:/R_scripts/PhD/Decay_data_R.csv", fileEncoding = "UTF-8-BOM")
str(df)

# Clean column names
colnames(df)[1] <- "Distance"
colnames(df) <- gsub("\\.", "_", colnames(df))

# Reshape to long format
df_long <- df %>%
  pivot_longer(cols = -Distance, names_to = "Curve", values_to = "Impact") %>%
  mutate(
    Distance_km = Distance / 1000,
    Epsilon = case_when(
      str_detect(Curve, "0_1") ~ "e = 0.1",
      str_detect(Curve, "0_01") ~ "e = 0.01",
      str_detect(Curve, "0_001") ~ "e = 0.001",
      TRUE ~ "Unknown"
    ),
    D = case_when(
      str_detect(Curve, "15km") ~ "D = 15 km",
      str_detect(Curve, "25km") ~ "D = 25 km",
      str_detect(Curve, "50km") ~ "D = 50 km",
      TRUE ~ "Unknown"
    )
  )

# Define new colors (for Distance D)
distance_colors <- c("D = 15 km" = "#E41A1C", "D = 25 km" = "#4DAF4A", "D = 50 km" = "#377EB8")

# Define new line types (for Epsilon)
epsilon_linetypes <- c("e = 0.1" = "longdash", "e = 0.01" = "solid", "e = 0.001" = "twodash")

# Plot
decay_plot <- ggplot(df_long, aes(x = Distance_km, y = Impact, color = D, linetype = Epsilon)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = distance_colors) +
  scale_linetype_manual(values = epsilon_linetypes) +
  labs(
    title = "Exponential Decay Impact-Distance Curves",
    x = "Distance from Source (km)",
    y = "Impact Value (scaled)",
    color = "Max Distance (D)",
    linetype = "End point values (e)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Show plot
print(decay_plot)

# Export to PNG with white background
ggsave("C:/R_scripts/PhD/Decay_Curves_Final_Clean.png", plot = decay_plot,
       width = 10, height = 6, dpi = 300, bg = "white")

