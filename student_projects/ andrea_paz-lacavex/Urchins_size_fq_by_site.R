library(dplyr)
library(ggplot2)

# Local mounted file path
file_path <- "/Volumes/enhydra/data/kelp_recovery/MBA_kelp_forest_database/processed/recovery/recovery_urch_sizefq.csv"

# Read the CSV
urch_size <- read.csv(file_path)

# Filter for Purple urchins and ensure numeric size
purple_urchins <- urch_size %>%
  filter(species == "Purple") %>%
  mutate(size_cm = as.numeric(size_cm))

# Calculate weighted mean size per site_type, site, zone
mean_size <- purple_urchins %>%
  filter(!is.na(size_cm) & !is.na(count)) %>%  # remove rows with NA in either
  group_by(site_type, site, zone) %>%
  summarize(mean_size = weighted.mean(size_cm, count), .groups = "drop")


# Build the plot
p <- ggplot(purple_urchins, aes(x = size_cm, y = count)) +
  geom_col(fill = "purple4", width = 1, alpha = 0.7) +
  geom_vline(data = mean_size, aes(xintercept = mean_size), color = "black", linetype = "solid") +
  geom_text(
    data = mean_size,
    aes(x = 1, y = Inf, label = paste0("μ: ", round(mean_size, 1),"cm")),
    inherit.aes = FALSE,
    size = 3,
    color = "black",
    hjust = 0,
    vjust = 1.1
  ) +
  facet_grid(rows = vars(site), cols = vars(site_type, zone)) +
  scale_x_continuous(breaks = seq(floor(min(purple_urchins$size_cm, na.rm = TRUE)),
                                  ceiling(max(purple_urchins$size_cm, na.rm = TRUE)), by = 1)) +
  labs(
    title = "Size Frequency Distribution of Purple Sea Urchins",
    x = "Size (cm)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_line(),
    axis.text.y = element_text()
  )
p


# Save to Downloads folder
ggsave(
  filename = "~/Downloads/purple_urchin_size_distribution.png",
  plot = p,
  width = 14, height = 10, dpi = 600, bg = "white"
)
