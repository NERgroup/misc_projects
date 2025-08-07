
rm(list=ls())

################################################################################
#load packages and set directories

require(librarian)
librarian::shelf(tidyverse, ggplot2, here, readxl, janitor, scales, lubridate)

datin <- "/Volumes/enhydra/data/cbb_wet_lab/temp_data/raw"

temp_raw <- read_excel(file.path(datin, "CBB_temp_data.xlsx")) %>%
              janitor::clean_names()

################################################################################
#plot


# Trim data to start from August 1, 2025 at 00:00
temp_trimmed <- temp_raw %>%
  filter(date_time_pdt >= as.POSIXct("2025-08-01 00:00:00", tz = "America/Los_Angeles"))

# Generate breaks at 6am and 6pm for each day in the data range
breaks_6am_6pm <- seq(
  from = floor_date(min(temp_trimmed$date_time_pdt), unit = "day") + hours(6),
  to   = ceiling_date(max(temp_trimmed$date_time_pdt), unit = "day") - hours(6),
  by   = "12 hours"
)

# Plot with custom breaks
p <- ggplot(temp_trimmed, aes(x = date_time_pdt, y = temperature_c)) +
  geom_line(color = "steelblue") +
  scale_x_datetime(
    breaks = breaks_6am_6pm,
    date_labels = "%b %d\n%H:%M"
  ) +
  labs(
    x = "Date & Time",
    y = "Temperature (°C)",
    title = "Temperature in CBB 115"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5, size = 8)
  )

# Export
ggsave(
  filename = "~/Downloads/CBB_temp.png",
  plot = p,
  width = 10,
  height = 5,
  dpi = 600,
  bg = "white"
)









