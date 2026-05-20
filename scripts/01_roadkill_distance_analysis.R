library(sf)
library(dplyr)
library(ggplot2)

roadkill <- st_read("data/raw/ventura_roadkill_points.shp", quiet = TRUE)
roads <- st_read("data/raw/ventura_roads.shp", quiet = TRUE)

roadkill <- st_transform(roadkill, 32611)
roads <- st_transform(roads, 32611)

roads_union <- st_union(roads)
roadkill$HubDist <- as.numeric(st_distance(roadkill, roads_union))

roadkill_df <- roadkill |>
  st_drop_geometry()

summary_stats <- roadkill_df |>
  summarise(
    n = n(),
    mean_m = mean(HubDist, na.rm = TRUE),
    median_m = median(HubDist, na.rm = TRUE),
    sd_m = sd(HubDist, na.rm = TRUE),
    min_m = min(HubDist, na.rm = TRUE),
    max_m = max(HubDist, na.rm = TRUE),
    q1_m = quantile(HubDist, 0.25, na.rm = TRUE),
    q3_m = quantile(HubDist, 0.75, na.rm = TRUE),
    iqr_m = IQR(HubDist, na.rm = TRUE)
  )

print(summary_stats)

write.csv(summary_stats, "data/processed/roadkill_distance_summary.csv", row.names = FALSE)

p <- ggplot(roadkill_df, aes(x = HubDist)) +
  geom_histogram(bins = 10, fill = "steelblue", color = "white") +
  labs(
    title = "Distance from roadkill to nearest road (Ventura County)",
    x = "Distance (meters)",
    y = "Number of observations"
  ) +
  theme_minimal()

ggsave("figures/roadkill_distance_histogram.png", plot = p, width = 7, height = 5, dpi = 300)