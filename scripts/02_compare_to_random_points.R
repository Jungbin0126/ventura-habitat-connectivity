library(sf)
library(dplyr)
library(ggplot2)


roadkill_path <- "data/raw/roadkill_to_road_distance.shp"
county_path   <- "data/raw/tl_2021_us_county/tl_2021_us_county.shp"
fig_dir  <- "figures"
proc_dir <- "data/processed"
if (!dir.exists(fig_dir)) dir.create(fig_dir)
if (!dir.exists(proc_dir)) dir.create(proc_dir)

rk <- st_read(roadkill_path, quiet = TRUE)

counties <- st_read(county_path, quiet = TRUE)

ventura <- counties %>%
  filter(NAME == "Ventura") %>%
  st_transform(st_crs(rk)) %>%
  st_make_valid()

rk <- st_transform(rk, st_crs(ventura))

# Road network layer for distance calc 
roads_path <- "data/raw/ventura_roads.shp"
roads <- st_read(roads_path, quiet = TRUE) %>%
  st_transform(st_crs(ventura)) %>%
  st_make_valid()

# Get roadkill distances (meters)
rk_df <- rk %>%
  st_drop_geometry() %>%
  transmute(type = "Roadkill", dist_m = as.numeric(HubDist))

set.seed(42)
n <- nrow(rk)

random_pts <- st_sample(ventura, size = n, type = "random") %>%
  st_as_sf() %>%
  st_set_crs(st_crs(ventura))

# Distance from random points to nearest road 
# st_distance gives matrix; take min per point
dmat <- st_distance(random_pts, roads)
rand_dist <- apply(dmat, 1, min)

rand_df <- tibble(
  type = "Random",
  dist_m = as.numeric(rand_dist)
)


all_df <- bind_rows(rk_df, rand_df)
write.csv(all_df, "data/processed/roadkill_vs_random_distances.csv", row.names = FALSE)


w <- wilcox.test(dist_m ~ type, data = all_df, exact = FALSE)
print(w)
stats_out <- data.frame(
  n = n,
  roadkill_median = median(rk_df$dist_m, na.rm = TRUE),
  random_median   = median(rand_df$dist_m, na.rm = TRUE),
  p_value = w$p.value
)
write.csv(stats_out, "data/processed/wilcox_roadkill_vs_random.csv", row.names = FALSE)


p <- ggplot(all_df, aes(x = type, y = dist_m)) +
  geom_boxplot(outlier.alpha = 0.6) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Roadkill vs Random: distance to nearest road (Ventura County)",
    x = "",
    y = "Distance to nearest road (meters)"
  ) +
  theme_minimal()

ggsave("figures/roadkill_vs_random_boxplot.png", p, width = 7, height = 5, dpi = 300)


