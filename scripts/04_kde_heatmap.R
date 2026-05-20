# Figure 5: Roadkill KDE Heatmap (2 km bandwidth)

library(sf)
library(terra)
library(dplyr)
library(ggplot2)


county_path <- "data/raw/tl_2021_us_county/tl_2021_us_county.shp"
fig_dir     <- "figures"
out_png     <- file.path(fig_dir, "figure5_roadkill_kde_heatmap.png")
kde_out_tif <- "data/processed/roadkill_kde_2km.tif"

if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
if (!dir.exists(dirname(kde_out_tif))) dir.create(dirname(kde_out_tif), recursive = TRUE)

counties <- st_read(county_path, quiet = TRUE)
ventura_sf <- counties %>%
  filter(NAME == "Ventura") %>%
  st_make_valid()

roadkill_path <- "data/processed/ventura_roadkill_points.gpkg"
roadkill_sf <- st_read(roadkill_path, quiet = TRUE)

if (is.na(st_crs(roadkill_sf))) st_crs(roadkill_sf) <- 4326
stopifnot(nrow(roadkill_sf) > 0)
message("✔ roadkill points loaded: n = ", nrow(roadkill_sf))

# Reproject to CA Albers (meters)
ventura_3310 <- st_transform(ventura_sf, 3310)
roadkill_3310 <- st_transform(roadkill_sf, 3310)

roadkill_3310 <- roadkill_3310[!st_is_empty(st_geometry(roadkill_3310)), ]

roadkill_3310 <- roadkill_3310 %>%
  filter(st_geometry_type(.) %in% c("POINT", "MULTIPOINT")) %>%
  st_cast("POINT", warn = FALSE)

# clip to Ventura in SAME CRS
roadkill_3310 <- st_filter(roadkill_3310, ventura_3310)

message("✔ roadkill points after clip: n = ", nrow(roadkill_3310))
stopifnot(nrow(roadkill_3310) > 0)

v <- vect(ventura_3310)
pts <- vect(roadkill_3310)

r <- rast(ext(v), resolution = 200, crs = "EPSG:3310")

# Rasterize roadkill points
counts <- rasterize(
  pts,
  r,
  field = 1,
  fun = "sum",
  background = 0,
  touches = TRUE
)

print(global(counts, "sum", na.rm = TRUE))
print(global(counts, "max", na.rm = TRUE))

# Gaussian KDE smoothing
bw <- 2000
sigma <- bw / 2

w <- focalMat(counts, d = sigma, type = "Gauss")

kde <- focal(
  counts,
  w = w,
  fun = "sum",
  na.policy = "omit",
  fillvalue = 0
)

# Mask KDE to Ventura County
kde <- mask(kde, v)
print(global(kde, "max", na.rm = TRUE))

writeRaster(kde, kde_out_tif, overwrite = TRUE)
message("✔ KDE raster rebuilt + saved: ", kde_out_tif)


kde_df <- as.data.frame(kde, xy = TRUE, na.rm = TRUE)
names(kde_df)[3] <- "kde"

fig5 <- ggplot() +
  geom_raster(data = kde_df, aes(x = x, y = y, fill = kde)) +
  geom_sf(data = ventura_3310, fill = NA, color = "white", linewidth = 0.8) +
  geom_sf(data = roadkill_sf, color = "white", size = 0.2) +
  coord_sf(expand = FALSE) +
  scale_fill_viridis_c(option = "magma", trans = "sqrt", name = "KDE") +
  labs(title = "Roadkill Density Hotspots (KDE, 2 km bandwidth)") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

print(fig5)

ggsave(out_png, fig5, width = 10, height = 7, dpi = 300)
