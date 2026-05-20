# Figure 6: Corridor Suitability Map (Habitat + Roads + Roadkill)

library(sf)
library(terra)
library(dplyr)
library(ggplot2)

county_path <- "data/raw/tl_2021_us_county/tl_2021_us_county.shp"
nlcd_path   <- "data/raw/Annual_NLCD_LndCov_2021_CU_C1V1/Annual_NLCD_LndCov_2021_CU_C1V1.tif"
roads_path  <- "data/raw/ventura_roads.shp"
kde_path    <- "data/processed/roadkill_kde_2km.tif"

fig_dir <- "figures"
out_png <- file.path(fig_dir, "figure6_corridor_suitability.png")
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)


counties <- st_read(county_path, quiet = TRUE)
ventura_sf <- counties %>%
  filter(NAME == "Ventura") %>%
  st_make_valid() %>%
  st_transform(3310)

v <- vect(ventura_sf)

nlcd <- rast(nlcd_path)
nlcd <- as.numeric(nlcd)
kde  <- rast(kde_path)

ventura_3310 <- ventura_sf
ventura_nlcd <- st_transform(ventura_sf, crs(nlcd))

v_3310 <- vect(ventura_3310)
v_nlcd <- vect(ventura_nlcd)

#  NLCD: crop in its native CRS first
nlcd <- crop(nlcd, v_nlcd)
nlcd <- aggregate(nlcd, fact = 8, fun = "modal")
nlcd <- project(nlcd, "EPSG:3310", method = "near")
nlcd <- mask(nlcd, v_3310)

# KDE: already in 3310
if (crs(kde) != "EPSG:3310") {
  kde <- project(kde, "EPSG:3310", method = "bilinear")
}
kde <- crop(kde, v_3310)
kde <- mask(kde, v_3310)

roads_sf <- st_read(roads_path, quiet = TRUE) %>% st_transform(3310)
roads_v  <- vect(roads_sf)

# Distance-to-road raster (meters)
roads_r <- rasterize(roads_v, nlcd, field = 1, background = NA)
dist_to_road <- distance(roads_r)  # meters to nearest road cell

# Habitat suitability from NLCD 
# Common NLCD codes:
# 21-24 Developed, 31 Barren, 41 Forest, 52 Shrub, 71 Grass, 81 Pasture, 82 Crops, 90-95 Wetlands/Water
hab <- classify(
  nlcd,
  rcl = matrix(c(
    21, 24, 0.05, # developed -> very low suitability
    31, 31, 0.20, # barren -> low
    41, 43, 1.00, # forest -> high
    52, 52, 0.85, # shrub -> high-ish
    71, 71, 0.70, # grassland -> medium-high
    81, 82, 0.35, # ag -> lower
    90, 95, 0.10  # wetlands/water -> low for terrestrial corridor
  ), ncol = 3, byrow = TRUE),
  others = 0.30  # default if class not listed
)

# Convert dist_to_road to suitability, farther from roads = better
dmax <- 5000
road_suit <- clamp(dist_to_road, lower = 0, upper = dmax) / dmax

# KDE to "risk" then invert, lower roadkill risk = better
kmax <- global(kde, "max", na.rm = TRUE)[1,1]

if (kmax > 0) {
  kde_norm <- kde / kmax
} else {
  kde_norm <- kde * 0
}

risk_suit <- 1 - kde_norm

road_suit <- resample(road_suit, nlcd, method = "bilinear")
risk_suit <- resample(risk_suit, nlcd, method = "bilinear")

hab <- resample(hab, nlcd, method = "near")

print(ext(hab))
print(ext(road_suit))
print(ext(risk_suit))

print(res(hab))
print(res(road_suit))
print(res(risk_suit))


suit <- 0.50 * hab + 0.30 * road_suit + 0.20 * risk_suit
suit <- clamp(suit, 0, 1)

suit_df <- as.data.frame(suit, xy = TRUE, na.rm = TRUE)
names(suit_df)[3] <- "suitability"


fig6 <- ggplot() +
  geom_raster(data = suit_df, aes(x = x, y = y, fill = suitability)) +
  geom_sf(data = ventura_sf, fill = NA, color = "white", linewidth = 0.6) +
  coord_sf(expand = FALSE) +
  scale_fill_viridis_c(option = "viridis", name = "Suitability") +
  labs(title = "Corridor Suitability Surface (Habitat + Roads + Roadkill)") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

print(fig6)
ggsave(out_png, fig6, width = 10, height = 7, dpi = 300)
