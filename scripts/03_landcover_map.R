# Figure 4: Land Cover Classification in Ventura County (NLCD 2021)
library(sf)
library(terra)
library(dplyr)
library(ggplot2)


nlcd_path   <- "data/raw/Annual_NLCD_LndCov_2021_CU_C1V1/Annual_NLCD_LndCov_2021_CU_C1V1.tif"
county_path <- "data/raw/tl_2021_us_county/tl_2021_us_county.shp"
out_png     <- "figures/figure4_landcover_map.png"

if (!dir.exists("figures")) dir.create("figures", recursive = TRUE)


counties <- st_read(county_path, quiet = TRUE)

ventura_sf <- counties |>
  filter(NAME == "Ventura", STATEFP == "06") |>
  st_make_valid()

if (nrow(ventura_sf) == 0) stop("Could not find Ventura County, CA in county shapefile.")

nlcd <- rast(nlcd_path)

ventura_v <- vect(ventura_sf)
ventura_v <- project(ventura_v, crs(nlcd))

nlcd_crop <- crop(nlcd, ext(ventura_v))
nlcd_clip <- mask(nlcd_crop, ventura_v)

nlcd_df <- as.data.frame(nlcd_clip, xy = TRUE, na.rm = TRUE)
names(nlcd_df)[3] <- "landcover"

nlcd_df <- nlcd_df |>
  mutate(
    landcover = as.character(landcover),
    class = case_when(
      landcover %in% c("Deciduous Forest","Evergreen Forest","Mixed Forest") ~ "Forest",
      landcover == "Shrub/Scrub" ~ "Shrubland",
      landcover == "Grassland/Herbaceous" ~ "Grassland",
      landcover %in% c("Pasture/Hay","Cultivated Crops") ~ "Agriculture",
      landcover %in% c(
        "Developed, Open Space",
        "Developed, Low Intensity",
        "Developed, Medium Intensity",
        "Developed, High Intensity"
      ) ~ "Urban",
      TRUE ~ "Other"
    )
  )

nlcd_df$class <- factor(
  nlcd_df$class,
  levels = c("Forest", "Shrubland", "Grassland", "Agriculture", "Urban", "Other")
)

-
ventura_sf_plot <- st_transform(ventura_sf, st_crs(crs(nlcd)))

fig4 <- ggplot() +
  geom_raster(data = nlcd_df, aes(x = x, y = y, fill = class)) +
  geom_sf(data = ventura_sf_plot, fill = NA, color = "black", linewidth = 0.8) +
  scale_fill_manual(values = c(
    Forest = "#006400",
    Shrubland = "#6B8E23",
    Grassland = "#9ACD32",
    Agriculture = "#FFD700",
    Urban = "#E31A1C",
    Other = "gray80"
  )) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Land Cover Classification in Ventura County (NLCD 2021)",
    x = NULL, y = NULL,
    fill = "Land Cover"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

ggsave(out_png, fig4, width = 10, height = 7, dpi = 300)
message("✓ Figure 4 saved: ", out_png)