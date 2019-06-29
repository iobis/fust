library(mapview)
library(robis)
library(sp)
library(dplyr)
library(rgdal)
library(ggplot2)
library(sf)
library(maptools)
library(ggrepel)

tolerance <- 0.01

color_eez <- "#e1e9ee"
color_land <- "#fa9355"
color_stations <- "#adc4d1"

countries <- c(
  "East Timor",
  "Palau",
  "Papua New Guinea",
  "Protected Zone established under the Torres Strait Treaty",
  "Micronesia",
  "Marshall Islands",
  "Nauru",
  "Kiribati",
  "Solomon Islands",
  "Tuvalu",
  "Vanuatu",
  "Samoa",
  "American Samoa",
  "Fiji",
  "Tonga",
  "Niue",
  "Cook Islands"
)

areaids <- c(57, 185, 189, 16, 149, 143, 158, 132, 216, 244, 279, 208, 267, 68, 240, 170, 169)

# occurrences

if (file.exists("occurrence.dat")) {
  load("occurrence.dat")
} else {
  occ <- occurrence(areaid = areaids, fields = c("decimalLongitude", "decimalLatitude", "dataset_id"), exclude = "bath_issue")
  save(occ, file = "occurrence.dat")
}
stations <- occ %>% mutate(decimalLongitude = round(decimalLongitude, -log10(tolerance)), decimalLatitude = round(decimalLatitude, -log10(tolerance))) %>% group_by(decimalLongitude, decimalLatitude) %>% summarize(records = n())
stations_sf <- st_as_sf(stations, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# eez

eez <- read_sf(dsn = "shapefiles/eez_filtered.shp", layer = "eez_filtered")
eez_filtered <- eez %>% filter(Territory1 %in% countries | GeoName %in% countries | Sovereign1 %in% countries)
eez_nowrap_temp <- st_as_sf(nowrapSpatialPolygons(as(eez_filtered, "Spatial")))
eez_nowrap <- eez_filtered
eez_nowrap$geometry <- eez_nowrap_temp$geometry
eez_simplified <- st_simplify(eez_nowrap, dTolerance = tolerance)
notempty <- which(!is.na(st_dimension(eez_simplified)))
eez_cleaned <- eez_simplified[notempty,]
eez_transformed <- st_transform(eez_cleaned, "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
eez_buffered <- eez_transformed %>% mutate(geometry = st_buffer(geometry, 0))

# land

clipped <- read_sf(dsn = "shapefiles/land_clipped.shp", layer = "land_clipped")
clipped_transformed <- st_transform(clipped, 4326)
clipped_nowrap <- st_as_sf(nowrapSpatialPolygons(as(clipped_transformed, "Spatial")))
clipped_simplified <- st_simplify(clipped_nowrap, dTolerance = 0.1)
notempty <- which(!is.na(st_dimension(clipped_simplified)))
clipped_cleaned <- clipped_simplified[notempty,]

# centroids

centroids_transformed <- st_centroid(eez_transformed)
centroids <- cbind(
  as.data.frame(eez_cleaned) %>% select(GeoName, Territory1, Sovereign1),
  as.data.frame(st_coordinates(centroids_transformed))
)
centroids$label <- centroids$Territory1
centroids$label[centroids$label == "Phoenix Group"] <- "Kiribati"
centroids$label[centroids$label == "Line Group"] <- "Kiribati"
centroids$label[centroids$label == "Gilbert Islands"] <- "Kiribati"
centroids$label[centroids$label == "Oecusse"] <- "East Timor"
placenames <- centroids %>% group_by(label) %>% summarize(longitude = mean(X), latitude = mean(Y))

# plot

ggplot() + 
  geom_sf(data = eez_buffered, lwd = 1, fill = color_eez, color = "#ffffff") +
  geom_sf(data = stations_sf, color = color_stations, size = 0.3) +
  geom_sf(data = clipped_cleaned, lwd = 0, fill = color_land) +
  geom_text_repel(
    data = placenames,
    aes(x = longitude, y = latitude, label = label),
    size = 3.9,
    col = "black", segment.color = NA,
    direction = "y"
  ) +
  coord_sf(
    crs = "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
    xlim = c(-6000000, 4400000),
    ylim = c(-3100000, 2500000)
  ) +
  theme(
    panel.background = element_rect(fill = "#fafafa"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) + xlab("") + ylab("")
  
ggsave(file = "psids.png", width = 12, height = 6)
