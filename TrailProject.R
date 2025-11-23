#Strava Heatmap Analysis Project Evan Pelerine
library(tidyverse)
library(janitor)
library(ggplot2)
library(tigris)
library(sf)
library(dplyr)
library(terra)
library(mapview)
library(stars)
library(raster)

## sets GPX data directory
setwd('/Users/evanpelerine/Desktop/Onyx GPX Files')
mydir = '/Users/evanpelerine/Desktop/Onyx GPX Files'
myfiles = list.files(path=mydir, pattern="\\.gpx", full.names=TRUE)
myfiles

## reads in Onyx GPX files I pulled form the MTB Project (https://www.mtbproject.com)
data <- data.frame()
for (file in myfiles){
  addition <- st_read(file, layer="tracks") %>% dplyr::select(name, geometry)
  data <- data %>% rbind(addition)
}

## reads in WA road data from WADR
road_wa <- st_read("/Users/evanpelerine/Desktop/WADNR_Active_Roads/WADNR_Active_Roads.shp")

## buffers road data-from WA state to North Bend Raging River area
buffer <- data %>% st_buffer(4000) %>%
  summarise(geometry = st_union(geometry))

mapview(buffer)

road_buffered = road_wa %>% st_intersection(st_transform(buffer, st_crs(road_wa))) %>% 
  mutate(name="road") %>%
  dplyr::select(name, geometry) 

tdata <- st_transform(data, st_crs(road_wa))

## cleaning
legalproxy <- rbind(tdata, road_buffered) %>%
  filter(name != 'Raging River Loop')

mapview(road_buffered)

mapview(legalproxy)

mapview(tdata)

## reads in strava heatmap image (from strava.com) as a raster
imgrast <- rast("/Users/evanpelerine/Desktop/R-Folder/LegalProject/HMRLO.png")

## takes GIS output of the orignal strava heatmap raster, which is now a geo-referenced CD shapefile w/ a CRS 
strava_heatmap <- st_read("/Users/evanpelerine/Desktop/R-Folder/LegalProject/strava_heatmap/strava_heatmap.shp")

cleaned_strava_no_density <- strava_heatmap %>% filter(gridcode != 0) %>%
  summarise(geometry = st_union(geometry))

cleaned_strava_with_density <- strava_heatmap %>% filter(gridcode != 0)


## analysis 
buffered_legal <- legalproxy %>% st_buffer(100)

buffered_legal_union <- buffered_legal %>%
  st_union() %>%
  st_make_valid() %>%
  st_as_sf()

mapview(cleaned_strava_no_density, col.regions = "orangered") + mapview(legalproxy)

mapview(cleaned_strava_no_density) + mapview(buffered_legal)

mapview(cleaned_strava_no_density) + mapview(buffered_legal_union)

intersection <- st_intersection(cleaned_strava_no_density, buffered_legal_union)

disintersection <- st_difference(cleaned_strava_no_density, buffered_legal_union)

# Extract coordinates as simple features

# Compute the area of each geometry
area_strava <- st_area(cleaned_strava_no_density) %>% sum()
area_intersection <- st_area(intersection) %>% sum()

print(area_strava)

print(area_intersection)

mapview(intersection)

mapview(disintersection)

overlap <- (area_intersection/area_strava)

print(overlap)

target_shape <- legalproxy

st_crs(target_shape) 

plot(imgrast)

mapview(imgrast)


### exports for ARCGISPRO
export_dir <- "/Users/evanpelerine/Desktop"

st_write(tdata, file.path(export_dir, "tdata.shp"), delete_layer = TRUE)

writeRaster(imgrast, 
            filename = file.path(export_dir, "strava_overlay.tif"),
            overwrite = TRUE)


# Define export path
export_dir <- "/Users/evanpelerine/Desktop/Landing"
export_path <- file.path(export_dir, "legalproxy.shp")

# Export shapefile
st_write(legalproxy, export_path, delete_dsn = TRUE)
