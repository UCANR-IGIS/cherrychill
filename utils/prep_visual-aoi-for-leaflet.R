## Prepare map data for the Shiny app

#########################
## CREATE COUNTY BOUNDARY (VISUAL AOI) GEOJSON FILE FOR THE LEAFLET MAP

library(tigris)
library(sf)
library(dplyr)
library(fs)

## From cherrychill_utils.r: I have found the "5m" resolution gives the best balance between precision and file size
## (see sanjoaquin_zones2.R)

sanjoaquin_kern_sf <- tigris::counties(state = "CA", cb = TRUE, resolution = "5m", progress_bar = FALSE) |> 
  filter(COUNTYFP == "077" | COUNTYFP == "029" ) |> 
  select(STATEFP, COUNTYFP, GEOID, NAME, LSAD) |> 
  st_transform(4326)
  
head(sanjoaquin_kern_sf)
names(sanjoaquin_kern_sf)
sanjoaquin_kern_sf
plot(sanjoaquin_kern_sf$geometry)

## SAVE TO DISK

data_dir <- here::here("cherrychill/data"); dir.exists(data_dir)
sanjoaquin_kern_geojson <- fs::path(data_dir, "sanjoaquin_kern_bnd.geojson")
st_write(sanjoaquin_kern_sf, dsn = sanjoaquin_kern_geojson)

########################################################################
## Do some tests with leaflet (which wasn't showing up in the map)
library(leaflet)

leaflet() %>% addTiles()

leaflet(data = sanjoaquin_kern_sf) |> 
  addPolygons()

leaflet(data = sanjoaquin_kern_sf) |> 
  addTiles(group = "Open Street Map") |> 
  addPolygons()

leaflet(data = sanjoaquin_kern_sf) |> 
  addProviderTiles("Esri.WorldImagery", group = "Satellite") |> 
  addPolygons()

leaflet(data = sanjoaquin_kern_sf) |> 
  addTiles(group = "Open Street Map") |> 
  addProviderTiles("Esri.WorldImagery", group = "Satellite") |> 
  addLayersControl(baseGroups = c("Open Street Map", "Satellite"),
                   options = layersControlOptions(collapsed = FALSE)) |>  
  addPolygons()

leaflet(data = sanjoaquin_kern_sf) |> 
  addTiles(group = "Open Street Map") |> 
  addProviderTiles("Esri.WorldImagery", group = "Satellite") |> 
  addLayersControl(baseGroups = c("Open Street Map", "Satellite"),
                   options = layersControlOptions(collapsed = FALSE)) |>  
  addPolygons(fillOpacity = 0)

## From app.R:
# leaflet(visual_aoi_bnd_sf, options = leafletOptions(minZoom = 9, maxZoom = 18)) |>
#   addTiles(group = "Open Street Map") |> 
#   addProviderTiles("Esri.WorldImagery", group = "Satellite") |> 
#   addLayersControl(baseGroups = c("Open Street Map", "Satellite"),
#                    options = layersControlOptions(collapsed = FALSE)) |>  
#   addPolygons(fillOpacity = 0)

################################
## PREPARE THE CHERRY GROWING REGIONS POLYGONS FROM EMILIO (sent as KMLs on 7/15/2025)
kml_dir <- here::here("../from_emilio/polygons_cherrygrowingregions") |> fs::path_abs(); dir.exists(kml_dir)

cherry_region_north_kml <- fs::path(kml_dir, "NorthCherryRegion.kml")
cherry_region_north_sf <- st_read(cherry_region_north_kml) |> 
  transmute(name = "North cherry growing region")

cherry_region_south_kml <- fs::path(kml_dir, "SouthCherryRegion.kml")
cherry_region_south_sf <- st_read(cherry_region_south_kml) |> 
  transmute(name = "South cherry growing region")

cherry_region_north_sf; cherry_region_south_sf

cherry_regions_sf <- cherry_region_north_sf |> 
  bind_rows(cherry_region_south_sf) |> 
  st_zm()   ## get rid of z-values

# View in leaflet
leaflet(data = cherry_regions_sf) |> 
  addTiles(group = "Open Street Map") |> 
  addProviderTiles("Esri.WorldImagery", group = "Satellite") |> 
  addLayersControl(baseGroups = c("Open Street Map", "Satellite"),
                   options = layersControlOptions(collapsed = FALSE)) |>  
  addPolygons(fillOpacity = 0)

# Save to disk
data_dir <- here::here("cherrychill/data"); dir.exists(data_dir)
cherry_regions_geojson <- fs::path(data_dir, "cherry_regions_bnd.geojson")
st_write(cherry_regions_sf, dsn = cherry_regions_geojson)

#####################################
## Save the stations that have gap filled data as a geojson
here::here()
source(here::here("cherrychill/Rscripts/cc_stn_active_eal.R"))

all_stns_sf <- cc_stn_active_eal(use_cache = TRUE, 
                                 data_dir = here::here("cherrychill/data"))
               
                  
# dim(all_stns_sf) 270 x 10 (probably returning all stations, not just the active ones)
stn_data <- read_rds(here::here("cherrychill/data/cherry_cimis_data_2021_25.gzip.rds"))

## Create a subset of just the locations of the stations for which we have gap-filled data
cherry_stns_4leaf_sf <- all_stns_sf |> 
  filter(stid %in% unique(stn_data$station_n)) |> 
  mutate(station_n = stid) |> 
  select(stid, name, city) |> 
  st_transform(4326)

dim(cherry_stns_4leaf_sf)
head(cherry_stns_4leaf_sf)

# View in leaflet
leaflet(data = cherry_regions_sf) |> 
  addTiles(group = "Open Street Map") |> 
  addProviderTiles("Esri.WorldImagery", group = "Satellite") |> 
  addLayersControl(baseGroups = c("Open Street Map", "Satellite"),
                   options = layersControlOptions(collapsed = FALSE)) |>  
  addPolygons(fillOpacity = 0) |> 
  addCircles(data = cherry_stns_4leaf_sf |> select(stid, name), 
             stroke = TRUE, color = "#f00", opacity = 1, weight = 4,
             fill = TRUE, fillOpacity = 1, popup = ~paste0(stid, ": ", name))

# Save to disk
data_dir <- here::here("cherrychill/data"); dir.exists(data_dir)
cherry_cimis_geojson <- fs::path(data_dir, "cherry_cimis.geojson")
st_write(cherry_stns_4leaf_sf, dsn = cherry_cimis_geojson)

## Get max bounds for the USA
ca_bnd_sf <- tigris::states(cb = TRUE) |> 
  filter(GEOID == "06") |> 
  st_transform(4326)

ca_bnd_sf$geometry |> plot()
st_bbox(ca_bnd_sf)

