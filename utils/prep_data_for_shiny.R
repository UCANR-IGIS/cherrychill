## Prepare data for the Shiny app

## Background: cleancimis is a beast of a package (due to all the dependencies), 
## so I do not want to use it in the Shiny app.

library(cleancimis)
library(sf)

data_dir <- here::here("cherrychill/data"); dir.exists(data_dir)

cimis_stn_sf <- cc_stn_active(overwrite = TRUE)

st_write(cimis_stn_sf |> st_transform(4326) , file.path(data_dir, "cimis_stn_active.geojson"), delete_dsn = TRUE)

# Generate a list of each station’s nearest neighbors:
stn_all_nn_lst <- cc_stn_nn(cimis_sf = cimis_stn_sf, nn = 4)
names(stn_all_nn_lst)

saveRDS(stn_all_nn_lst, file = file.path(data_dir, "stn_all_nn_lst.rds"))

#################################################
### REDUCE THE SIZE OF cherry_cimis_data_2021_25.rds SO IT CAN GO ON GITHUB

weather_data_in_fn <- here::here("cherrychill/data/cherry_cimis_data_2021_25.rds"); file.exists(weather_data_in_fn)
weather_data_tbl <- readRDS(weather_data_in_fn) # 167 MB

weather_data_out_fn <- here::here("cherrychill/data/cherry_cimis_data_2021_25_xz.rds"); file.exists(weather_data_out_fn)
saveRDS(weather_data_tbl, file = weather_data_out_fn, compress = "xz") # 10 MB, but takes 0.75 sec to import

weather_data_out_gzip_fn <- here::here("cherrychill/data/cherry_cimis_data_2021_25.gzip.rds"); file.exists(weather_data_out_gzip_fn)
saveRDS(weather_data_tbl, file = weather_data_out_gzip_fn, compress = "gzip") # 23.5 MB, takes 0.4 sec to import

system.time(xx <- readRDS(weather_data_in_fn))        
system.time(xx <- readRDS(weather_data_out_fn))
system.time(xx <- readRDS(weather_data_out_gzip_fn))  ## 

######################################################
## REDUCE THE SIZE OF gam_T_tree_0.rds

# gam_orig_fn <- here::here("cherrychill/model/gam_T_tree_0.rds"); file.exists(gam_orig_fn)
gam_orig_fn <- "D:/GitHub/cherry_chill/baks_keep/~data_first/gam_T_tree_0.rds"; file.exists(gam_orig_fn)
gam_orig_obj <- readRDS(gam_orig_fn) # 171 MB
class(gam_orig_obj)

gam_gzip_zstd_fn <- here::here("cherrychill/model/gam_T_tree_0.zstd.rds"); file.exists(gam_gzip_zstd_fn)

saveRDS(gam_orig_obj, file = gam_gzip_fn, compress = "gzip")         ## 100 MB
saveRDS(gam_orig_obj, file = gam_gzip_fn, compress = "bzip2")        ## 98 MB
saveRDS(gam_orig_obj, file = gam_gzip_fn, compress = "xz")           ## 77 MB

gam_bzip_fn <- here::here("cherrychill/model/gam_T_tree_0.bzip.rds"); file.exists(gam_bzip_fn)
saveRDS(gam_orig_obj, file = gam_bzip_fn, compress = "bzip2")        ## 98 MB

gam_gzip_zstd_fn <- here::here("cherrychill/model/gam_T_tree_0.zstd.rds"); file.exists(gam_gzip_zstd_fn)
saveRDS(gam_orig_obj, file = gam_gzip_zstd_fn, compress = "zstd")   ## 87 MB, quick   WINNNER
                                                                    ## ShinyApps says: Zstd compression support was not included in this R binary.

gam_xz_fn <- here::here("cherrychill/model/gam_T_tree_0.xz.rds"); file.exists(gam_xz_fn)
saveRDS(gam_orig_obj, file = gam_xz_fn, compress = "xz")            ## 77 MB


system.time(readRDS(gam_orig_fn))       # 0.13
system.time(readRDS(gam_gzip_zstd_fn))  # 0.19
system.time(readRDS(gam_bzip_fn))       # 4.44
system.time(readRDS(gam_xz_fn))         # 2.08


#########################
## CRAETE THE GEOJSON FILES FOR THE MAP

library(tigris)
library(sf)
library(dplyr)
library(fs)

## From cherrychill_utils: I have found the "5m" resolution gives the best balance between precision and file size
## (see sanjoaquin_zones2.R)

sanjoaquin_kern_sf <- tigris::counties(state = "CA", cb = TRUE, resolution = "5m", progress_bar = FALSE) |> 
  filter(COUNTYFP == "077" | COUNTYFP == "029" ) |> 
  select(STATEFP, COUNTYFP, GEOID, NAME, LSAD) |> 
  st_transform(4326)
  
head(sanjoaquin_kern_sf)
names(sanjoaquin_kern_sf)
sanjoaquin_kern_sf
plot(sanjoaquin_kern_sf$geometry)

sanjoaquin_kern_geojson <- fs::path(here::here("cherrychill/data"), "sanjoaquin_kern_bnd.geojson")

st_write(sanjoaquin_kern_sf, dsn = sanjoaquin_kern_geojson)

plot(sanjoaquin_kern_sf$geometry)

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




########################
## Test out running it locally using shiny::runGitHub

library(shiny)
runGitHub("cherrychill", "ucanr-igis", subdir = "cherrychill")




