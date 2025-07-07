# library(purrr)
# library(httr2)
# library(dplyr)
# library(conflicted)
# library(sf)
# library(crayon)
# conflicted::conflicts_prefer(dplyr::filter, .quiet = TRUE)

# Modification of Andy's functions such that active interval is created
# NOTE: should we worry about time zones tz = ?

cc_stn_active_eal <- function(
    crs = 3310, 
    use_cache = TRUE, 
    #data_dir = Sys.getenv("CC_DATADIR"),    ## can't use environment variables on shinyapps.io
    data_dir = tempdir(),
    overwrite = FALSE) {
  
  download_stations <- TRUE
  
  if (use_cache) {
    if (!dir.exists(data_dir)) {
      stop(paste0("Can't find data directory"))
    }
    stn_geojson <- file.path(data_dir, "cimis_stn.geojson")
    if (file.exists(stn_geojson) && !overwrite) {
      cat(crayon::silver("   - loading stored copy of cimis stations \n"))
      stn_sf <- sf::st_read(stn_geojson, quiet = TRUE)
      download_stations <- FALSE
    }
  }
  
  if (download_stations) {
    message(crayon::green(" - downloading active cimis stations"))
    stn_resp <- req_perform(request("https://et.water.ca.gov/api/station"))
    if (resp_status(stn_resp) != 200) {
      stop("CIMIS API error")
    }
    stn_lst <- resp_body_json(stn_resp)
    
    stn_tbl <- select(
      mutate(
        tibble(
          stid = as.integer(purrr::map_chr(
            stn_lst$Stations,
            "StationNbr")), 
          name = purrr::map_chr(
            stn_lst$Stations,
            "Name"), 
          city = purrr::map_chr(
            stn_lst$Stations,
            "City"), 
          county = purrr::map_chr(
            stn_lst$Stations,
            "County"), 
          hmslon_chr = purrr::map_chr(
            stn_lst$Stations,
            "HmsLongitude"), 
          hmslat_chr = purrr::map_chr(
            stn_lst$Stations,
            "HmsLatitude"), 
          elev_ft = as.integer(purrr::map_chr(
            stn_lst$Stations,
            "Elevation")), 
          is_active_chr = purrr::map_chr(
            stn_lst$Stations,
            "IsActive"),
          connected_chr = purrr::map_chr(
            stn_lst$Stations,
            "ConnectDate"),
          disconnected_chr = purrr::map_chr(
            stn_lst$Stations,
            "DisconnectDate"),
        ), 
        connect_date = lubridate::mdy(connected_chr),
        disconnect_date = lubridate::mdy(disconnected_chr),
        stn_intvl = lubridate::interval(connect_date, disconnect_date),
        active = (is_active_chr == "True"),
        lon = as.numeric(stringr::str_extract(
          hmslon_chr,
          "(?<=/ ).*")), 
        lat = as.numeric(stringr::str_extract(
          hmslat_chr,
          "(?<=/ ).*"))
      ), 
      -hmslon_chr, 
      -hmslat_chr, 
      -is_active_chr, 
      -connected_chr, 
      -disconnected_chr
    )
    
    stn_sf <- st_transform(
      st_as_sf(
        stn_tbl, 
        coords = c("lon","lat"), 
        crs = 4326
      ), 
      crs = crs
    )
    
    if (use_cache) {
      if (!file.exists(stn_geojson) || overwrite) {
        st_write(
          stn_sf,
          dsn = stn_geojson, 
          delete_dsn = TRUE,
          quiet = TRUE
        )
      }
    }
  }
  
  stn_sf
}
