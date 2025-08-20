# THIS CALCULATES CIMIS AIR AND TREE BASED CHILL FOR 2021-2025 CHERRY AREAS
# PREPARE ENVIRONMENT
# Set up ====
# library(tidyverse)      ## try without this 
# library(mgcv)            
# library(magrittr)
# library(chillR)         ## verify we need both chillR and chillModels
# library(ChillModels)    
# library(data.table)     ## verify we need this
# library(tsibble)
# # library(zoo)
# library(oce)
# # library(missRanger)
# ## library(httr2)                     ## not needed here (used in cc_stn_active_eal())
# library(sf)
# library(FNN)
# library(conflicted)
# conflicts_prefer(lubridate::year)      ## combine these in one statement
# conflicts_prefer(lubridate::month)
# conflicts_prefer(lubridate::hour)
# conflicts_prefer(lubridate::minute)
# conflicts_prefer(lubridate::second)
# conflicts_prefer(lubridate::yday)
# 
# library(readr)   ## needed for read_rds

## The following R files are in 'R' directory and will be automatically sourced when the Shiny
## app loads:
##  - tree_chill.R
##  - crop_year_of_date.R
##  - crop_interval_of_date.R
##  - cc_stn_active_eal.R
##  - cc_stn_nn_eal.R

# here::i_am(
#   "cherry_Rcode/fix_station4web_preview/tree_chill.R"
# )
# library(here)
# 
# # Load crop_year_of_date function ====
# source(
#   here(
#     "cherry_Rcode",
#     "fix_station4web_preview",
#     "crop_year_of_date.R"
#   )
# )
# 
# # Load to get crop interval from any date =====
# source(
#   here(
#     "cherry_Rcode",
#     "fix_station4web_preview",
#     "crop_interval_of_date.R"
#   )
# )

# # Load function to get station metadata =====
# source(
#   here(
#     "cherry_Rcode",
#     "fix_station4web_preview",
#     "cc_stn_active_eal.R"
#   )
# )
# 
# # Load function to find nearest stations =====
# source(
#   here(
#     "cherry_Rcode",
#     "fix_station4web_preview",
#     "cc_stn_nn_eal.R"
#   )
# )

# Read model for prediction
# (If needed for memory management, this could be read in tree_chill() only)
gam_T_tree_0 <- read_rds("model/gam_T_tree_0.xz.rds")

# Get station data already corrected and gap-filled for 37 stations (selected by Emilio for the cherry growing regions)
stn_data <- read_rds("data/cherry_cimis_data_2021_25.gzip.rds")

# ===== Get station metadata =====
## AL: because CIMIS is unstable, I have turned caching back on, using the 'data' directory
## as the cache directory
all_stns_sf <- cc_stn_active_eal(use_cache = TRUE, data_dir = "data")
# dim(all_stns_sf) 270 x 10 (probably returning all stations, not just the active ones)

## Create a subset of just the locations of the stations for which we have gap-filled data
cherry_stns_sf <- all_stns_sf |> 
  filter(stid %in% unique(stn_data$station_n)) |> 
  mutate(station_n = stid)

## Create a utility function to convert orchard coordinates to a sf object
## (This will be used to find the nearest CIMIS stations)
orch_coords2sf <- function(.orch_name, .orch_lon_dd, .orch_lat_dd) {
  tibble(
    orch = .orch_name,
    Lon = .orch_lon_dd,
    Lat = .orch_lat_dd
  ) |> 
    st_as_sf(
      coords = c("Lon","Lat"),
      crs = 4326
    ) %>%
    st_transform(3310)
}

# tree_chill is the main function. It will 
# 1) identify the 3 closest CIMIS stations to the orchard point
# 2) grab the gap-filled CIMIS data for those stations for a single crop year (winter)
# 3) compute some additional variables
# 4) predict bark temperature using the GAM
# 5) compute both bark chill and air chill
# 6) return a tibble

tree_chill <- function(
    .orch_name, 
    .orch_lat_dd, 
    .orch_lon_dd, 
    .date) {
  
  if (!inherits(.orch_name,  "character")) {
    stop(".orch_name should be a string")}
  if (!inherits(.orch_lat_dd,  "numeric")) {
    stop(".orch_lat_dd should be in decimal degrees (32 to 42)")}
  if (!inherits(.orch_lon_dd,  "numeric")) {
    stop(".orch_lon_dd should be in decimal degrees (-124 to -114)")}
  if (is.na(lubridate::dmy(.date))) {
    stop(".date should be in a format that can be parsed by lubridate::dmy()")}
  
  # Date related values ====
  date_in <- dmy(.date)
  crop_year <- crop_year_of_date(date_in)
  crop_intrvl <- crop_interval_of_date(date_in) # cut to 7 Apr if later
  end_year <- year(int_end(crop_intrvl))
  
  # Make orchard sf and identify 3 nearest stations ABC =====
  ## THIS CAN ALL BE MADE INIFINTELY SIMPLER. NO NEED TO USE SF PACKAGE AT ALL
  
  ## I created a separate function for this because it is also needed in app.R
  orch_sf <- orch_coords2sf(.orch_name, .orch_lon_dd, .orch_lat_dd)
  
  # orch_sf <- tibble(
  #   orch = .orch_name,
  #   Lon = .orch_lon_dd,
  #   Lat = .orch_lat_dd
  # ) |> 
  #   st_as_sf(
  #     coords = c("Lon","Lat"),
  #     crs = 4326
  #   ) %>%
  #   st_transform(3310)
  
  orch_stids <- cc_stn_nn_eal(cherry_stns_sf, orch_sf, nn = 3)
  
  ## To Do: use setNames() instead of magrittr::set_names (base R)
  ABC_stns <- orch_stids |> 
    magrittr::set_names(
      LETTERS[1:3]
    )
  
  stn_letter4number <- LETTERS[1:3] |> 
    magrittr::set_names(
      paste0("stn_", ABC_stns)
    )
  
  # browser()
  # When you run 36.6746, -120.4618, get an error:
  # Object 'st_ppt_mm_C' not found
  
  # Get relevant stations-dates and add ppt MA =====
  
  ABC_stn_dat1 <- stn_data |> 
    dplyr::filter(
      # date_time %within% crop_intrvl,     ## this results in a single measurement at midnight on the last day
      date_time >= int_start(crop_intrvl),
      date_time <= int_end(crop_intrvl) + (3600 * 24 - 1), ## need to extend to 11:59pm 
      station_n %in% orch_stids
    ) %>% 
    pivot_wider(
      names_from = station_n,
      names_prefix = "stn_",
      values_from = starts_with("st_")
    ) %>% 
    magrittr::set_names( 
      names(.) %>% 
        str_replace_all(stn_letter4number)
    ) %>% 
    arrange(date_time) %>% 
    mutate(
      st_ppt_ma_A = (dplyr::lag(st_ppt_mm_A) +
                       dplyr::lag(st_ppt_mm_A, 2) +
                       dplyr::lag(st_ppt_mm_A, 3) +
                       dplyr::lag(st_ppt_mm_A, 4)) / 4,
      st_ppt_ma_B = (dplyr::lag(st_ppt_mm_B) +
                       dplyr::lag(st_ppt_mm_B, 2) +
                       dplyr::lag(st_ppt_mm_B, 3) +
                       dplyr::lag(st_ppt_mm_B, 4)) / 4,
      st_ppt_ma_C = (dplyr::lag(st_ppt_mm_C) +
                       dplyr::lag(st_ppt_mm_C, 2) +
                       dplyr::lag(st_ppt_mm_C, 3) +
                       dplyr::lag(st_ppt_mm_C, 4)) / 4,
    )

  # Add other necessary variables for model prediction =====
  ABC_stn_dat2 <- ABC_stn_dat1 %>%
    mutate(
      dec_hour = hour(date_time) +
        minute(date_time) / 60 +
        second(date_time) / 3600,
      date_time4sun = date_time + 8 * 3600,
      orch_sun_azmth = sunAngle(
        t = date_time4sun,
        latitude = .orch_lat_dd,
        longitude = .orch_lon_dd,
        useRefraction = TRUE
      )$azimuth %>% unname(),
      orch_sun_alt = sunAngle(
        t = date_time4sun,
        latitude = .orch_lat_dd,
        longitude = .orch_lon_dd,
        useRefraction = TRUE
      )$altitude %>% unname(),
      orch_sun_alt_day = ifelse(
        orch_sun_alt < 0,
        0,
        orch_sun_alt
      ),
      doy = yday(date_time),
      date = date(date_time),
      st_et_mm_A_lag1 = dplyr::lag(st_et_mm_A, 1),
      st_et_mm_A_lag2 = dplyr::lag(st_et_mm_A, 2),
      st_et_mm_A_lag3 = dplyr::lag(st_et_mm_A, 3),
      st_slr_A_lag1 = dplyr::lag(st_slr_w_m2_A, 1),
      st_slr_A_lag2 = dplyr::lag(st_slr_w_m2_A, 2),
      st_slr_A_lag3 = dplyr::lag(st_slr_w_m2_A, 3),
      st_t_air_A_lag1 = dplyr::lag(st_t_air_C_A, 1),
      st_t_air_A_lag2 = dplyr::lag(st_t_air_C_A, 2),
      st_t_air_A_lag3 = dplyr::lag(st_t_air_C_A, 3),
      st_t_soil_A_lag1 = dplyr::lag(st_t_soil_C_A, 1),
      st_t_soil_A_lag2 = dplyr::lag(st_t_soil_C_A, 2),
      st_t_soil_A_lag3 = dplyr::lag(st_t_soil_C_A, 3),
      st_wind_A_lag1 = dplyr::lag(st_wind_m_s_A, 1),
      st_wind_A_lag2 = dplyr::lag(st_wind_m_s_A, 2),
      st_wind_A_lag3 = dplyr::lag(st_wind_m_s_A, 3)
    )  %>%
    as_tibble() %>% 
    group_by(
      doy
    ) %>% 
    mutate(
      cum_slr_A = cumsum(
        tidyr::replace_na(st_slr_w_m2_A, 0)
      ),
      orch = paste(
        .orch_name,
        "lat",
        .orch_lat_dd,
        "long",
        .orch_lon_dd,
        sep = "_"
      )
    ) %>% 
    ungroup() |> 
    na.omit()
  
  # Add calculated chill for tree and nearest station air temp and return
  suppressWarnings(
    ABC_stn_dat2 %>% mutate(
      pred_avg_t_tree = predict(
        gam_T_tree_0,
        newdata = .,
        newdata.guaranteed = TRUE,
        exclude = c(
          "s(orch,crop_year)"
        )
      ) %>%
        as.numeric()
    ) %>%
      group_by(crop_year) %>% # could be removed
      arrange(
        date_time
      ) %>%
      dplyr::filter(!is.na(pred_avg_t_tree)) |>
      mutate(
        chill_pTtree = dynamic_model(pred_avg_t_tree, total = FALSE),
        chill_st_t_air_A = dynamic_model(st_t_air_C_A, total = FALSE),
        cum_chill_pTtree = cumsum(chill_pTtree),
        cum_chill_st_t_air_A = cumsum(chill_st_t_air_A)
      )
  )
}


