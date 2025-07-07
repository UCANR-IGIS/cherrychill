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


