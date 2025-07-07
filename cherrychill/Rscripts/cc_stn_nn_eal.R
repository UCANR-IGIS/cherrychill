
# Function to retrieve the station ids of the 3 nearest CIMIS stations to an orchard

cc_stn_nn_eal <- function(stn_set_sf, qry_set_sf, nn = 3) {
  if (!isNamespaceLoaded("sf")) stop("This function requires that sf is loaded")
  if (!isNamespaceLoaded("FNN")) stop("This function requires that FNN is loaded")
  
  if (
    !inherits(stn_set_sf, "sf") | !inherits(qry_set_sf, "sf")
    ) {
    stop("stn_set_sf and qry_set_sf must be point sf objects")
  }
  if (!"stid" %in% names(stn_set_sf)) {
    stop("stn_set_sf must have a column called 'stid'")
  }
  
  # conflicted::conflicts_prefer(dplyr::select, .quiet = TRUE)
  
  stn_tbl <- as_tibble(select(st_drop_geometry(stn_set_sf), stid))
  stn_mat <- st_coordinates(stn_set_sf)
  qry_sf <- qry_set_sf
  qry_mat <- st_coordinates(qry_sf)
  
  stn_knn_lst <- FNN::get.knnx(
    data = stn_mat, 
    query = qry_mat,
    k = nn
  )
  
  ref_stns_idx <- as.numeric(stn_knn_lst$nn.index)
  
  stn_set_sf$stid[ref_stns_idx]
  
}
