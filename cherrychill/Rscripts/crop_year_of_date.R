# Function to get cherry crop year for any given date ======
# Define crop_year_of_date function ====

crop_year_of_date <- function(.date_time, eos = "01-Nov-") {
  if (!isNamespaceLoaded("lubridate")) stop("This function requires that lubridate is loaded")
  if (!isNamespaceLoaded("stringr")) stop("This function requires that stringr is loaded")
  
  if (!inherits(.date_time, c("POSIXct", "POSIXt", "Date"))) {
    stop(".date_time argument must be a POSIXct, POSIXt or Date object")
  }
  yr <- year(.date_time) |> str_sub(3, 4) |> as.numeric()
  mo <- month(.date_time)
  # dy <- day(.date_time)
  endos <- dmy(paste0(eos, yr))
  crop_year <- ifelse(
    .date_time < endos,
    paste(yr - 1, yr, sep = "_"),
    paste(yr, yr + 1, sep = "_")
  )
  return(crop_year)
}
