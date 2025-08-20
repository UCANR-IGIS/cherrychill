# Function to get crop interval for a single date =====
# 'crop interval' is a lubridate interval object for the period for which chill should be computed
crop_interval_of_date <- function(.date_time) {
  if (!isNamespaceLoaded("lubridate")) stop("This function requires that lubridate is loaded")
  if (!inherits(.date_time, c("POSIXct", "POSIXt", "Date"))) {
    stop(".date_times should be a POSIXct oe Date object")
    }
  yr <- year(.date_time)
  endos <- dmy(paste0("01-Nov-", yr))
  if (.date_time < endos) {
    lubridate::interval(
      start = dmy(paste("01-Nov", year(.date_time) - 1, sep = "-")),
      end = dmy(paste("07-Apr", year(.date_time), sep = "-")) 
    )
  } else {
    lubridate::interval(
      start = dmy(paste("01-Nov", year(.date_time), sep = "-")),
      end = dmy(paste("07-Apr", year(.date_time) + 1, sep = "-")) 
    )
  }
}
