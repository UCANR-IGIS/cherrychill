## julianDay() from oce package
## Source: https://github.com/dankelley/oce/blob/develop/R/astronomy.R

#' Convert a Time to a Julian Day
#'
#' Convert a POSIXt time (given as either the `t` argument
#' or as the `year`, `month`, and other arguments) to a Julian day,
#' using the method provided in
#' Chapter 3 of Meeus (1982).  It should be noted that
#' Meeus and other astronomical treatments use fractional days, whereas the
#' present code follows the R convention of specifying days in whole numbers,
#' with hours, minutes, and seconds also provided as necessary.  Conversion is
#' simple, as illustrated in the example for 1977 April 26.4, for which Meeus
#' calculates julian day 2443259.9.  Note that the R documentation for
#' [julian()] suggests another formula, but the point of the present
#' function is to match the other Meeus formulae, so that suggestion is ignored
#' here.
#'
#' @param t a time, in POSIXt format, e.g. as created by
#' [as.POSIXct()], [as.POSIXlt()], or
#' [numberAsPOSIXct()], or a character string that can be
#' converted to a time using [as.POSIXct()].  If `t` is provided,
#' the other arguments are ignored.
#'
#' @param year year, to be provided along with `month`, etc., if `t`
#' is not provided.
#'
#' @param month numerical value for the month, with January being 1.
#' (This is required if `t` is not provided.)
#'
#' @param day numerical value for day in month, starting at 1.
#' (This is required if `t` is not provided.)
#'
#' @param hour numerical value for hour of day, in range 0 to 24.
#' (This is required if `t` is not provided.)
#'
#' @param min numerical value of the minute of the hour.
#' (This is required if `t` is not provided.)
#'
#' @param sec numerical value for the second of the minute.
#' (This is required if `t` is not provided.)
#'
#' @param tz timezone
#'
#' @return A Julian-Day number, in astronomical convention as explained in
#' Meeus.
#'
#' @author Dan Kelley
#'
#' @references
#' * Meeus, Jean. Astronomical Formulas for Calculators. Second Edition.
#' Richmond, Virginia, USA: Willmann-Bell, 1982.
#'
#' @examples
#' library(oce)
#' # example from Meeus
#' t <- ISOdatetime(1977, 4, 26, hour = 0, min = 0, sec = 0, tz = "UTC") + 0.4 * 86400
#' stopifnot(all.equal(julianDay(t), 2443259.9))
#'
#' @family things related to astronomy
#' @family things related to time
julianDay <- function(t, year = NA, month = NA, day = NA, hour = NA, min = NA, sec = NA, tz = "UTC") {
  if (missing(t)) {
    if (is.na(year) || is.na(month) || is.na(day) || is.na(hour) || is.na(min) || is.na(sec)) {
      stop("must supply year, month, day, hour, min, and sec")
    }
    t <- ISOdatetime(year, month, day, hour, min, sec, tz = tz)
  }
  tt <- as.POSIXlt(t, tz = tz)
  year <- tt$year + 1900
  month <- tt$mon + 1
  day <- tt$mday + (tt$hour + tt$min / 60 + tt$sec / 3600) / 24
  m <- ifelse(month <= 2, month + 12, month)
  y <- ifelse(month <= 2, year - 1, year)
  A <- floor(y / 100)
  B <- 2 - A + floor(A / 4)
  jd <- floor(365.25 * y) + floor(30.6001 * (m + 1)) + day + 1720994.5
  # correct for Gregorian calendar
  jd <- ifelse(tt > ISOdatetime(1582, 10, 15, 0, 0, 0), jd + B, jd)
  jd
}