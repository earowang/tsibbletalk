#' Dice dates
#'
#' @param x,by Dates.
#'
#' @export
dice_date <- function(x, by) {
  UseMethod("dice_date", x)
}

#' @method dice_date POSIXt
#' @export
dice_date.POSIXt <- function(x, by = NULL) {
  UseMethod("dice_date.POSIXt", by)
}

#' @export
dice_date.POSIXt.default <- function(x, by) {
  abort("Oops!")
}

#' @export
dice_date.POSIXt.POSIXt <- function(x, by) {
  lt <- as.POSIXlt(x)
  by_lt <- as.POSIXlt(by)
  hms::hms(lt$sec, lt$min, lt$hour,
    as.double(as_date(lt)) - as.double(as_date(by_lt)))
}

#' @export
dice_date.POSIXt.Date <- dice_date.POSIXt.POSIXt

dice_date.POSIXt.yearweek <- function(x, by) {
  # TODO: extend for weeks > 1
  lt <- as.POSIXlt(x)
  wday <- lt$wday
  wday[wday == 0] <- 7L
  lt_wk <- lubridate::isoweek(lt)
  by_wk <- lubridate::isoweek(by)
  hms::hms(lt$sec, lt$min, lt$hour,
    (wday - 1) * (lt_wk - by_wk + 1))
}

dice_date.POSIXt.yearmonth <- function(x, by) {
  lt <- as.POSIXlt(x)
  by_lt <- as.POSIXlt(by)
  hms::hms(lt$sec, lt$min, lt$hour, lt$mday - by_lt$mday)
}

dice_date.POSIXt.yearquarter <- dice_date.POSIXt.Date

dice_date.POSIXt.double <- function(x, by) {
  # TODO: extend for years > 1
  lt <- as.POSIXlt(x)
  hms::hms(lt$sec, lt$min, lt$hour, lt$yday - 1)
}
