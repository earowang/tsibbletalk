#' Dice dates
#'
#' @param x,by Dates.
#'
#' @export
date_dice <- function(x, by) {
  UseMethod("date_dice", x)
}

#' @method date_dice POSIXt
#' @export
date_dice.POSIXt <- function(x, by = NULL) {
  UseMethod("date_dice.POSIXt", by)
}

#' @export
date_dice.POSIXt.default <- function(x, by) {
  abort("Oops!")
}

#' @export
date_dice.POSIXt.POSIXt <- function(x, by) {
  lt <- as.POSIXlt(x)
  by_lt <- as.POSIXlt(by)
  lt$sec + lt$min * 60 + lt$hour * 3600 +
    (as.double(as_date(lt)) - as.double(as_date(by_lt))) * 86400
}

#' @export
date_dice.POSIXt.Date <- date_dice.POSIXt.POSIXt

#' @export
date_dice.POSIXt.yearweek <- date_dice.POSIXt.POSIXt

#' @export
date_dice.POSIXt.yearmonth <- date_dice.POSIXt.POSIXt

#' @export
date_dice.POSIXt.yearquarter <- date_dice.POSIXt.POSIXt

date_dice.POSIXt.double <- function(x, by) {
  # TODO: extend for years > 1
  lt <- as.POSIXlt(x)
  lt$sec + lt$min * 60 + lt$hour * 3600 + (lt$yday - 1) * 86400
}

#' Floor dates
#'
#' @param x,to Dates.
#'
#' @export
date_floor <- function(x, to, unit = 1) {
  UseMethod("date_floor", x)
}

#' @method date_floor POSIXt
#' @export
date_floor.POSIXt <- function(x, to = new_date(), unit = 1) {
  UseMethod("date_floor.POSIXt", to)
}

#' @importFrom lubridate as_date wday period
#' @export
date_floor.POSIXt.Date <- function(x, to = new_date(), unit = 1) {
  x <- as_date(x)
  min_x <- min(x)
  wday_x <- wday(min_x, week_start = 1)
  anchor <- min_x - wday_x + 1 # anchor to Monday
  diff <- as.double(x) - as.double(anchor)
  anchor + floor(diff / unit) * unit
}

#' @importFrom dplyr mutate as_tibble
dice_tsibble <- function(data, period) {
  stopifnot(is_tsibble(data))
  idx <- tsibble::index(data)
  period <- period(period)
  if (period$day != 0) {
    to <- new_date()
    unit <- period$day
    scale <- 3600
  }
  mutate(as_tibble(data),
    ".GROUP" := date_floor(!!idx, to = new_date(), unit = unit),
    !!idx := date_dice(!!idx, .GROUP) / scale,
    ".GROUP" := as.factor(.GROUP)
  )
}
