date_dice <- function(x, by) {
  UseMethod("date_dice", x)
}

date_dice.POSIXt <- function(x, by = NULL) {
  UseMethod("date_dice.POSIXt", by)
}

date_dice.POSIXt.default <- function(x, by) {
  abort("Oops!")
}

date_dice.POSIXt.POSIXt <- function(x, by) {
  # use lt to avoid DST issue
  lt <- as.POSIXlt(x)
  by_lt <- as.POSIXlt(by)
  lt$sec + lt$min * 60 + lt$hour * 3600 +
    (as.double(as_date(lt)) - as.double(as_date(by_lt))) * 86400
}

date_dice.POSIXt.Date <- date_dice.POSIXt.POSIXt

date_dice.POSIXt.yearweek <- date_dice.POSIXt.POSIXt

date_dice.POSIXt.yearmonth <- date_dice.POSIXt.POSIXt

date_dice.POSIXt.yearquarter <- date_dice.POSIXt.POSIXt

date_dice.POSIXt.double <- function(x, by) {
  # TODO: extend for years > 1
  lt <- as.POSIXlt(x)
  lt$sec + lt$min * 60 + lt$hour * 3600 + (lt$yday - 1) * 86400
}

date_dice.yearquarter <- function(x, by = NULL) {
  UseMethod("date_dice.yearquarter", by)
}

#' @importFrom tsibble yearquarter
date_dice.yearquarter.double <- function(x, by) {
  by_qtr <- yearquarter((by - 1970) * 4)
  x - by_qtr + 1
}

date_floor <- function(x, to, unit = 1) {
  UseMethod("date_floor", x)
}

date_floor.POSIXt <- function(x, to = new_date(), unit = 1) {
  UseMethod("date_floor.POSIXt", to)
}

#' @importFrom lubridate as_date wday
date_floor.POSIXt.Date <- function(x, to = new_date(), unit = 1) {
  x <- as_date(x)
  min_x <- min(x)
  wday_x <- wday(min_x, week_start = 1)
  anchor <- min_x - wday_x + 1 # anchor to Monday
  diff <- as.double(x) - as.double(anchor)
  anchor + floor(diff / unit) * unit
}

date_floor.yearquarter <- function(x, to = double(), unit = 1) {
  UseMethod("date_floor.yearquarter", to)
}

#' @importFrom lubridate quarter year
date_floor.yearquarter.double <- function(x, to = double(), unit = 1) {
  min_x <- min(x)
  qtr_x <- quarter(min_x)
  anchor <- min_x - qtr_x + 1 # anchor to Q1
  diff <- (as.double(x) - as.double(anchor)) / 4
  year(anchor) + floor(diff / unit) * unit
}
