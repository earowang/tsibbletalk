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

#' @importFrom lubridate make_date
date_dice.POSIXt.numeric <- function(x, by) {
  date_dice.POSIXt.POSIXt(x, make_date(by))
}

date_dice.yearquarter <- function(x, by = NULL) {
  UseMethod("date_dice.yearquarter", by)
}

#' @importFrom tsibble yearquarter
date_dice.yearquarter.numeric <- function(x, by) {
  by_qtr <- yearquarter((by - 1970) * 4)
  x - by_qtr + 1
}

date_dice.yearmonth <- function(x, by = NULL) {
  UseMethod("date_dice.yearmonth", by)
}

#' @importFrom tsibble yearmonth
date_dice.yearmonth.numeric <- function(x, by) {
  by_mth <- yearmonth((by - 1970) * 12)
  x - by_mth + 1
}

date_dice.numeric <- function(x, by = NULL) {
  UseMethod("date_dice.numeric", by)
}

date_dice.numeric.numeric <- function(x, by) {
  x - by + 1
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

#' @importFrom tsibble yearweek
date_floor.POSIXt.yearweek <- function(x, to = yearweek(), unit = 1) {
  x <- yearweek(x)
  min_x <- min(x)
  diff <- as.double(x) - as.double(min_x)
  min_x + floor(diff / unit) * unit
}

#' @importFrom tsibble yearmonth
date_floor.POSIXt.yearmonth <- function(x, to = yearmonth(), unit = 1) {
  x <- yearmonth(x)
  min_x <- min(x)
  diff <- as.double(x) - as.double(min_x)
  min_x + floor(diff / unit) * unit
}

#' @importFrom lubridate floor_date
date_floor.POSIXt.numeric <- function(x, to = numeric(), unit = 1) {
  x <- floor_date(x, unit = paste(unit, "year"))
  min_x <- min(x)
  diff <- as.double(x) - as.double(min_x)
  min_x + floor(diff / unit) * unit
}

date_floor.yearquarter <- function(x, to = double(), unit = 1) {
  UseMethod("date_floor.yearquarter", to)
}

#' @importFrom lubridate quarter year
date_floor.yearquarter.numeric <- function(x, to = double(), unit = 1) {
  min_x <- min(x)
  qtr_x <- quarter(min_x)
  anchor <- min_x - qtr_x + 1 # anchor to Q1
  diff <- (as.double(x) - as.double(anchor)) / 4
  year(anchor) + floor(diff / unit) * unit
}

date_floor.yearmonth <- function(x, to = double(), unit = 1) {
  UseMethod("date_floor.yearmonth", to)
}

#' @importFrom lubridate month
date_floor.yearmonth.numeric <- function(x, to = double(), unit = 1) {
  min_x <- min(x)
  mth_x <- month(min_x)
  anchor <- min_x - mth_x + 1 # anchor to Jan
  diff <- (as.double(x) - as.double(anchor)) / 12
  year(anchor) + floor(diff / unit) * unit
}

date_floor.numeric <- function(x, to = double(), unit = 1) {
  UseMethod("date_floor.numeric", to)
}

date_floor.numeric.numeric <- function(x, to = double(), unit = 1) {
  anchor <- min(x)
  anchor + floor((x - anchor) / unit) * unit
}
