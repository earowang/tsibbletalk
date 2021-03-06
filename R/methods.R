reconstruct_shared_tsibble <- function(data, template) {
  nesting <- template$nesting()
  crossing <- template$crossing()
  SharedTsibbleData$new(data,
    nesting =  nesting,
    crossing = crossing,
    key = ~ parse_key_val(data, nesting, crossing),
    group = template$groupName())
}

features.SharedTsibbleData <- function(.tbl, .var, features, ...) {
  out <- fabletools::features(as_tsibble(.tbl), {{ .var }}, features, ...)
  reconstruct_shared_tsibble(out, .tbl)
}

model.SharedTsibbleData <- function(.data, ...) {
  out <- fabletools::model(as_tsibble(.data), ...)
  reconstruct_shared_tsibble(out, .data)
}

components.SharedTsibbleData <- function(object, ...) {
  out <- fabletools::components(as_tibble(object), ...)
  reconstruct_shared_tsibble(out, object)
}

forecast.SharedTsibbleData <- function(object, ...) {
  out <- fabletools::forecast(as_tibble(object), ...)
  reconstruct_shared_tsibble(out, object)
}

#' @export
as_tsibble.SharedTsibbleData <- function(x, ...) {
  x$origData()
}

#' @export
as_tibble.SharedTsibbleData <- as_tsibble.SharedTsibbleData

mutate.SharedTsibbleData <- function(.data, ...) {
  out <- dplyr::mutate(as_tibble(.data), ...)
  reconstruct_shared_tsibble(out, .data)
}

filter.SharedTsibbleData <- function(.data, ..., .preserve = FALSE) {
  out <- dplyr::filter(as_tibble(.data), ..., .preserve = .preserve)
  reconstruct_shared_tsibble(out, .data)
}

transmute.SharedTsibbleData <- function(.data, ...) {
  abort("Not supported for `as_shared_tsibble()`.")
}

select.SharedTsibbleData <- transmute.SharedTsibbleData

rename.SharedTsibbleData <- transmute.SharedTsibbleData
