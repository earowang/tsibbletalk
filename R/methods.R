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

model.SharedTsibbleData <- function() {
  
}

components.SharedTsibbleData <- function() {
  
}

#' @export
as_tsibble.SharedTsibbleData <- function(x, ...) {
  x$origData()
}

#' @export
as_tibble.SharedTsibbleData <- function(x, ...) {
  x$origData()
}
