reconstruct_shared_tsibble <- function(data, template) {
  SharedTsibbleData$new(data,
    key = parse_key_val(data, key_vars(template$origData())),
    group = template$groupName())
}

features.SharedTsibbleData <- function(.tbl, .var, features, ...) {
  out <- fabletools::features(.tbl$origData(), {{ .var }}, features, ...)
  reconstruct_shared_tsibble(out, .tbl)
}
