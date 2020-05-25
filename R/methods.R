reconstruct_shared_tsibble <- function(data, template) {
  SharedTsibbleData$new(data,
    nesting = template$nesting(),
    crossing = template$crossing(),
    key = ~ parse_key_val(data, key = key_vars(template$origData()),
      list_out = template$isKeyList()),
    group = template$groupName())
}

features.SharedTsibbleData <- function(.tbl, .var, features, ...) {
  out <- fabletools::features(.tbl$origData(), {{ .var }}, features, ...)
  reconstruct_shared_tsibble(out, .tbl)
}
