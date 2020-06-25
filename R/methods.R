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
  out <- fabletools::features(.tbl$origData(), {{ .var }}, features, ...)
  reconstruct_shared_tsibble(out, .tbl)
}

model.SharedTsibbleData <- function() {
  
}

components.SharedTsibbleData <- function() {
  
}
