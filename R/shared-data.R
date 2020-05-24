SharedTsibbleData <- R6::R6Class(
  classname = "SharedTsibbleData",
  inherit = crosstalk::SharedData,
  public = list(
    print = function(...) {
      print(self$origData(), ...)
    }
  )
)

as_shared_tsibble <- function(x) {
  SharedTsibbleData$new(data = x, key = parse_key_val(x))
}

paste_data <- function(data) {
  vec_c(!!!pmap(data, paste, sep = "&"))
}

parse_key_val <- function(data, key = NULL) {
  if (is_null(key)) {
    key <- tsibble::key_vars(data)
  }
  paste_data(as.list(data)[key])
}
