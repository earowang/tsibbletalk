SharedTsibbleData <- R6::R6Class(
  classname = "SharedTsibbleData",
  inherit = crosstalk::SharedData,
  private = list(
    .nesting = character(),
    .crossing = character()
  ),

  public = list(
    initialize = function(data, nesting, crossing, key = NULL, group = NULL) {
      if (is_null(group)) {
        rand <- as.hexmode(sample(256, 4, replace = TRUE) - 1)
        group <- paste0("SharedTsibbleData",
          paste(format(rand, width = 2), collapse = ""))
      }
      super$initialize(
        data, key = key,
        group = group
      )
      private$.nesting <- nesting
      private$.crossing <- crossing
    },

    isKeyList = function() {
      has_length(private$.nesting) && has_length(private$.crossing)
    },

    nesting = function() {
      private$.nesting
    },

    crossing = function() {
      private$.crossing
    },

    print = function(...) {
      print(self$origData(), ...)
    }
  )
)

as_shared_tsibble <- function(x, spec) {
  spec <- enquo(spec)
  if (quo_is_missing(spec)) {
    spec <- parse_expr(paste0(key_vars(x), collapse = "*"))
  }
  spec <- new_formula(lhs = NULL, rhs = spec, env = empty_env())
  tm <- attr(terms(spec), "factors")
  loc <- vec_group_loc(tm[, ncol(tm)])
  nest_loc <- vec_c(!!!loc$loc[loc$key == 2])
  nest_loc <- vec_c(nest_loc, tail(nest_loc, 1) + 1)
  vars <- rownames(tm)
  nest_vars <- vars[nest_loc]
  cross_vars <- vars[-nest_loc]
  is_key_list <- has_length(nest_vars) && has_length(cross_vars)
  x <- tsibble::as_tsibble(x, key = vars, validate = FALSE)
  # when both nesting and crossing present, use list() for key
  # otherwise character
  SharedTsibbleData$new(
    data = x, nesting = nest_vars, crossing = cross_vars,
    key = ~ parse_key_val(x, list_out = is_key_list)
  )
}

paste_data <- function(data, list_out = FALSE) {
  out <- pmap(data, paste, sep = ":")
  if (list_out) return(out)
  vec_c(!!!out)
}

parse_key_val <- function(data, key = NULL, list_out = FALSE) {
  if (is_null(key)) {
    key <- tsibble::key_vars(data)
  }
  paste_data(as.list(data)[key], list_out = list_out)
}
