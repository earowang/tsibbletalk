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

#' Coerce to a shared tsibble from tsibble
#'
#' @param x A tsibble.
#' @param spec A formula to specify tsibble key structures. By default, crossing
#' structures (i.e `key1 * key2`) are assumed for the key. The required
#' specification for nesting is `parent / child`.
#'
#' @examples
#' library(tsibble)
#' as_shared_tsibble(tourism, spec = (State / Region) * Purpose)
#' @importFrom tsibble key_vars as_tsibble
#' @importFrom stats terms
#' @importFrom utils head tail
#' @export
as_shared_tsibble <- function(x, spec) {
  spec <- enquo(spec)
  keys <- key_vars(x)
  if (quo_is_missing(spec)) {
    spec <- parse_expr(paste0(keys, collapse = "*"))
  }
  spec <- new_formula(lhs = NULL, rhs = spec, env = empty_env())
  tm <- attr(terms(spec), "factors")
  loc <- vec_group_loc(tm[, ncol(tm)])
  nest_loc <- vec_c(!!!loc$loc[loc$key == 2])
  nest_loc <- vec_c(nest_loc, tail(nest_loc, 1) + 1)
  vars <- rownames(tm)
  nest_vars <- vars[nest_loc]
  cross_vars <- if (has_length(nest_vars)) vars[-nest_loc] else vars
  crosstalk_data <- x
  if (!identical(keys, vars)) {
    crosstalk_data <- as_tsibble(crosstalk_data, key = vars, validate = FALSE)
  }
  # when both nesting and crossing vars are present, use list() for key
  # otherwise character
  SharedTsibbleData$new(
    data = crosstalk_data, nesting = nest_vars, crossing = cross_vars,
    key = ~ parse_key_val(crosstalk_data, nest_vars, cross_vars)
  )
}

#' @importFrom glue glue_data
paste_data <- function(data) {
  msg <- curly_braces(names(data))
  as.character(eval_tidy(quo(
    glue_data(!!data, !!msg, .sep = ":", .envir = !!caller_env())
  )))
}

curly_braces <- function(x) {
  paste0("{", x, "}", collapse = ":")
}

parse_key_val <- function(data, nesting, crossing) {
  if (has_length(nesting) && has_length(crossing)) {
    nest_vals <- paste_data(data[nesting])
    cross_vals <- paste_data(vec_group_loc(data[crossing])$key)
    map(nest_vals, function(x) paste(x, cross_vals, sep = ":"))
  } else {
    paste_data(data[c(nesting, crossing)])
  }
}
