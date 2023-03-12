
make_validator <- function(validator, l_fun = length) {
  # Generates validator functions

  function(max_l = Inf, min_l = 1) {
    function(x) {
      typ <- validator(x)
      low <- l_fun(x) <= max_l
      hi  <- l_fun(x) >= min_l
      typ && low && hi
    }
  }
}

#' Validate dataclass inputs
#'
#' These are utility functions which can be used to validate dataclass inputs.
#' All of these functions follow the same basic structure. If no arguments are
#' provided they will be passed as basic type validators. However, you can
#' specify a max and min length (or rowcount in the case of df_like()). We put
#' max as the first argument because we are typically concerned with the max
#' amount of values more than the minimum. For example, dte_vec(1) ensures a
#' date vector of length one is returned (in other words a single date).
#'
#' This function will return a new function with named argument for each of the
#' elements you define here. If you
#'
#' @examples
#' atm_vec(1, 10)   # An atomic vector of any type between 1 and 10 elements
#' dte_vec(1)       # A single date
#' num_vec()        # A numeric vector of any length
#' chr_vec(1)       # A single string
#' lgl_vec(5, 10)   # A logical vector between 5 and 10 elements in length
#' df_like(Inf, 50) # A data object with at least 50 rows
#' any_obj()        # Allows any object type without validation
#' @export
any_obj <- function() function(x) TRUE

#' @describeIn any_obj Validate a vector
#' @export
atm_vec <- make_validator(rlang::is_bare_atomic)

#' @describeIn any_obj Validate a date vector
#' @export
dte_vec <- make_validator(function(x) {
  inherits(x, "Date") || inherits(x, "POSIXct")
})

#' @describeIn any_obj Validate a numeric vector
#' @export
num_vec <- make_validator(rlang::is_bare_numeric)

#' @describeIn any_obj Validate a character vector
#' @export
chr_vec <- make_validator(rlang::is_bare_character)

#' @describeIn any_obj Validate a logical vector
#' @export
lgl_vec <- make_validator(rlang::is_bare_logical)

#' @describeIn any_obj Validate a data like object
#' @export
df_like <- make_validator(function(x) inherits(x, "data.frame"), nrow)
