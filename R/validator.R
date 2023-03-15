
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

#' Convert a dataclass to a data frame validator
#' 
#' If you intend to use your dataclass to validate data frame like object such
#' as tibbles, data frames, or data tables, pass the dataclass into this 
#' function to modify behavior.
#' 
#' @param x A dataclass object
#' @return
#' A function with the following properties:
#'
#' * A modified dataclass function designed to accept data frames
#' * A single argument to test new data frames
#' * Each column in a new data frame will be tested
#' * An error occurs if new data passed to the returned function are invalid
#' * Data is returned if new data passed to the returned function are valid
#' 
#' @examples
#' # Define a dataclass for creating data! Wrap in data_validator():
#' my_df_dataclass <-
#'  data_validator(dataclass(
#'    dte_col = dte_vec(),
#'    chr_col = chr_vec(),
#'    # Custom column validator ensures values are positive!
#'    new_col = function(x) all(x > 0)
#'  ))
#' 
#' # Validate a data frame or data frame like objects!
#' my_df_dataclass(data.frame(
#'  dte_col = as.Date("2022-01-01"),
#'  chr_col = "String!",
#'  new_col = 100
#' ))
#' @export
data_validator <- function(x) {
  
  function(data) {
    
    dc_names <- names(formals(x))
    df_names <- names(data)

    if (!all(names(data) %in% dc_names)) {
      cli::cli_abort(c(
        "Ensure no additional columns are present!",
        "dataclass can only check for these known columns:",
        purrr::set_names(dc_names, "i")
      ))
    }
    
    do.call(x, data) %>%
      tibble::as_tibble()
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
#' elements you define here. If you want to define more customized behavior you
#' can create your own validator functions and insert them as arguments during
#' dataclass creation.
#'
#' @param max_l The maximum length (or row count for data frames) of an object
#' @param min_l The minimum length (or row count for data frames) of an object
#' @return
#' A function with the following properties:
#'
#' * Accepts vector (or data frame in the case of df_like()) to be tested
#' * The returned functions are run when the created dataclass is called
#' * Returned functions each return TRUE or FALSE if new elements are valid
#' 
#' @examples
#' atm_vec(1, 10)   # An atomic vector of any type between 1 and 10 elements
#' dte_vec(1)       # A single date
#' num_vec()        # A numeric vector of any length
#' chr_vec(1)       # A single string
#' lgl_vec(5, 10)   # A logical vector between 5 and 10 elements in length
#' fct_vec(100)     # A factor vector with at most 100 elements
#' df_like(Inf, 50) # A data object with at least 50 rows
#' any_obj()        # Allows any object without validation (can be dangerous!)
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

#' @describeIn any_obj Validate a factor vector
#' @export
fct_vec <- make_validator(is.factor)

#' @describeIn any_obj Validate a data like object
#' @export
df_like <- make_validator(function(x) inherits(x, "data.frame"), nrow)
