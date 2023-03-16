
validator_input_check <- function(level, ...) {
  
  # Check if level is error or warning
  if (!(level %in% c("error", "warn"))) {
    cli::cli_abort(c(
      'Level must be "error" or "warn"!',
      "i" = '`level = "error"` will halt if element is invalid',
      "i" = '`level = "warn"` will throw a warning, but not halt'
    ))
  }
  
  # Check if dimension arguments are valid
  dim_args <-
    list(...) %>%
    purrr::map_lgl(function(x) {
      !rlang::is_atomic(x) || length(x) > 1 
    })
  
  # Identify problematic inputs
  prob_args <- names(dim_args[dim_args])
  
  if (length(prob_args) >= 1) {
    cli::cli_abort(c(
      "These elements are not single length atomic limits:",
      purrr::set_names(prob_args, "x")
    ))
  }
}

#' Convert a dataclass to a data frame validator
#' 
#' If you intend to use your dataclass to validate data frame like object such
#' as tibbles, data frames, or data tables, pass the dataclass into this 
#' function to modify behavior.
#' 
#' @param x A dataclass object
#' @param strict_cols Should additional columns be allowed in the output?
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
#' # Define a dataclass for creating data! Pass to data_validator():
#' my_df_dataclass <-
#'  dataclass(
#'    dte_col = dte_vec(),
#'    chr_col = chr_vec(),
#'    # Custom column validator ensures values are positive!
#'    new_col = function(x) all(x > 0)
#'  ) |>
#'  data_validator()
#' 
#' # Validate a data frame or data frame like objects!
#' data.frame(
#'  dte_col = as.Date("2022-01-01"),
#'  chr_col = "String!",
#'  new_col = 100
#' ) |>
#'   my_df_dataclass()
#' 
#' # Allow additional columns in output
#' test_df_class <-
#'   dataclass(
#'     dte_col = dte_vec()
#'   ) |>
#'   data_validator(strict_cols = FALSE)
#'  
#' tibble::tibble(
#'   dte_col = as.Date("2022-01-01"),
#'   other_col = "a"
#' ) |>
#'   test_df_class()
#' @export
data_validator <- function(x, strict_cols = TRUE) {
  
  function(data) {
    
    dc_names <- names(formals(x))
    df_names <- names(data)
    
    # Checks if columns defined in dataclass are in data
    if (!all(dc_names %in% df_names)) {
      cli::cli_abort(c(
        "Input data is missing these columns:",
        purrr::set_names(setdiff(dc_names, df_names), "i")
      ))
    }
    
    # Checks for bloat columns if strict_cols = TRUE
    if (strict_cols) {
      if (!all(df_names %in% dc_names)) {
        cli::cli_abort(c(
          "Ensure no additional columns are present!",
          "dataclass can only check for these known columns:",
          purrr::set_names(dc_names, "i"),
          "i" = "Set data_validator(strict_cols = FALSE)` to bypass this check."
        ))
      }
    }
    
    # Main columns to check
    subset_df <-
      data %>%
      dplyr::select(
        dplyr::all_of(dc_names)
      )
    
    # Additional columns to check
    extra_df <-
      data %>%
      dplyr::select(
        - dplyr::all_of(dc_names)
      )
    
    # Call dataclass and reassemble data
    do.call(x, subset_df) %>%
      tibble::as_tibble() %>%
      dplyr::bind_cols(extra_df) %>%
      dplyr::select(
        dplyr::all_of(df_names)
      )
  }
}

#' Validate atomic vectors with dataclass
#'
#' These are utility functions which can be used to validate dataclass inputs.
#' All of these functions follow the same basic structure. If no arguments are
#' provided they will be passed as basic type validators. However, you can
#' specify a max and min length. We put max as the first argument because we are
#' typically concerned with the max amount of values more than the minimum. For
#' example, dte_vec(1) ensures a date vector of length one is returned (in other
#' words a single date). Alternatively, dte_vec(10, 5) allows between 5 and 10
#' elements in the vector.
#'
#' You can also specify max and min values in addition to a vector of allowable
#' values. Finally, you can set the level of the validator. Setting
#' `level = "warn"` will warn you if a violation occurs whereas
#' `level = "error"` will halt upon violation.
#'
#' This function will return a new function with named argument for each of the
#' elements you define here. If you want to define more customized behavior you
#' can create your own validator functions and insert them as arguments during
#' dataclass creation.
#'
#' @param max_len The maximum length of a vector
#' @param min_len The minimum length of a vector
#' @param max_val The maximum value of a vector
#' @param min_val The minimum value of a vector
#' @param max_dte The maximum date of a vector
#' @param min_dte The minimum date of a vector
#' @param max_row The maximum row count of a data frame
#' @param min_row The minimum row count of a data frame
#' @param allowed Allowable values for a vector
#' @param level   "warn" print a warning while "error" will halt upon violation
#' @return
#' A function with the following properties:
#'
#' * Accepts vector (or data frame in the case of df_like()) to be tested
#' * The returned functions are run when the created dataclass is called
#' * Returned functions each return TRUE or FALSE if new elements are valid
#' 
#' @examples
#' dte_vec(1) # A single date
#' num_vec()  # A numeric vector of any length
#' chr_vec(1) # A single string
#' lgl_vec(5) # A logical vector with at most 5 elements
#' atm_vec(4, 1) # An atomic vector of any type between 1 and 4 elements
#' num_vec(min_val = 0) # A numeric vector where all elements are positive
#' chr_vec(allowed = c("a", "b")) # Character vector only allowing "a" and "b"
#' dte_vec(level = "warn") # Date vector which warns if non-date is provided
#' any_obj() # Allows any object without validation (can be dangerous!)
#' df_like() # Tests if something is data frame like
#' @export
any_obj <- function() function(x) TRUE

#' @describeIn any_obj Validate a vector
#' @export
atm_vec <- function(max_len = Inf, min_len = 1, level = "error") {
  
  # Check validator inputs
  validator_input_check(level, max_len, min_len)
  
  function(x) {
    
    # Early return for non vectors
    if (!rlang::is_atomic(x)) {
      return(list(result = FALSE, level = level))
    }
    
    list(
      result = all(c(
        length(x) <= max_len,
        length(x) >= min_len
      )),
      level = level
    )
  }
}

#' @describeIn any_obj Validate a date vector
#' @export
dte_vec <- function(max_len = Inf, min_len = 1, level = "error") {
  
  # Check validator inputs
  validator_input_check(level, max_len, min_len)
  
  function(x) {
    
    # Early return for non vectors
    if (!(inherits(x, "Date") || inherits(x, "POSIXct"))) {
      return(list(result = FALSE, level = level))
    }
    
    list(
      result = all(c(
        length(x) <= max_len,
        length(x) >= min_len
      )),
      level = level
    )
  }
}

#' @describeIn any_obj Validate a numeric vector
#' @export
num_vec <- function(
    max_len = Inf,
    min_len = 1,
    max_val = Inf,
    min_val = -Inf,
    allowed = NA,
    level = "error"
) {
  
  # Check validator inputs
  validator_input_check(
    level, max_len, min_len,
    max_val, min_val
  )
  
  if (!(rlang::is_bare_numeric(allowed) || all(is.na(allowed)))) {
    cli::cli_abort(c(
      "x" = "`allowed` must be an numeric vector of allowable values",
      "i" = 'You can also set `allowed` to NA to allow any values!'
    ))
  }
  
  function(x) {
    
    # Early return for non vectors
    if (!rlang::is_bare_numeric(x)) {
      return(list(result = FALSE, level = level))
    }
    
    result <-
      all(c(
        length(x) <= max_len,
        length(x) >= min_len,
        x <= max_val,
        x >= min_val
      ))
    
    if (!all(is.na(allowed))) {
      result <- result && all(x %in% allowed)
    }
    
    list(
      result = result,
      level = level
    )
  }
}

#' @describeIn any_obj Validate a character vector
#' @export
chr_vec <- function(
    max_len = Inf,
    min_len = 1,
    allowed = NA,
    level = "error"
) {
  
  # Check validator inputs
  validator_input_check(level, max_len, min_len)
  
  # Validate allowed
  if (!(rlang::is_bare_character(allowed) || all(is.na(allowed)))) {
    cli::cli_abort(c(
      "x" = "`allowed` must be an character vector of allowable values",
      "i" = 'You can also set `allowed` to NA to allow any values!'
    ))
  }
  
  function(x) {
    
    # Early return for non vectors
    if (!rlang::is_bare_character(x)) {
      return(list(result = FALSE, level = level))
    }
    
    result <-
      all(c(
        length(x) <= max_len,
        length(x) >= min_len
      ))
    
    if (!all(is.na(allowed))) {
      result <- result && all(x %in% allowed)
    }
    
    list(
      result = result,
      level = level
    )
  }
}

#' @describeIn any_obj Validate a logical vector
#' @export
lgl_vec <- function(
    max_len = Inf,
    min_len = 1,
    level = "error"
) {
  
  # Check validator inputs
  validator_input_check(level, max_len, min_len)
  
  function(x) {
    
    # Early return for non vectors
    if (!rlang::is_bare_logical(x)) {
      return(list(result = FALSE, level = level))
    }
    
    result <-
      all(c(
        length(x) <= max_len,
        length(x) >= min_len
      ))
    
    if (!all(is.na(allowed))) {
      result <- result && all(x %in% allowed)
    }
    
    list(
      result = result,
      level = level
    )
  }
}

#' @describeIn any_obj Ensure something is data like
#' @export
df_like <- function(max_row = Inf, min_row = 1, level = "error") {
  
  # Check validator inputs
  validator_input_check(level, max_row, min_row)
  
  function(x) {
    
    # Early return for non vectors
    if (!(inherits(x, "data.frame"))) {
      return(list(result = FALSE, level = level))
    }
    
    list(
      result = all(c(
        nrow(x) <= max_row,
        nrow(x) >= min_row
      )),
      level = level
    )
  }
}
