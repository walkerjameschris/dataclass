
dataclass_record <- function(level, report, valid = FALSE) {
  # Creates a dataclass record with custom attribute
  
  validity_check <- tibble::tibble(valid, level, report)
  attr(validity_check, ".dataclass") <- TRUE
  validity_check
}

dataclass_return <- function(level, tests) {
  # Returns a dataclass report
  
  issues <- names(tests[tests])
  report <- "Nothing to report"
  
  if (length(issues) >= 1) {
    report <- glue::glue_collapse(issues, sep = ", ")
  }
  
  dataclass_record(level, report, !any(tests))
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
#'   dataclass(
#'     dte_col = dte_vec(),
#'     chr_col = chr_vec(),
#'     # Custom column validator ensures values are positive!
#'     new_col = function(x) all(x > 0)
#'   ) |>
#'   data_validator()
#'
#' # Validate a data frame or data frame like objects!
#' data.frame(
#'   dte_col = as.Date("2022-01-01"),
#'   chr_col = "String!",
#'   new_col = 100
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
    
    dataclass_names <- names(formals(x))
    dataframe_names <- names(data)
    
    # Names in dataclass but not input data
    in_dataclass <-
      setdiff(
        x = dataclass_names,
        y = dataframe_names
      )
    
    # Names in input data but not dataclass
    in_dataframe <-
      setdiff(
        x = dataframe_names,
        y = dataclass_names
      )

    # Checks if columns defined in dataclass are in data
    if (length(in_dataclass) >= 1) {
      cli::cli_abort(c(
        "Input data is missing these columns:",
        purrr::set_names(in_dataclass, "i")
      ))
    }

    # Checks for bloat columns if strict_cols = TRUE
    if (strict_cols && length(in_dataframe) >= 1) {
      cli::cli_abort(c(
        "Ensure no additional columns are present!",
        "dataclass can only check for these known columns:",
        purrr::set_names(dataclass_names, "i"),
        "i" = "Set data_validator(strict_cols = FALSE)` to bypass this check."
      ))
    }

    # Main columns to check
    subset_cols <-
      data %>%
      dplyr::select(
        dplyr::all_of(dataclass_names)
      ) %>%
      as.list()
    
    rlang::exec(x, !!!subset_cols)
    data
  }
}

#' Validator: Allow any object
#'
#' This function is used to bypass dataclass checks for a given element. If you
#' do not want dataclass to check a given element, set the element equal to
#' any_obj() to allow any object. Keep in mind that while dataclass will bypass
#' the check, the object must still be a valid R object. Furthermore, if you
#' are using dataclass to create a tibble, then the object must be a valid
#' tibble column type, even if additional checks are not considered. This can be
#' dangerous because dataclass is designed to check objects, not bypass them.
#' Use this validator sparingly and consider how you can write a stricter
#' dataclass.
#'
#' @return
#' A function with the following properties:
#'
#' * Always returns TRUE
#' * Bypasses any dataclass checks
#'
#' @examples
#' # Define a dataclass:
#' my_dataclass <-
#'   dataclass(
#'     date_val = dte_vec(),
#'     anything = any_obj()
#'   )
#'
#' # While `date_val` must be a date, `anything` can be any value!
#' my_dataclass(
#'   date_val = as.Date("2022-01-01"),
#'   anything = lm(vs ~ am, mtcars)
#' )
#'
#' my_dataclass(
#'   date_val = as.Date("2022-01-01"),
#'   anything = c(1, 2, 3, 4, 5)
#' )
#'
#' my_dataclass(
#'   date_val = as.Date("2022-01-01"),
#'   anything = list(a = 1, b = 2)
#' )
#' @export
any_obj <- function() function(x) TRUE

#' Validator: Check if element is atomic
#'
#' This function is used to check whether something is atomic. Atomic elements
#' are represented by simple vectors, (i.e., numeric, logical, character) but
#' also include special vectors like date vectors. You can use this function
#' to check the length of a vector. You can also specify the level of a
#' violation. If level is set to "warn" then invalid inputs will warn you.
#' However, if level is set to "error" then invalid inputs will abort.
#'
#' @param max_len The maximum length of an atomic element
#' @param min_len The minimum length of an atomic element
#' @param level Setting "warn" throws a warning, setting "error" halts
#' @param allow_na Should NA values be allowed?
#' @param allow_dups Should duplicates be allowed?
#' @return
#' A function with the following properties:
#'
#' * Checks whether something is atomic
#' * Determines whether the check will throw warning or error
#' * Optionally checks for element length
#'
#' @examples
#' # Define a dataclass for testing atomic:
#' my_dataclass <-
#'   dataclass(
#'     num_val = num_vec(),
#'     # Setting warn means a warning will occur if violation is found
#'     # The default is "error" which is stricter and will halt upon violation
#'     atm_val = atm_vec(level = "warn")
#'   )
#'
#' # While `num_val` must be a number, `atm_val` can be any atomic element!
#' my_dataclass(
#'   num_val = c(1, 2, 3),
#'   atm_val = Sys.Date()
#' )
#'
#' my_dataclass(
#'   num_val = c(1, 2, 3),
#'   atm_val = c(TRUE, FALSE)
#' )
#'
#' my_dataclass(
#'   num_val = c(1, 2, 3),
#'   atm_val = c("This is", "a character!")
#' )
#' @export
atm_vec <- function(
    max_len = Inf,
    min_len = 1,
    level = "error",
    allow_na = FALSE,
    allow_dups = TRUE) {

  function(x) {

    if (!rlang::is_atomic(x)) {
      return(dataclass_record(level, "is not atomic"))
    }
    
    dataclass_return(
      level,
      tests = c(
        "NAs found" = !allow_na && any(is.na(x)),
        "duplicates found" = !allow_dups && (length(unique(x)) != length(x)),
        "too few values" = length(x) < min_len,
        "too many values" = length(x) > max_len
      )
    )
  }
}

#' Validator: Check if element is a date
#'
#' This function is used to check whether something is a date. You can use this
#' function to check the length of a date vector. You can also specify the level
#' of a violation. If level is set to "warn" then invalid inputs will warn you.
#' However, if level is set to "error" then invalid inputs will abort.
#'
#' @param max_len The maximum length of a date element
#' @param min_len The minimum length of a date element
#' @param level Setting "warn" throws a warning, setting "error" halts
#' @param allow_na Should NA values be allowed?
#' @param allow_dups Should duplicates be allowed?
#' @return
#' A function with the following properties:
#'
#' * Checks whether something is a date
#' * Determines whether the check will throw warning or error
#' * Optionally checks for element length
#'
#' @examples
#' # Define a dataclass for testing dates:
#' my_dataclass <-
#'   dataclass(
#'     num_val = num_vec(),
#'     # Setting warn means a warning will occur if violation is found
#'     # The default is "error" which is stricter and will halt upon violation
#'     dte_val = dte_vec(level = "warn")
#'   )
#'
#' # While `num_val` must be a number, `dte_val` must be a date!
#' my_dataclass(
#'   num_val = c(1, 2, 3),
#'   dte_val = Sys.Date()
#' )
#'
#' my_dataclass(
#'   num_val = c(1, 2, 3),
#'   dte_val = as.Date("2022-01-01")
#' )
#'
#' my_dataclass(
#'   num_val = c(1, 2, 3),
#'   dte_val = as.Date(c("2022-01-01", "2023-01-01"))
#' )
#' @export
dte_vec <- function(
    max_len = Inf,
    min_len = 1,
    level = "error",
    allow_na = FALSE,
    allow_dups = TRUE) {

  function(x) {
    
    if (!(inherits(x, "Date") || inherits(x, "POSIXct"))) {
      return(dataclass_record(level, "is not a date"))
    }
    
    dataclass_return(
      level,
      tests = c(
        "NAs found" = !allow_na && any(is.na(x)),
        "duplicates found" = !allow_dups && (length(unique(x)) != length(x)),
        "too few values" = length(x) < min_len,
        "too many values" = length(x) > max_len
      )
    )
  }
}

#' Validator: Check if element is a number
#'
#' This function is used to check whether something is a number. You can use
#' this function to check the length and min-max of a number vector. You can
#' also specify the level of a violation. If level is set to "warn" then invalid
#' inputs will warn you. However, if level is set to "error" then invalid inputs
#' will abort.
#'
#' @param max_len The maximum length of a numeric element
#' @param min_len The minimum length of a numeric element
#' @param max_val The maximum value of a numeric element
#' @param min_val The minimum value of a numeric element
#' @param allowed A vector of allowable values
#' @param level Setting "warn" throws a warning, setting "error" halts
#' @param allow_na Should NA values be allowed?
#' @param allow_dups Should duplicates be allowed?
#' @return
#' A function with the following properties:
#'
#' * Checks whether something is a number vector
#' * Determines whether the check will throw warning or error
#' * Optionally checks for element length
#' * Optionally checks for allowable values
#' * Optionally checks for max/min
#'
#' @examples
#' # Define a dataclass for testing numbers:
#' my_dataclass <-
#'   dataclass(
#'     dte_val = dte_vec(),
#'     # Setting warn means a warning will occur if violation is found
#'     # The default is "error" which is stricter and will halt upon violation
#'     # We also set allowed to 0 and 1 which means elements must be 0 or 1
#'     num_val = num_vec(level = "warn", allowed = c(0, 1))
#'   )
#'
#' # While `dte_val` must be a date, `num_val` must be 0 or 1!
#' my_dataclass(
#'   dte_val = Sys.Date(),
#'   num_val = c(0, 1, 1, 0, 1)
#' )
#'
#' my_dataclass(
#'   dte_val = Sys.Date(),
#'   num_val = 1
#' )
#'
#' # Set min and max requirements!
#' test_dataclass <-
#'   dataclass(
#'     num = num_vec(min_val = 1, max_val = 100)
#'   )
#'
#' # Value must be between 1 and 10 inclusive!
#' test_dataclass(num = 10.03012)
#' @export
num_vec <- function(
    max_len = Inf,
    min_len = 1,
    max_val = Inf,
    min_val = -Inf,
    allowed = NA,
    level = "error",
    allow_na = FALSE,
    allow_dups = TRUE) {

  function(x) {
    # Early return for non vectors
    if (!rlang::is_bare_numeric(x)) {
      return(dataclass_record(level, "is not numeric"))
    }
    
    x_narm <- x[!is.na(x)]
  
    # Report of issues
    dataclass_return(
      level,
      tests = c(
        "NAs found" = !allow_na && any(is.na(x)),
        "duplicates found" = !allow_dups && (length(unique(x)) != length(x)),
        "too few values" = length(x) < min_len,
        "too many values" = length(x) > max_len,
        "values exceed upper bound" = any(x_narm > max_val),
        "values are below lower bound" = any(x_narm < min_val),
        "non-allowable values found" = !all(is.na(allowed)) && !all(x %in% allowed)
      )
    )
  }
}

#' Validator: Check if element is a character
#'
#' This function is used to check whether something is a character. You can use
#' this function to check the length and allowable values of character. You can
#' also specify the level of a violation. If level is set to "warn" then invalid
#' inputs will warn you. However, if level is set to "error" then invalid inputs
#' will abort.
#'
#' @param max_len The maximum length of a character element
#' @param min_len The minimum length of a character element
#' @param allowed A vector of allowable values
#' @param level Setting "warn" throws a warning, setting "error" halts
#' @param allow_na Should NA values be allowed?
#' @param allow_dups Should duplicates be allowed?
#' @return
#' A function with the following properties:
#'
#' * Checks whether something is a character vector
#' * Determines whether the check will throw warning or error
#' * Optionally checks for element length
#' * Optionally checks for allowable values
#'
#' @examples
#' # Define a dataclass for testing characters:
#' my_dataclass <-
#'   dataclass(
#'     string = chr_vec(allowed = c("this", "or", "that")),
#'     other_string = chr_vec()
#'   )
#'
#' # `string` must be one of these: `c("this", "or", "that")`
#' my_dataclass(
#'   string = "this",
#'   other_string = "I can be anything I want (as long as I am a string)"
#' )
#' @export
chr_vec <- function(
    max_len = Inf,
    min_len = 1,
    allowed = NA,
    level = "error",
    allow_na = FALSE,
    allow_dups = TRUE) {

  function(x) {

    if (!rlang::is_bare_character(x)) {
      return(dataclass_record(level, "is not a character"))
    }
    
    # Report of issues
    dataclass_return(
      level,
      tests = c(
        "NAs found" = !allow_na && any(is.na(x)),
        "duplicates found" = !allow_dups && (length(unique(x)) != length(x)),
        "too few values" = length(x) < min_len,
        "too many values" = length(x) > max_len,
        "non-allowable values found" = !all(is.na(allowed)) && !all(x %in% allowed)
      )
    )
  }
}

#' Validator: Check if element is a logical
#'
#' This function is used to check whether something is a logical. You can use
#' this function to check the length of a logical vector. You can also specify
#' the level of a violation. If level is set to "warn" then invalid inputs will
#' warn you. However, if level is set to "error" then invalid inputs will abort.
#'
#' @param max_len The maximum length of a logical element
#' @param min_len The minimum length of a logical element
#' @param level Setting "warn" throws a warning, setting "error" halts
#' @param allow_na Should NA values be allowed?
#' @return
#' A function with the following properties:
#'
#' * Checks whether something is a logical vector
#' * Determines whether the check will throw warning or error
#' * Optionally checks for element length
#'
#' @examples
#' # Define a dataclass for testing logicals:
#' my_dataclass <-
#'   dataclass(
#'     bool = lgl_vec()
#'   )
#'
#' # `bool` must be a logical vector of any length!
#' my_dataclass(
#'   bool = TRUE
#' )
#' @export
lgl_vec <- function(
    max_len = Inf,
    min_len = 1,
    level = "error",
    allow_na = FALSE) {

  function(x) {

    if (!rlang::is_bare_logical(x)) {
      return(dataclass_record(level, "is not a logical"))
    }

    dataclass_return(
      level,
      tests = c(
        "NAs found" = !allow_na && any(is.na(x)),
        "too few values" = length(x) < min_len,
        "too many values" = length(x) > max_len
      )
    )
  }
}

#' Validator: Check if element is a data like object
#'
#' This function is used to check whether something is data like. You can use
#' this function to check the data row count. You can also specify the level of
#' a violation. If level is set to "warn" then invalid inputs will warn you.
#' However, if level is set to "error" then invalid inputs will abort.
#'
#' @param max_row The maximum row count of a data element
#' @param min_row The minimum row count of a data element
#' @param level Setting "warn" throws a warning, setting "error" halts
#' @return
#' A function with the following properties:
#'
#' * Checks whether something is a data frame like object
#' * Determines whether the check will throw warning or error
#' * Optionally checks for row count
#'
#' @examples
#' # Define a dataclass for testing data:
#' my_dataclass <-
#'   dataclass(
#'     df = df_like(100)
#'   )
#'
#' # `df` must be a data like object with at most 100 rows!
#' my_dataclass(
#'   df = mtcars
#' )
#' @export
df_like <- function(max_row = Inf, min_row = 1, level = "error") {

  function(x) {
    
    if (!inherits(x, "data.frame")) {
      return(dataclass_record(level, "is not data frame like"))
    }

    dataclass_return(
      level,
      tests = c(
        "too few rows" = nrow(x) < min_row,
        "too many rows" = nrow(x) > max_row
      )
    )
  }
}
