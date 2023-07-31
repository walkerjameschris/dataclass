#' Construct a dataclass in R
#'
#' Building a dataclass is easy! Provide names for each of the elements you want
#' in your dataclass and an associated validator. The dataclass package comes
#' with several built in validators, but you can define a custom validator as
#' an anonymous function or named function to be bundled with your dataclass.
#'
#' dataclass() will return a new function with named arguments for each of the
#' elements you define here. If you want to use your dataclass on data frames or
#' tibbles you must pass the dataclass to data_validator() to modify behavior.
#'
#' @param ... Elements to validate (i.e., dte_vec() will validate a date vector)
#' @return
#' A function with the following properties:
#'
#' * An argument for each element provided to dataclass()
#' * Each argument in the returned function will validate inputs
#' * An error occurs if new elements passed to the returned function are invalid
#' * List is returned if new elements passed to the returned function are valid
#'
#' @examples
#' my_dataclass <- dataclass(
#'   min_date = dte_vec(1), # Ensures min_date is a date vector of length 1
#'   max_date = dte_vec(1), # Ensures max_date is a date vector of length 1
#'   run_data = df_like(), # Ensures run_date is a data object (i.e. tibble)
#'   run_note = chr_vec(1) # Ensures run_note is a character vector of length 1
#' )
#'
#' # This returns a validated list!
#' my_dataclass(
#'   min_date = as.Date("2022-01-01"),
#'   max_date = as.Date("2023-01-01"),
#'   run_data = head(mtcars, 2),
#'   run_note = "A note!"
#' )
#'
#' # An example with anonymous functions
#' a_new_dataclass <-
#'   dataclass(
#'     start_date = dte_vec(1),
#'     # Ensures calculation is a column in this data and is data like
#'     results_df = function(df) "calculation" %in% colnames(df)
#'   )
#'
#' # Define a dataclass for creating data! Wrap in data_validator():
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
#' @export
#' @importFrom magrittr `%>%`
dataclass <- function(...) {
  
  # Extract inputs and their components for validation
  validator_funs <- list(...)
  validator_name <- names(validator_funs)
  not_named_funs <- which(validator_name == "")
  non_functions  <- names(purrr::discard(validator_funs, ~ is.function(.x)))
  
  if (is.null(validator_name)) {
    cli::cli_abort("All validators must be named functions!")
  }

  if (length(not_named_funs) >= 1) {
    cli::cli_abort(c(
      "Validators at these positions are unnamed:",
      purrr::set_names(not_named_funs, "x")
    ))
  }
  
  if (length(non_functions) >= 1) {
    cli::cli_abort(c(
      "Validators at these positions are not named functions:",
      purrr::set_names(non_functions, "x")
    ))
  }
  
  # If all validators pass checks, create new dataclass
  new_dataclass <- function() {
    
    inputs_to_validate <-
      as.list(environment()) %>%
      purrr::discard(rlang::is_symbol)
    
    missing_args <-
      setdiff(
        x = names(validator_funs),
        y = names(inputs_to_validate)
      )
    
    if (length(missing_args) >= 1) {
      cli::cli_abort(c(
        "The following arguments are missing!",
        purrr::set_names(missing_args, "x"),
        "i" = "Ensure all arguments are filled",
        "i" = glue::glue(
          "If you are validating a data frame, don't forget to pass your ",
          "dataclass to data_validator() upon creation!"
        )
      ))
    }
    
    # Determine input validity
    valid <-
      inputs_to_validate %>%
      purrr::imap(function(input, name) {
        
        result <- validator_funs[[name]](input)
        
        # Upgrade simple validators (most often a user defined function)
        if (rlang::is_bare_logical(result)) {
          return(tibble::tibble(
            report = name,
            valid = result,
            level = "error"
          ))
        }
        
        # Handle dataclass validators
        if (!is.null(attr(result, "dataclass_validator"))) {
          return(dplyr::mutate(
            result,
            report = glue::glue("{name}: {report}")
          ))
        }
        
        # Return unknown for problematic validators
        tibble::tibble(
          report = name,
          valid = FALSE,
          level = "unknown"
        )
      }) %>%
      dplyr::bind_rows()
    
    # Error separation
    issue <- valid[["report"]][valid[["level"]] == "unknown"]
    warns <- valid[["report"]][!valid[["valid"]] & valid[["level"]] == "warn"]
    error <- valid[["report"]][!valid[["valid"]] & valid[["level"]] == "error"]
    
    if (length(issue) >= 1) {
      cli::cli_abort(c(
        "These validators returned an unexpected result!",
        purrr::set_names(issue, "x"),
        "i" = "Custom validators can only return TRUE/FALSE.",
        "i" = "dataclass built-in validators have more advanced behavior.",
        "See the documentation for more examples."
      ))
    }
    
    if (length(warns) >= 1) {
      cli::cli_warn(c(
        "The following elements have warn-level violations:",
        purrr::set_names(warns, "x")
      ), call = NULL)
    }
    
    if (length(error) >= 1) {  
      cli::cli_abort(c(
        "The following elements have error-level violations:",
        purrr::set_names(error, "x")
      ), call = NULL)
    }
    
    inputs_to_validate
  }
  
  named_function <-
    glue::glue(
      "function({args}) {{}}",
      args = glue::glue_collapse(
        validator_name,
        sep = ", "
      )
    ) %>%
    rlang::parse_expr() %>%
    rlang::eval_bare()
  
  formals(new_dataclass) <- formals(named_function)
  attr(new_dataclass, "dataclass") <- TRUE
  
  new_dataclass
}
