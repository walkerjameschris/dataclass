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
  # Extract validators
  validator_list <- list(...)
  validator_name <- names(validator_list)

  # Checks if all elements are unnamed
  if (is.null(validator_name)) {
    cli::cli_abort("All validators must be named!")
  }

  # Checks if some elements are unnamed
  unnamed_element <- which(validator_name == "")

  if (length(unnamed_element) >= 1) {
    cli::cli_abort(c(
      "Validators at these positions are unnamed:",
      purrr::set_names(unnamed_element, "x")
    ))
  }

  # Checks if elements are functions
  function_check <-
    validator_list %>%
    purrr::map_lgl(is.function)

  # Determine non-functions
  non_functions <- validator_name[!function_check]

  # Ensures inputs are functions
  if (length(non_functions) >= 1) {
    cli::cli_abort(c(
      "These validators are not named functions:",
      purrr::set_names(non_functions, "x")
    ))
  }

  # Vanilla dataclass function
  new_dataclass <- function() {
    # Assemble dataclass inputs discarding unused arguments
    inputs <-
      environment() %>%
      as.list() %>%
      purrr::discard(rlang::is_symbol)

    # Assemble validators
    validators <- rlang::dots_list(..., .named = TRUE)

    # Missing args
    missing_args <- setdiff(names(validators), names(inputs))

    # Throw error if arguments are missing
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
      inputs %>%
      purrr::imap(function(input, name) {
        # Determine which validator to use
        validator <- validators[[name]]
        result <- validator(input)

        # Upgrade simple validators
        if (rlang::is_bare_logical(result)) {
          return(tibble::tibble(
            name,
            valid = result,
            level = "error"
          ))
        }

        # Handle advanced validators
        if (rlang::is_bare_list(result)) {
          return(tibble::tibble(
            name = glue::glue("{name}: {result$report}"),
            valid = result$result,
            level = result$level
          ))
        }

        tibble::tibble(
          name,
          valid = "unknown",
          level = "unknown"
        )
      }) %>%
      dplyr::bind_rows()

    # Error separation
    # TODO: This was originally done with dplyr::filter() but lintr rejects it
    # TODO: Learn more about unbound variables inside packages
    problematic <- valid[["name"]][valid[["valid"]] == "unknown"]
    warn_args <- valid[["name"]][!valid[["valid"]] & valid[["level"]] == "warn"]
    err_args <- valid[["name"]][!valid[["valid"]] & valid[["level"]] == "error"]

    if (length(problematic) >= 1) {
      cli::cli_abort(c(
        "These validators returned an unexpected result!",
        purrr::set_names(problematic, "x"),
        "i" = "Custom validators can only return TRUE/FALSE.",
        "i" = "dataclass built-in validators have more advanced behavior.",
        "See the documentation for more examples."
      ))
    }

    if (length(warn_args) >= 1) {
      cli::cli_warn(c(
        "The following elements have warn-level violations:",
        purrr::set_names(warn_args, "x")
      ))
    }

    if (length(err_args) >= 1) {
      cli::cli_abort(c(
        "The following elements have error-level violations:",
        purrr::set_names(err_args, "x")
      ))
    }

    # Return inputs if no violations are found
    inputs
  }

  # Determines validator names
  args <-
    rlang::call_match() %>%
    rlang::call_args_names() %>%
    glue::glue_collapse(sep = ", ")

  # Creates formals for dataclass
  named_fn <-
    glue::glue("function({args}) {{}}") %>%
    rlang::parse_expr() %>%
    rlang::eval_bare()

  # Returns new dataclass
  formals(new_dataclass) <- formals(named_fn)
  new_dataclass
}
