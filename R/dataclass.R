
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
#' @examples
#' \dontrun{
#' my_dataclass <- dataclass(
#'   min_date = dte_vec(1), # Ensures min_date is a date vector of length 1
#'   max_date = dte_vec(1), # Ensures max_date is a date vector of length 1
#'   run_data = df_like(),  # Ensures run_date is a data object (i.e. tibble)
#'   run_note = chr_vec(1)  # Ensures run_note is a character vector of length 1
#' )
#'
#' # This returns a validated list!
#' my_dataclass(
#'   min_date = as.Date("2022-01-01"),
#'   max_date = as.Date("2023-01-01"),
#'   run_data = head(mtcars),
#'   run_note = "A note!"
#' )
#'
#' # This throws an error since run_data is not a data frame
#' my_dataclass(
#'   min_date = as.Date("2022-01-01"),
#'   max_date = as.Date("2023-01-01"),
#'   run_data = c(1, 2, 3),
#'   run_note = "A note!"
#' )
#'
#' # An example with anonymous functions
#' dataclass(
#'   start_date = dte_vec(1),
#'   # Ensures calculation is a column in this data and is data like
#'   results_df = function(x) "calculation" %in% names(x) && df_like(x)
#' )
#' 
#' # Define a dataclass for creating a tibble! Simply omit length restrictions:
#' my_df_dataclass <-
#'  dataclass(
#'    dte_col = dte_vec(),
#'    chr_col = chr_vec(),
#'    # Custom column validator which ensures column is numeric and postitive!
#'    new_col = function(x) num_vec(x) && all(x > 0)
#'  ) %>%
#'  # You MUST convert to a data validator for use with data frames
#'  data_validator()
#' 
#' # Validate a tibble!
#' tibble(
#'  dte_col = as.Date("2022-01-01"),
#'  chr_col = "String!",
#'  new_col = 100
#' ) %>%
#'  my_df_dataclass()
#' }
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
      "All validators must be named functions!",
      purrr::set_names(non_functions, "x")
    ))
  }

  # Vanilla dataclass function
  new_dataclass <- function() {

    # Assemble dataclass inputs and validators
    inputs <- as.list(environment())
    validators <- rlang::dots_list(..., .named = TRUE)

    # Determine input validity
    valid <-
      inputs %>%
      purrr::imap_lgl(function(input, name) {
        validator <- validators[[name]]
        validator(input)
      })

    # Subset to invalid inputs
    invalid <- valid[!valid]

    if (length(invalid) >= 1) {
      cli::cli_abort(c(
        "The following elements are invalid:",
        purrr::set_names(names(invalid), "x")
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
