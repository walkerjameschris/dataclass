
#' Construct a dataclass in R
#'
#' Building a dataclass is easy! Provide names for each of the elements you want
#' in your dataclass and an associated validator. The dataclass package comes
#' with several build in validators, but you can define a custom validator as
#' an anonymous function or named function to be bundled with your data.
#'
#' This function will return a new function with named argument for each of the
#' elements you define here. If you
#'
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
#'    new_col = function(x) num_vec() && all(x > 0)
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

  # Ensures inputs are named
  if (is.null(names(list(...)))) {
    stop("All validators must be named!")
  }

  # Ensures inputs are functions
  purrr::iwalk(
    list(...),
    function(v, i) {

      if (!is.function(v)) {
        stop("All validators must be functions!")
      }

      if (i == "") {
        stop("All validators must be named!")
      }
    }
  )

  # Vanilla validator function
  validator <- function() {

    inputs <- as.list(environment())

    purrr::walk2(
      inputs,
      rlang::dots_list(..., .named = TRUE),
      function(i, fun) {

        if (!fun(i)) {
          stop("Element is not valid!")
        }
      }
    )

    inputs
  }

  # Determines validator names
  args <-
    rlang::call_match() %>%
    rlang::call_args_names() %>%
    glue::glue_collapse(sep = ", ")

  # Creates formals for validator
  named_fn <-
    glue::glue("function({args}) {{}}") %>%
    rlang::parse_expr() %>%
    rlang::eval_bare()

  # Returns validator
  formals(validator) <- formals(named_fn)
  validator
}
