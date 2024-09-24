
int <- identity
dbl <- identity
lgl <- identity
chr <- identity
tbl <- identity

#' Enforced typing in R
#' `r lifecycle::badge("experimental")`
#'
#' This function allows for simple type enforcement in R inspired by C++ and
#' other compiled languages. There are currently six primitive types which the
#' function handles:
#' 
#' * `int()`: An integer specified with the *L* syntax (i.e., `1L``)
#' * `chr()`: A string or character
#' * `lgl()`: A boolean TRUE/FALSE
#' * `dbl()`: A double or numeric value
#' * `tbl()`: A data frame or tibble (types within the data frame are not checked)
#' 
#' You can also provide default arguments within the parenthesis of the type. This
#' is shown in the example below. You can provide new arguments as well. The function
#' has knowledge of the function declaration when it runs. Note: types are checked
#' at runtime not when the function is declared.
#' 
#' @param level Should type failures error, warn, or be skipped (none)?
#'
#' @examples
#' foo <- function(
#'   x = int(1L),
#'   y = chr("Hello!"),
#'   z = lgl(TRUE),
#'   a = dbl(1.1),
#'   b = tbl(mtcars),
#'   c = NULL # This argument will not be checked
#' ) {
#'
#'   # Simply place enforce_types() in your function header!
#'   dataclass::enforce_types()
#'
#'   # Function logic ...
#' }
#' 
#' # This run the function with the type defaults
#' foo()
#' 
#' # This will check types but for new arguments
#' foo(2L, "Hi!", FALSE, 1.2, mtcars)
#' 
#' # This would fail because types are incorrect!
#' # foo(1.1, FALSE, NULL, "Hi", list())
#' 
#' # This function will only warn when there are type failures
#' bar <- function(x = int(1)) {
#'   dataclass::enforce_types("warn")
#' }
#' @export
#' @import purrr glue cli
enforce_types <- function(level = c("error", "warn", "none")) {

  fun <- sys.function(sys.parent())
  level <- rlang::arg_match(level)

  if (level == "none") {
    return(NULL)
  }

  type_match <-
    list(
      int = "integer",
      dbl = "numeric",
      lgl = "logical",
      chr = "character",
      tbl = "data.frame"
    )

  args <- as.list(parent.frame())
  types <- as.list(formals(fun))

  bad_types <-
    purrr::imap(types, function(x, name) {

      info <- as.character(x)[1]

      if (!info %in% names(type_match)) {
        return(NULL)
      }

      provided <- class(purrr::chuck(args, name))
      specified <- purrr::chuck(type_match, info)
        
      if (specified %in% provided) {
        return(NULL)
      }

      provided <- provided[length(provided)]

      glue::glue(
        "`[name]`: {.cls {'[provided]'}} should be {.cls {'[specified]'}}",
        .open = "[",
        .close = "]"
      )
      
    }) |>
    purrr::discard(is.null) |>
    purrr::map_chr(identity) |>
    purrr::set_names("*")

  rejector <-
    switch(
      level,
      error = cli::cli_abort,
      warn = cli::cli_warn
    )

  if (length(bad_types) > 0) {
    rejector(bad_types)
  }
}
