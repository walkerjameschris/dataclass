
dataclass_record <- function(level, report, valid = FALSE) {
  # Creates a dataclass record with custom attribute
  
  validity_check <- tibble::tibble(valid, level, report)
  attr(validity_check, "dataclass_validator") <- TRUE
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

#' @method print dataclass
#' @export
print.dataclass <- function(x, ...) {
  # Print method for dataclass
  
  is_dataclass <- class(x) == "dataclass"
  is_data_validator <- !is.null(attr(x, "dataclass_columns"))
  
  if (is_data_validator && is_dataclass) {
    cli::cli_inform(c(
      ">" = "A dataclass",
      "v" = "This dataclass checks a dataframe.",
      "i" = "Columns to be checked:",
      purrr::set_names(attr(x, "dataclass_columns"), "*")
    ))
  }
  
  if (!is_data_validator && is_dataclass) {
    cli::cli_inform(c(
      ">" = "A dataclass",
      "v" = "This dataclass creates a list.",
      "i" = "Elements to be checked:",
      purrr::set_names(names(as.list(formals(x))), "*")
    ))
  } 
  
  invisible(x)
}
