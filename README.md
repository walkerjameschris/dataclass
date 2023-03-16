# dataclass <img src='https://chrisjameswalker.com/wp-content/uploads/2023/03/logo.png' align="right" height="140" />

[![CRAN status](https://www.r-pkg.org/badges/version/dataclass)](https://cran.r-project.org/package=dataclass)
[![R build status](https://github.com/walkerjameschris/dataclass/workflows/R-CMD-check/badge.svg)](https://github.com/walkerjameschris/dataclass/actions?workflow=R-CMD-check)

## Easily Create Structured Lists or Data Frames with Input Validation

Easily define templated lists with an associated validator function
for each element in the list. For example, if you wanted to create a list
with a min date, max date, and a dataframe, you could use dataclass to
validate that each of the elements are present and correct. This package
comes bundled with common validators, however, you can easily define your
own with an anonymous function. This could be considered a very-minimal
variant of the S7 standard aimed at standardizing structured data generation
within an R process.

```r
my_dataclass <- dataclass(
 min_date = dte_vec(1), # Ensures min_date is a date vector of length 1
 max_date = dte_vec(1), # Ensures max_date is a date vector of length 1
 run_data = df_like(),  # Ensures run_date is a data object (i.e. tibble)
 run_note = chr_vec(1)  # Ensures run_note is a character vector of length 1
)

# This returns a validated list!
my_dataclass(
  min_date = as.Date("2022-01-01"),
  max_date = as.Date("2023-01-01"),
  run_data = head(mtcars, 2),
  run_note = "A note!"
)

# An example with anonymous functions
a_new_dataclass <-
  dataclass(
    start_date = dte_vec(1),
    # Ensures calculation is a column in this data and is data like
    results_df = function(df) "calculation" %in% colnames(df)
  )

# Define a dataclass for creating data! Wrap in data_validator():
my_df_dataclass <-
  dataclass(
    dte_col = dte_vec(),
    chr_col = chr_vec(),
    # Custom column validator ensures values are positive!
    new_col = function(x) all(x > 0)
  ) |>
  data_validator()

# Validate a data frame or data frame like objects!
data.frame(
  dte_col = as.Date("2022-01-01"),
  chr_col = "String!",
  new_col = 100
) |>
  my_df_dataclass()
```
