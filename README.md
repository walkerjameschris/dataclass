## dataclass

### Easily Create Structured Lists or Data Frames with Input Validation

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
   results_df = function(x) "calculation" %in% names(x) && df_like(x)
 )

# Define a dataclass for creating data! Wrap in data_validator():
my_df_dataclass <-
data_validator(dataclass(
  dte_col = dte_vec(),
  chr_col = chr_vec(),
  # Custom column validator ensures values are positive!
  new_col = function(x) all(x > 0)
))

# Validate a data frame or data frame like objects!
my_df_dataclass(data.frame(
  dte_col = as.Date("2022-01-01"),
  chr_col = "String!",
  new_col = 100
))
```
