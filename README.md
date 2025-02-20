# dataclass <img src='https://chrisjameswalker.com/wp-content/uploads/2023/03/logo.png' align="right" height="140" />

> [!IMPORTANT]
> `dataclass` was archived on CRAN. There are better, well supported,
> alternatives for input validation like [`pointblank`](https://github.com/rstudio/pointblank).

## Easily Create Structured Lists or Data Frames with Input Validation

Easily define templates for lists and data frames that validate each element.
Specify the expected type (i.e., character, numeric, etc), expected length,
minimum and maximum values, allowable values, and more for each element in your
data. Decide whether violations of these expectations should throw an error or a
warning. This package is useful for validating data within R processes which
pull from dynamic data sources such as databases and web APIs to provide an
extra layer of validation around input and output data.
