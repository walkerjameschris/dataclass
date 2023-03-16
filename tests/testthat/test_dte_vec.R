testthat::test_that("Invalid date input:", {
  
  # Date input is not date
  testthat::expect_error(
    dataclass::dataclass(dt = dataclass::dte_vec())(
      dt = "Not a date!"
    )
  )
  
  # Date input too long
  testthat::expect_error(
    dataclass::dataclass(dt = dataclass::dte_vec(1))(
      dt = as.Date(c("2022-01-01", "2023-01-01"))
    )
  )
  
  # Date input too short
  testthat::expect_error(
    dataclass::dataclass(dt = dataclass::dte_vec(Inf, 3))(
      dt = as.Date(c("2022-01-01", "2023-01-01"))
    )
  )
  
  # Date input is not date
  testthat::expect_warning(
    dataclass::dataclass(dt = dataclass::dte_vec(level = "warn"))(
      dt = "Not a date!"
    )
  )
  
  # Date input too long
  testthat::expect_warning(
    dataclass::dataclass(dt = dataclass::dte_vec(1, level = "warn"))(
      dt = as.Date(c("2022-01-01", "2023-01-01"))
    )
  )
  
  # Date input too short
  testthat::expect_warning(
    dataclass::dataclass(dt = dataclass::dte_vec(Inf, 3, level = "warn"))(
      dt = as.Date(c("2022-01-01", "2023-01-01"))
    )
  )
})
