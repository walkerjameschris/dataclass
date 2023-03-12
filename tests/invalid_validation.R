
testthat::context("Test invalid dataclass() validation:")

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
})

testthat::test_that("Invalid atomic input:", {
  
  # Atomic input is not atomic
  testthat::expect_error(
    dataclass::dataclass(atm = dataclass::atm_vec())(
      atm = list("Not atomic!")
    )
  )
  
  # Atomic input too long
  testthat::expect_error(
    dataclass::dataclass(atm = dataclass::atm_vec(1))(
      atm = c(1, 2, 3, 4)
    )
  )
  
  # Atomic input too short
  testthat::expect_error(
    dataclass::dataclass(atm = dataclass::atm_vec(Inf, 3))(
      atm = c("too", "short")
    )
  )
})

testthat::test_that("Invalid numeric input:", {
  
  # Numeric input is not numeric
  testthat::expect_error(
    dataclass::dataclass(num = dataclass::num_vec())(
      num = "Not numeric!"
    )
  )
  
  # Numeric input too long
  testthat::expect_error(
    dataclass::dataclass(num = dataclass::num_vec(1))(
      num = c(1, 2, 3, 4)
    )
  )
  
  # Numeric input too short
  testthat::expect_error(
    dataclass::dataclass(num = dataclass::num_vec(Inf, 3))(
      num = c(1, 2)
    )
  )
})

testthat::test_that("Invalid logical input:", {
  
  # Logical input is not logical
  testthat::expect_error(
    dataclass::dataclass(lgl = dataclass::lgl_vec())(
      lgl = "Not logical!"
    )
  )
  
  # Logical input too long
  testthat::expect_error(
    dataclass::dataclass(lgl = dataclass::lgl_vec(1))(
      lgl = c(TRUE, FALSE, TRUE, TRUE)
    )
  )
  
  # Logical input too short
  testthat::expect_error(
    dataclass::dataclass(lgl = dataclass::lgl_vec(Inf, 3))(
      lgl = c(FALSE, FALSE)
    )
  )
})

testthat::test_that("Invalid data input:", {
  
  # Data input is not data like
  testthat::expect_error(
    dataclass::dataclass(dfl = dataclass::df_like())(
      dfl = "Not a data frame like object!"
    )
  )
  
  # Data input too many rows
  testthat::expect_error(
    dataclass::dataclass(dfl = dataclass::df_like(1))(
      dfl = mtcars
    )
  )
  
  # Data input too few rows
  testthat::expect_error(
    dataclass::dataclass(dfl = dataclass::df_like(Inf, 3))(
      dfl = data.frame(col = 1)
    )
  )
})
