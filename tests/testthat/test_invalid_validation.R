
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
  
  # Atomic input is not atomic
  testthat::expect_warning(
    dataclass::dataclass(atm = dataclass::atm_vec(level = "warn"))(
      atm = list("Not atomic!")
    )
  )
  
  # Atomic input too long
  testthat::expect_warning(
    dataclass::dataclass(atm = dataclass::atm_vec(1, level = "warn"))(
      atm = c(1, 2, 3, 4)
    )
  )
  
  # Atomic input too short
  testthat::expect_warning(
    dataclass::dataclass(atm = dataclass::atm_vec(Inf, 3, level = "warn"))(
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
  
  # Numeric input is not numeric
  testthat::expect_warning(
    dataclass::dataclass(num = dataclass::num_vec(level = "warn"))(
      num = "Not numeric!"
    )
  )
  
  # Numeric input too long
  testthat::expect_warning(
    dataclass::dataclass(num = dataclass::num_vec(1, level = "warn"))(
      num = c(1, 2, 3, 4)
    )
  )
  
  # Numeric input too short
  testthat::expect_warning(
    dataclass::dataclass(num = dataclass::num_vec(Inf, 3, level = "warn"))(
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
  
  # Logical input is not logical
  testthat::expect_warning(
    dataclass::dataclass(lgl = dataclass::lgl_vec(level = "warn"))(
      lgl = "Not logical!"
    )
  )
  
  # Logical input too long
  testthat::expect_warning(
    dataclass::dataclass(lgl = dataclass::lgl_vec(1, level = "warn"))(
      lgl = c(TRUE, FALSE, TRUE, TRUE)
    )
  )
  
  # Logical input too short
  testthat::expect_warning(
    dataclass::dataclass(lgl = dataclass::lgl_vec(Inf, 3, level = "warn"))(
      lgl = c(FALSE, FALSE)
    )
  )
})

testthat::test_that("Invalid data input:", {
  
  # Data incorrect types
  testthat::expect_error(
    dataclass::dataclass(dfl = dataclass::df_like())(
      dfl = "Not a data frame like object!"
    )
  )
  
  # Data incorrect types
  testthat::expect_error(
    dataclass::dataclass(dfl = dataclass::df_like())(
      dfl = list(a = 1, b = 2)
    )
  )
  
  # Data incorrect types
  testthat::expect_error(
    dataclass::dataclass(dfl = dataclass::df_like())(
      dfl = c("a" = 1, "b" = 2)
    )
  )
})

testthat::test_that("Passing data into un-converted dataclass:", {
  
  # dataclass not converted
  testthat::expect_error(
    dataclass::dataclass(my_col = dataclass::chr_vec())(
      tibble::tibble(my_col = "")
    )
  )
})

testthat::test_that("Passing unknown columns into dataclass:", {
  
  test_class <-
    dataclass::dataclass(
      my_col = dataclass::chr_vec(),
      column = dataclass::atm_vec()
    ) %>%
    dataclass::data_validator()
  
  test_df <-
    tibble::tibble(
      my_col = "",
      column = "",
      other = 1
    )
  
  # Unknown columns
  testthat::expect_error(test_class(test_df))
})

