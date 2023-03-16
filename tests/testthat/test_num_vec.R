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
  
  # Numeric input is too small
  testthat::expect_error(
    dataclass::dataclass(num = dataclass::num_vec(min_val = 10))(
      num = 0
    )
  )
  
  # Numeric input too large
  testthat::expect_error(
    dataclass::dataclass(num = dataclass::num_vec(max_val = 100))(
      num = Inf
    )
  )
  
  # Numeric layered violation
  testthat::expect_error(
    dataclass::dataclass(num = dataclass::num_vec(2, 1, 100, 0))(
      num = c(1000, 300, 100)
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