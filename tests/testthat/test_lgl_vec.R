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
