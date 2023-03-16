testthat::test_that("Invalid data like input:", {
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

  # Too many rows
  testthat::expect_error(
    dataclass::dataclass(dfl = dataclass::df_like(max_row = 10))(
      dfl = mtcars
    )
  )

  # Too few rows
  testthat::expect_warning(
    dataclass::dataclass(dfl = dataclass::df_like(
      min_row = 100, level = "warn"
    ))(
      dfl = mtcars
    )
  )

  # Too many rows
  testthat::expect_warning(
    dataclass::dataclass(dfl = dataclass::df_like(
      max_row = 10, level = "warn"
    ))(
      dfl = mtcars
    )
  )

  # Too few rows
  testthat::expect_error(
    dataclass::dataclass(dfl = dataclass::df_like(min_row = 100))(
      dfl = mtcars
    )
  )

  # Data incorrect types
  testthat::expect_warning(
    dataclass::dataclass(dfl = dataclass::df_like(level = "warn"))(
      dfl = "Not a data frame like object!"
    )
  )

  # Data incorrect types
  testthat::expect_warning(
    dataclass::dataclass(dfl = dataclass::df_like(level = "warn"))(
      dfl = list(a = 1, b = 2)
    )
  )

  # Data incorrect types
  testthat::expect_warning(
    dataclass::dataclass(dfl = dataclass::df_like(level = "warn"))(
      dfl = c("a" = 1, "b" = 2)
    )
  )
})

testthat::test_that("Passing unknown columns into dataclass:", {
  test_class <-
    dataclass::dataclass(
      my_col = dataclass::chr_vec(),
      column = dataclass::atm_vec()
    ) |>
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
