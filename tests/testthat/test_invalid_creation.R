testthat::test_that("Unnamed validators:", {
  # Create dataclass without any named elements
  testthat::expect_error(
    dataclass::dataclass(
      dataclass::any_obj(),
      dataclass::df_like()
    )
  )

  # Create dataclass with partially named elements
  testthat::expect_error(
    dataclass::dataclass(
      numbers = dataclass::num_vec(),
      dataclass::df_like(),
      values = function(x) TRUE,
      dataclass::atm_vec()
    )
  )
})

testthat::test_that("Non-function validators:", {
  testthat::expect_error(
    dataclass::dataclass(
      nums = "This is NOT a function!",
      chrs = list("Also not a function", 1),
      test = dataclass::atm_vec()
    )
  )
})

testthat::test_that("Passing data into un-converted dataclass:", {
  # dataclass not converted
  testthat::expect_error(
    dataclass::dataclass(
      my_col = dataclass::chr_vec(),
      other_col = dataclass::num_vec()
    )(
      tibble::tibble(my_col = "")
    )
  )
})
