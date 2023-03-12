
testthat::context("Test invalid dataclass() creation:")

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
      dataclass::df_like()
    )
  )
})

testthat::test_that("Non-function validators:", {
  
  testthat::expect_error(
    dataclass::dataclass(
      nums = "This is NOT a function!",
      chrs = list("Also not a function", 1)
    )
  )
})
