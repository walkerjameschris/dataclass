testthat::test_that("Invalid atomic input:", {
  # Un-handled duplicates
  testthat::expect_error(
    dataclass::dataclass(atm = dataclass::atm_vec(allow_dups = FALSE))(
      atm = c(1, 1)
    )
  )
  
  # Un-handled NAs
  testthat::expect_error(
    dataclass::dataclass(atm = dataclass::atm_vec())(
      atm = c("too", NA)
    )
  )
  
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
