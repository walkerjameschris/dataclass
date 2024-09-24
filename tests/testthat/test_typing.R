foo <- function(
  x = int(1L),
  y = chr("Hello!"),
  z = lgl(TRUE),
  a = dbl(1.1),
  b = tbl(mtcars),
  level = "error"
) {
  dataclass::enforce_types(level)
}

bar <- function(x = int(1.2)) {
  dataclass::enforce_types()
}

testthat::test_that("Test runtime typing", {

  # Provide incorrect types but set level to warn
  testthat::expect_warning(
    foo(1.1, FALSE, NULL, "Hi", list(), level = "warn")
  )

  # Provide incorrect types so expect error
  testthat::expect_error(
    foo(1.1, FALSE, NULL, "Hi", list())
  )

  # Provide correct types which arent default
  testthat::expect_no_error(
    foo(2L, "", FALSE, 1.23, mtcars)
  )

  # Use defaults which have correct types
  testthat::expect_no_error(
    foo()
  )

  # Use defaults which have incorrect types so expect error
  testthat::expect_error(
    bar()
  )

  # Provide correct type despite default
  testthat::expect_no_error(
    bar(1L)
  )

})