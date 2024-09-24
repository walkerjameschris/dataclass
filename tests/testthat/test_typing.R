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

  testthat::expect_warning(
    foo(1.1, FALSE, NULL, "Hi", list(), level = "warn")
  )

  testthat::expect_error(
    foo(1.1, FALSE, NULL, "Hi", list())
  )

  testthat::expect_no_error(
    foo(2L, "", FALSE, 1.23, mtcars)
  )

  testthat::expect_no_error(
    foo()
  )

  testthat::expect_error(
    bar()
  )

})