test_list_class <- dataclass::dataclass(
  dte_anyl = dataclass::dte_vec(),
  dte_et_1 = dataclass::dte_vec(1),
  dte_gt_1 = dataclass::dte_vec(Inf, 2),
  atm_anyl = dataclass::atm_vec(),
  atm_et_1 = dataclass::atm_vec(1),
  atm_gt_1 = dataclass::atm_vec(Inf, 2),
  num_anyl = dataclass::num_vec(),
  num_et_1 = dataclass::num_vec(1),
  num_gt_1 = dataclass::num_vec(Inf, 2),
  lgl_anyl = dataclass::lgl_vec(),
  lgl_et_1 = dataclass::lgl_vec(1),
  lgl_gt_1 = dataclass::lgl_vec(Inf, 2),
  dfl_anyl = dataclass::df_like(),
  dfl_et_1 = dataclass::df_like(1),
  dfl_gt_1 = dataclass::df_like(Inf, 2),
  any_objt = dataclass::any_obj() 
)

test_list_out <- list(
  dte_anyl = as.Date("2022-01-01"),
  dte_et_1 = as.Date("2022-01-01"),
  dte_gt_1 = as.Date(c("2022-01-01", Sys.Date())),
  atm_anyl = TRUE,
  atm_et_1 = "A note!",
  atm_gt_1 = c(1, 2, 3, 4, 5),
  num_anyl = 1,
  num_et_1 = 3.14159,
  num_gt_1 = seq(0, 10, 0.1),
  lgl_anyl = TRUE,
  lgl_et_1 = FALSE,
  lgl_gt_1 = (seq(10) %% 2) == 1,
  dfl_anyl = tibble::tibble(col = 1),
  dfl_et_1 = data.frame(col = "a"),
  dfl_gt_1 = data.table::data.table(col_1 = c(1, 2), col_2 = c("a", "b")),
  any_objt = list(mtcars, lm(vs ~ am, mtcars), list(x = 2))
)

test_df_class <-
  dataclass::dataclass(
    dte_col = dataclass::dte_vec(),
    atm_col = dataclass::atm_vec(),
    num_col = dataclass::num_vec(),
    lgl_col = dataclass::lgl_vec()
  ) |>
  dataclass::data_validator()

test_tibble_out <- tibble::tibble(
  dte_col = as.Date(c("2022-01-01", "2023-01-01")),
  atm_col = c(1, 2),
  num_col = c(1.02, 2.32),
  lgl_col = c(TRUE, FALSE)
)

test_df_out <- as.data.frame(test_tibble_out)
test_dt_out <- data.table::as.data.table(test_tibble_out)

testthat::test_that("Happy path dataclass() for list", {
  
  # Test list formats
  testthat::expect_identical(
    do.call(test_list_class, test_list_out),
    test_list_out
  )
})

testthat::test_that("Happy path dataclass() for rectangular data", {
  
  # Test tibble format
  testthat::expect_identical(
    test_df_class(test_tibble_out),
    test_tibble_out
  )
  
  # Test data frame format
  testthat::expect_identical(
    test_df_class(test_df_out) |>
      as.data.frame(),
    test_df_out
  )
  
  # Test data table format
  testthat::expect_identical(
    test_df_class(test_dt_out) |>
      data.table::as.data.table(),
    test_dt_out
  )
})
