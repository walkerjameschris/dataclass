test_list_class <- dataclass::dataclass(
  dte_anyl = dataclass::dte_vec(level = sample(c("warn", "error"), 1)),
  dte_et_1 = dataclass::dte_vec(1, level = sample(c("warn", "error"), 1)),
  dte_gt_1 = dataclass::dte_vec(Inf, 2, level = sample(c("warn", "error"), 1)),
  atm_anyl = dataclass::atm_vec(level = sample(c("warn", "error"), 1)),
  atm_et_1 = dataclass::atm_vec(1, level = sample(c("warn", "error"), 1)),
  atm_gt_1 = dataclass::atm_vec(Inf, 2, level = sample(c("warn", "error"), 1)),
  num_anyl = dataclass::num_vec(level = sample(c("warn", "error"), 1)),
  num_et_1 = dataclass::num_vec(1, level = sample(c("warn", "error"), 1)),
  num_gt_1 = dataclass::num_vec(Inf, 2, level = sample(c("warn", "error"), 1)),
  chr_anyl = dataclass::chr_vec(level = sample(c("warn", "error"), 1)),
  chr_et_1 = dataclass::chr_vec(1, level = sample(c("warn", "error"), 1)),
  chr_gt_1 = dataclass::chr_vec(Inf, 2, level = sample(c("warn", "error"), 1)),
  lgl_anyl = dataclass::lgl_vec(level = sample(c("warn", "error"), 1)),
  lgl_et_1 = dataclass::lgl_vec(1, level = sample(c("warn", "error"), 1)),
  lgl_gt_1 = dataclass::lgl_vec(Inf, 2, level = sample(c("warn", "error"), 1)),
  dfl_objt = dataclass::df_like(level = sample(c("warn", "error"), 1)),
  any_objt = dataclass::any_obj()
)

test_list_out <- list(
  dte_anyl = rep(as.Date("2022-01-01"), sample.int(100, 1)),
  dte_et_1 = as.Date("2022-01-01"),
  dte_gt_1 = as.Date(c("2022-01-01", Sys.Date())),
  atm_anyl = rep(TRUE, sample.int(100, 1)),
  atm_et_1 = "A note!",
  atm_gt_1 = c(1, 2, 3, 4, 5),
  num_anyl = rep(1, sample.int(100, 1)),
  num_et_1 = 3.14159,
  num_gt_1 = seq(0, 10, 0.1),
  chr_anyl = rep("c", sample.int(100, 1)),
  chr_et_1 = "chhsdc has xhdapdksmcpe",
  chr_gt_1 = state.abb,
  lgl_anyl = rep(TRUE, sample.int(100, 1)),
  lgl_et_1 = FALSE,
  lgl_gt_1 = (seq(10) %% 2) == 1,
  dfl_objt = tibble::tibble(col = rep(1, sample.int(100, 1))),
  any_objt = list(mtcars, lm(vs ~ am, mtcars), list(x = sample.int(10)))
)

test_df_class <-
  dataclass::dataclass(
    dte_col = dataclass::dte_vec(level = sample(c("warn", "error"), 1)),
    atm_col = dataclass::atm_vec(level = sample(c("warn", "error"), 1)),
    num_col = dataclass::num_vec(level = sample(c("warn", "error"), 1)),
    chr_col = dataclass::chr_vec(level = sample(c("warn", "error"), 1)),
    lgl_col = dataclass::lgl_vec(level = sample(c("warn", "error"), 1))
  ) |>
  dataclass::data_validator()

test_df_class_bypass <-
  dataclass::dataclass(
    dte_col = dataclass::dte_vec()
  ) |>
  dataclass::data_validator(
    strict_cols = FALSE
  )

test_tibble_out <- tibble::tibble(
  dte_col = as.Date(c("2022-01-01", "2023-01-01")),
  atm_col = c(1, 2),
  num_col = c(1.02, 2.32),
  chr_col = c("a", "b"),
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

  # Bypass column checks
  testthat::expect_identical(
    test_df_class_bypass(test_tibble_out),
    test_tibble_out
  )
})
