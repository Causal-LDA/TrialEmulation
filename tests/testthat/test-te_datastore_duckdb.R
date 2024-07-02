test_that("read_expanded_data can read te_datastore_duckdb data", {
  temp_dir <- withr::local_tempdir(pattern = "duckdb_dir", tempdir(TRUE))
  data <- data_censored
  data$id <- as.factor(data$id)

  trial_to_expand <- trial_sequence("ITT") |>
    set_data(data = data) |>
    set_outcome_model(adjustment_terms = ~age) |>
    set_expansion_options(output = save_to_duckdb(temp_dir), chunk_size = 500)

  expanded_duckdb_data <- expand_trials(trial_to_expand)@expansion@datastore


  # check if no columns get added or removed by read_expanded_data
  expect_equal(ncol(read_expanded_data(expanded_duckdb_data)), 8)

  # check if omitting period reads in all data
  expect_equal(nrow(read_expanded_data(expanded_duckdb_data)), 1558)

  # check if period argument subsets data correctly
  expect_equal(nrow(read_expanded_data(expanded_duckdb_data, period = 1)), 327)
  expect_equal(nrow(read_expanded_data(expanded_duckdb_data, period = c(5, 8, 12))), 63)

  # check if method throws an error when using a character as period
  expect_error(read_expanded_data(expanded_duckdb_data, period = "1"), "period")

  # check if no new NAs are introduced
  expect_equal(sum(is.na.data.frame(read_expanded_data(expanded_duckdb_data))), 1469)

  # check if factor variables are kept
  expect_factor(read_expanded_data(expanded_duckdb_data)$id)

  # check if subset_condition subsets data correctly
  expect_equal(
    nrow(read_expanded_data(expanded_duckdb_data, subset_condition = "followup_time <= 5 | age < 40")),
    945
  )

  DBI::dbDisconnect(expanded_duckdb_data@con)
})


test_that("sample_controls works with trial_sequence objects containing te_datastore_duckdb objects", {
  trial_itt_dir <- withr::local_tempdir("trial_itt", tempdir(TRUE))

  trial_itt <- trial_sequence(estimand = "ITT") |>
    set_data(data = data_censored) |>
    set_outcome_model(adjustment_terms = ~ x1 + x2)

  trial_itt_duckdb <- set_expansion_options(
    trial_itt,
    output = save_to_duckdb(file.path(trial_itt_dir, "trial_duckdb")),
    chunk_size = 500
  ) |>
    expand_trials()


  # sample_controls works without additional arguments
  sc_01 <- sample_controls(trial_itt_duckdb, p_control = 0.01, seed = 1221)
  expect_equal(
    sort(sc_01$id),
    c(
      1, 10, 13, 14, 15, 21, 27, 29, 32, 38, 38, 40, 44, 44, 44, 44,
      49, 49, 58, 61, 68, 71, 71, 74, 84, 89, 95, 95, 95, 98, 99
    )
  )

  # sample_controls works with p_control
  sc_02 <- sample_controls(trial_itt_duckdb, p_control = 0.5, seed = 5678)
  expect_equal(nrow(sc_02), 756)

  # sample_controls works with p_control = 0
  sc_03 <- sample_controls(trial_itt_duckdb, p_control = 0)
  expect_equal(nrow(sc_03), 14)

  # cases are kept
  expect_equal(sum(sc_01$outcome), 14)
  expect_equal(sum(sc_02$outcome), 14)
  expect_equal(sum(sc_03$outcome), 14)

  # all columns are kept and sample_weight column is added
  expect_equal(colnames(sc_01), c(colnames(read_expanded_data(trial_itt_duckdb@expansion@datastore)), "sample_weight"))

  # sample_weight calculated correctly
  expect_equal(sort(sc_01$sample_weight), c(rep(1, 14), rep(100, 17)))
  expect_equal(sort(sc_02$sample_weight), c(rep(1, 14), rep(2, 742)))

  # sample_controls subsets data correctly
  sc_04 <- sample_controls(
    trial_itt_duckdb,
    period = 1:20,
    subset_condition = "x2 <= 1 & treatment == 1 & (id %in% 40:90 | followup_time %in% c(2, 3, 4, 5, 6))",
    p_control = 0.2,
    seed = 2332
  )
  expect_equal(
    sort(sc_04$id),
    c(
      21, 27, 34, 34, 44, 44, 44, 44, 44, 44, 44, 53, 53, 54, 59,
      59, 59, 59, 60, 65, 65, 70, 71, 73, 74, 74, 74, 83, 95
    )
  )

  # sample_controls returns a data.table
  expect_class(sc_04, "data.table")

  DBI::dbDisconnect(trial_itt_duckdb@expansion@datastore@con)
})


test_that("translate_to_sql works as intended", {
  input_string <- "a == 1 & b >= 0 & c < 10 & d != -1 & (e %in% 1:10 | f %in% c(4, 7, 9)"
  output_string <- translate_to_sql(input_string)
  expect_equal(
    output_string,
    "a = 1 AND b >= 0 AND c < 10 AND d != -1 AND (e IN (1, 2, 3, 4, 5, 6, 7, 8, 9, 10) OR f IN (4, 7, 9)"
  )
})

test_that("translate_to_sql catches the error and provides a message", {
  input_string <- "(e%in%1:10|f%in%c(4, 7, 9)"
  expect_warning(expect_error(
    translate_to_sql(input_string),
    "Error translating"
  ))
})
