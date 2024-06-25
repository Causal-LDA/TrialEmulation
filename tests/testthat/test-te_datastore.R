test_that("read_expanded_data can read te_datastore_csv data", {
  temp_dir <- tempfile("csv_dir_")
  dir.create(temp_dir)
  datastore <- save_to_csv(temp_dir)
  expanded_csv_data <- save_expanded_data(datastore, subset(vignette_switch_data, trial_period %in% 1:12))

  # check if no columns get added or removed by read_expanded_data
  expect_equal(ncol(read_expanded_data(expanded_csv_data)), length(expanded_csv_data@template))

  # check if omitting period reads in all data
  expect_equal(nrow(read_expanded_data(expanded_csv_data)), 25948)

  # check if period argument subsets data correctly
  expect_equal(nrow(read_expanded_data(expanded_csv_data, 1)), 1979)
  expect_equal(nrow(read_expanded_data(expanded_csv_data, c(5, 8, 12))), 6493)

  # check if method throws an error when using a character as period
  expect_error(read_expanded_data(expanded_csv_data, "1"))

  # check if no new NAs are introduced
  expect_equal(
    sum(is.na.data.frame(read_expanded_data(expanded_csv_data))),
    sum(is.na.data.frame(subset(vignette_switch_data, trial_period %in% 1:12)))
  )

  # check if subset_condition subsets data correctly
  expect_equal(
    nrow(read_expanded_data(expanded_csv_data, subset_condition = "followup_time <= 30 & nvarC == 40")), 371
  )

  unlink(temp_dir, recursive = TRUE)
})


test_that("read_expanded_data can read te_datastore_datatable data", {
  trial_to_expand <- trial_sequence("ITT") |>
    set_data(data = data_censored) |>
    set_outcome_model(adjustment_terms = ~age) |>
    set_expansion_options(output = save_to_datatable(), chunk_size = 500)

  expanded_datatable_data <- expand_trials(trial_to_expand)@expansion@datastore

  # check if no columns get added or removed by read_expanded_data
  expect_equal(ncol(read_expanded_data(expanded_datatable_data)), ncol(expanded_datatable_data@data))

  # check if omitting period reads in all data
  expect_equal(nrow(read_expanded_data(expanded_datatable_data)), 1558)

  # check if period argument subsets data correctly
  expect_equal(nrow(read_expanded_data(expanded_datatable_data, period = 1)), 327)
  expect_equal(nrow(read_expanded_data(expanded_datatable_data, period = c(5, 8, 12))), 63)

  # check if method throws an error when using a character as period
  expect_error(read_expanded_data(expanded_datatable_data, period = "1"), "period")

  # check if no new NAs are introduced
  expect_equal(
    sum(is.na.data.frame(read_expanded_data(expanded_datatable_data))),
    sum(is.na.data.frame(expanded_datatable_data@data))
  )

  # check if subset_condition subsets data correctly
  expect_equal(
    nrow(read_expanded_data(expanded_datatable_data, subset_condition = "followup_time <= 5 | age < 40")),
    945
  )
})


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
