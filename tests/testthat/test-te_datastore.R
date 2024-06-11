test_that("read_expanded_data can read te_datastore_csv data", {
  temp_dir <- tempfile("csv_dir_")
  dir.create(temp_dir)
  datastore <- save_to_csv(temp_dir)
  data(vignette_switch_data)
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

  unlink(temp_dir, recursive = TRUE)
})


test_that("read_expanded_data can read te_datastore_datatable data", {
  trial_to_expand <- trial_sequence("ITT") |>
    set_data(data = data_censored) |>
    set_expansion_options(output = save_to_datatable(), chunk_size = 500)
  expanded_datatable_data <- save_expanded_data(trial_to_expand@expansion@datastore, trial_to_expand@data@data)

  # check if no columns get added or removed by read_expanded_data
  expect_equal(ncol(read_expanded_data(expanded_datatable_data)), ncol(trial_to_expand@data@data))

  # check if omitting period reads in all data
  expect_equal(nrow(read_expanded_data(expanded_datatable_data)), 725)

  # check if period argument subsets data correctly
  expect_equal(nrow(read_expanded_data(expanded_datatable_data, 1)), 62)
  expect_equal(nrow(read_expanded_data(expanded_datatable_data, c(5, 8, 12))), 99)

  # check if method throws an error when using a character as period
  expect_error(read_expanded_data(expanded_datatable_data, "1"))

  # check if no new NAs are introduced
  expect_equal(
    sum(is.na.data.frame(read_expanded_data(expanded_datatable_data))),
    sum(is.na.data.frame(trial_to_expand@data@data))
  )
})


test_that("read_expanded_data can read te_datastore_duckdb data", {
  temp_dir <- tempfile("duckdb_dir_")
  dir.create(temp_dir)
  datastore <- save_to_duckdb(temp_dir)
  data(vignette_switch_data)
  expanded_duckdb_data <- save_expanded_data(datastore, subset(vignette_switch_data, trial_period %in% 1:12))

  # check if no columns get added or removed by read_expanded_data
  expect_equal(ncol(read_expanded_data(expanded_duckdb_data)), ncol(vignette_switch_data))

  # check if omitting period reads in all data
  expect_equal(nrow(read_expanded_data(expanded_duckdb_data)), 25948)

  # check if period argument subsets data correctly
  expect_equal(nrow(read_expanded_data(expanded_duckdb_data, 1)), 1979)
  expect_equal(nrow(read_expanded_data(expanded_duckdb_data, c(5, 8, 12))), 6493)

  # check if method throws an error when using a character as period
  expect_error(read_expanded_data(expanded_csv_data, "1"))

  # check if no new NAs are introduced
  expect_equal(
    sum(is.na.data.frame(read_expanded_data(expanded_duckdb_data))),
    sum(is.na.data.frame(subset(vignette_switch_data, trial_period %in% 1:12)))
  )

  DBI::dbDisconnect(expanded_duckdb_data@con)
  unlink(temp_dir, recursive = TRUE)
})
