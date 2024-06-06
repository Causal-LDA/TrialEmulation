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
  expect_equal(sum(is.na.data.frame(read_expanded_data(expanded_csv_data))),
               sum(is.na.data.frame(subset(vignette_switch_data, trial_period %in% 1:12))))

  unlink(temp_dir, recursive = TRUE)
})
