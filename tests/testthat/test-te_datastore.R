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
