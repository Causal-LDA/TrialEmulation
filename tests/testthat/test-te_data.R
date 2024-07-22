test_that("te_outcome_data constructor works", {
  data <- readRDS(test_path("data/ready_for_modelling.rds"))
  result <- te_outcome_data(data)
  expect_class(result, "te_outcome_data")
  expect_equal(result@n_rows, 1041L)
  expect_equal(result@n_ids, 181L)
  expect_equal(result@periods, c(2, 3, 4, 5, 6, 7, 8))
  expect_data_table(result@data, ncols = 13, nrows = 1041)
  expect_equal(result@p_control, numeric())
  expect_equal(result@subset_condition, character())
})

test_that("te_outcome_data constructor fails on bad input", {
  expect_error(te_outcome_data(iris), "data.table")
  expect_error(te_outcome_data(as.data.table(iris)), "colnames")
  expect_warning(te_outcome_data(
    data.table(
      id = numeric(),
      trial_period = numeric(),
      followup_time = numeric(),
      outcome = numeric(),
      weight = numeric()
    )
  ))
})
