test_that("data_manipulation works as expected with no censoring", {
  object <- as.data.table(trial_example)
  result <- data_manipulation(object, use_censor = 0)

  expect_data_table(
    result,
    key = "id",
    nrow = 48400,
    ncol = 20
  )
  expect_set_equal(
    colnames(result),
    c(
      "id", "eligible", "period", "outcome", "treatment", "catvarA",
      "catvarB", "catvarC", "nvarA", "nvarB", "nvarC",
      "time_of_event", "first", "am_1", "cumA", "switch",
      "regime_start", "time_on_regime", "eligible0", "eligible1"
    )
  )
  expect_equal(length(unique(result$id)), 503L)
})

test_that("data_manipulation works as expected with censoring", {
  object <- as.data.table(trial_example)
  result <- data_manipulation(object, use_censor = 1)

  expect_data_table(
    result,
    key = "id",
    nrow = 38734,
    ncol = 20
  )
  expect_set_equal(
    colnames(result),
    c(
      "id", "eligible", "period", "outcome", "treatment", "catvarA",
      "catvarB", "catvarC", "nvarA", "nvarB", "nvarC",
      "time_of_event", "first", "am_1", "cumA", "switch",
      "regime_start", "time_on_regime", "eligible0",
      "eligible1"
    )
  )
  expect_equal(length(unique(result$id)), 503L)
})
