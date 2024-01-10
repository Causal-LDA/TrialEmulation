test_that("data_manipulation works as expected with no censoring", {
  object <- as.data.table(trial_example)
  result <- data_manipulation(object, use_censor = FALSE)

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
  result <- data_manipulation(object, use_censor = TRUE)

  expect_data_table(
    result,
    key = "id",
    nrow = 38820,
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

test_that("data_manipulation works as expected with observations before eligibilitiy", {
  object <- as.data.table(trial_example)[id %in% 1:3]
  object <- object[id == 1 & period == 261, eligible := 0]
  expect_warning(
    result <- data_manipulation(object, use_censor = TRUE),
    "before trial eligibility"
  )

  expect_data_table(
    result,
    key = "id",
    nrow = 255,
    ncol = 20
  )
  expect_equal(min(result[id == 1, ]$period), 262)
})

test_that("data_manipulation works as expected with observations after outcome", {
  object <- as.data.table(trial_example)[id %in% 1:3]
  object <- object[id == 1 & period == 350, outcome := 1]
  expect_warning(
    result <- data_manipulation(object, use_censor = TRUE),
    "after the outcome occured"
  )

  expect_data_table(
    result,
    key = "id",
    nrow = 210,
    ncol = 20
  )
  expect_equal(max(result[id == 1, ]$period), 350)
})
