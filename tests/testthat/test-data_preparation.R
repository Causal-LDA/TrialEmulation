# data_preparation ----
test_that("data_preparation works as expected", {
  data("trial_example")
  result <- data_preparation(
    data = trial_example,
    id = "id",
    period = "period",
    eligible = "eligible",
    treatment = "treatment",
    outcome = "outcome",
    model_var = "assigned_treatment",
    outcome_cov = c("catvarA", "catvarB", "catvarC", "nvarA", "nvarB", "nvarC")
  )

  expect_identical(result$N, 1939053L)
  expect_identical(result$range, 396)
  expect_identical(result$min_period, 1L)
  expect_identical(result$max_period, 396L)
  expect(nrow(result$data), result$N)

  result_pat_1 <- as.data.frame(result$data[result$data$id == 1, ])
  expected_pat_1 <- vignette_switch_data[vignette_switch_data$id == 1, ]
  expect_equal(result_pat_1, expected_pat_1)
})

# data_preparation ----
test_that("data_preparation can be quiet", {
  expect_silent(
    result <- data_preparation(
      data = trial_example,
      id = "id",
      period = "period",
      eligible = "eligible",
      treatment = "treatment",
      outcome = "outcome",
      model_var = "assigned_treatment",
      outcome_cov = "catvarA",
      first_period = 1,
      last_period = 5,
      quiet = TRUE
    )
  )
})
