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


test_that("data_preparation gives an error for existing trial files", {
  save_dir <- withr::local_tempdir(pattern = "duplicates", tempdir(TRUE))

  write.csv(RandomisedTrialsEmulation::vignette_switch_data[1:10, ], file = file.path(save_dir, "trial_1.csv"))

  expect_error(
    data_preparation(
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
      quiet = TRUE,
      separate_files = TRUE,
      data_dir = save_dir
    ),
    "files already exist in"
  )
})


test_that("data_preparation gives a warning for existing model files", {
  save_dir <- withr::local_tempdir(pattern = "duplicates", tempdir(TRUE))

  saveRDS(list(data = "dummy data"), file = file.path(save_dir, "cense_model_n0.rds"))

  expect_warning(
    data_preparation(
      data = trial_example[1:10, ],
      id = "id",
      period = "period",
      eligible = "eligible",
      treatment = "treatment",
      outcome = "outcome",
      model_var = "assigned_treatment",
      outcome_cov = "catvarA",
      first_period = 1,
      last_period = 5,
      quiet = TRUE,
      separate_files = TRUE,
      data_dir = save_dir
    ),
    "contains model rds files. These may be overwritten."
  )
})
