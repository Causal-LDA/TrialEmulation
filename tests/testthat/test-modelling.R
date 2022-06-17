# data_modelling ----
test_that("data_modelling can be quiet", {
  working_dir <- tempdir(check = TRUE)
  data_path <- file.path(working_dir, "switch_data.R")
  data.table::fwrite(vignette_switch_data, data_path)
  expect_silent(
    result <- data_modelling(
      outcomeCov_var = c("catvarA", "catvarB", "catvarC", "nvarA", "nvarB", "nvarC"),
      model_var = "assigned_treatment",
      include_followup_time_case = "linear",
      include_expansion_time_case = "linear",
      absolutePath = data_path,
      use_sample_weights = FALSE,
      quiet = TRUE
    )
  )
  unlink(data_path)
})

test_that("data_modelling gives expected results in example data", {
  working_dir <- tempdir(check = TRUE)
  data_path <- file.path(working_dir, "switch_data.csv")
  data.table::fwrite(vignette_switch_data, data_path)
  result <- data_modelling(
    outcomeCov_var = c("catvarA", "catvarB", "catvarC", "nvarA", "nvarB", "nvarC"),
    model_var = "assigned_treatment",
    include_followup_time_case = "linear",
    include_expansion_time_case = "linear",
    absolutePath = data_path,
    use_sample_weights = FALSE,
    quiet = TRUE
  )
  unlink(data_path)
  expect_class(result$model, "glm")
  expected_coefs <- c(
    `(Intercept)` = -6.17184262601338, assigned_treatment = -0.118265495621331,
    for_period = 0.00325072039757488, followup_time = 0.00168035277357471
  )
  expect_equal(result$model$coefficients, expected_coefs)

  expect_equal(
    result$robust$summary$names,
    c("(Intercept)", "assigned_treatment", "for_period", "followup_time")
  )
  expected_robust_se <- c(0.34731920891695, 0.293437149718051, 0.00130047600033232, 0.00155479427447236)
  expect_equal(result$robust$summary$robust_se, expected_robust_se)

  expect_matrix(result$robust$matrix, nrows = 4, ncols = 4, any.missing = FALSE)
})
