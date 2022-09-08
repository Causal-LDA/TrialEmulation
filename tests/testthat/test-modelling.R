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
    outcomeCov = c("catvarA", "catvarB", "catvarC", "nvarA", "nvarB", "nvarC"),
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
    `(Intercept)` = -3.44513044265409,
    assigned_treatment = -0.279511046771487,
    for_period = 0.00193709391469456,
    followup_time = 0.00139579193160405,
    catvarA = 0.0586518690748273,
    catvarB = -0.0545402601125559,
    catvarC = -0.0209619578642913,
    nvarA = -0.0794050643431851,
    nvarB = 0.00477012273880385,
    nvarC = -0.0404651039259053
  )
  expect_equal(result$model$coefficients, expected_coefs)

  expect_equal(
    result$robust$summary$names,
    c(
      "(Intercept)", "assigned_treatment", "for_period", "followup_time",
      "catvarA", "catvarB", "catvarC", "nvarA", "nvarB", "nvarC"
    )
  )
  expected_robust_se <- c(
    0.608156159520476, 0.309527599563302, 0.00159274700265935,
    0.0015016748931986, 0.0312208855654385, 0.0296048600610076, 0.0263579673428329,
    0.0447759173905258, 0.00231602984314177, 0.00671869054658421
  )
  expect_equal(result$robust$summary$robust_se, expected_robust_se)

  expect_matrix(result$robust$matrix, nrows = 10, ncols = 10, any.missing = FALSE)
})
