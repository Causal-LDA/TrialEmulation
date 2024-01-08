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
    outcome_cov = c("catvarA", "catvarB", "catvarC", "nvarA", "nvarB", "nvarC"),
    estimand_type = "ITT"
  )

  expect_identical(result$N, 1939053L)
  expect_identical(result$min_period, 1L)
  expect_identical(result$max_period, 396L)
  expect(nrow(result$data), result$N)

  result_pat_1 <- as.data.frame(result$data[result$data$id == 1, ])
  expected_pat_1 <- vignette_switch_data[vignette_switch_data$id == 1, ]
  expect_equal(result_pat_1, expected_pat_1)
})

test_that("data_preparation can be quiet", {
  expect_silent(
    result <- data_preparation(
      data = trial_example,
      id = "id",
      period = "period",
      eligible = "eligible",
      treatment = "treatment",
      outcome = "outcome",
      outcome_cov = "catvarA",
      first_period = 1,
      last_period = 5,
      quiet = TRUE,
      estimand_type = "ITT"
    )
  )
})


test_that("data_preparation gives an error for existing trial files", {
  save_dir <- withr::local_tempdir(pattern = "duplicates", tempdir(TRUE))

  write.csv(TrialEmulation::vignette_switch_data[1:10, ], file = file.path(save_dir, "trial_1.csv"))

  expect_error(
    data_preparation(
      data = trial_example,
      id = "id",
      period = "period",
      eligible = "eligible",
      treatment = "treatment",
      outcome = "outcome",
      estimand_type = "ITT",
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


test_that("check_data_dir gives a warning for existing model files", {
  save_dir <- withr::local_tempdir(pattern = "duplicates", tempdir(TRUE))
  saveRDS(list(data = "dummy data"), file = file.path(save_dir, "cense_model_n0.rds"))

  expect_warning(
    check_data_dir(data_dir = save_dir),
    "contains model rds files. These may be overwritten."
  )
})

test_that("data_preparation has correct values for 'treatment'", {
  set.seed(2002211011)
  simdata_censored <- data_gen_censored(1000, 10)
  prep_PP_data <- data_preparation(
    data = simdata_censored,
    id = "ID",
    period = "t",
    treatment = "A",
    outcome = "Y",
    eligible = "eligible",
    outcome_cov = ~X1,
    estimand_type = "ITT",
    separate_files = FALSE,
    quiet = TRUE
  )

  prep_PP_data$data[, t := trial_period + followup_time]
  compare <- merge(
    x = prep_PP_data$data[, c("id", "t", "treatment", "outcome")],
    y = simdata_censored[, c("ID", "t", "A", "Y")],
    by.x = c("id", "t"),
    by.y = c("ID", "t")
  )
  expect_equal(compare$treatment, compare$A)
  expect_equal(compare$outcome, compare$Y)
})


test_that("data_preparation works with PP estimand type", {
  data("trial_example")
  set.seed(1)
  result <- data_preparation(
    data = trial_example,
    id = "id",
    period = "period",
    eligible = "eligible",
    treatment = "treatment",
    outcome = "outcome",
    outcome_cov = c("catvarA", "catvarB", "catvarC", "nvarA", "nvarB", "nvarC"),
    estimand_type = "PP",
    use_censor_weights = FALSE
  )

  expect_identical(result$N, 963883L)
  expect_identical(result$min_period, 1L)
  expect_identical(result$max_period, 396L)
  expect(nrow(result$data), result$N)
})


test_that("data_preparation works with As-Treated estimand type", {
  data("trial_example")
  set.seed(1)
  result <- data_preparation(
    data = trial_example,
    id = "id",
    period = "period",
    eligible = "eligible",
    treatment = "treatment",
    outcome = "outcome",
    outcome_cov = c("catvarA", "catvarB", "catvarC", "nvarA", "nvarB", "nvarC"),
    estimand_type = "As-Treated",
    use_censor_weights = FALSE,
    pool_cense = "none"
  )

  expect_identical(result$N, 1939053L)
  expect_identical(result$min_period, 1L)
  expect_identical(result$max_period, 396L)
  expect(nrow(result$data), result$N)
})


test_that("data_preparation works with ITT and censor weights", {
  set.seed(2002211011)
  simdata_censored <- data_gen_censored(1000, 10)
  result <- data_preparation(
    data = simdata_censored,
    id = "ID", period = "t", treatment = "A",
    outcome = "Y", eligible = "eligible",
    estimand_type = "ITT",
    outcome_cov = ~ X1 + X2 + X3 + X4 + age_s,
    model_var = "assigned_treatment",
    use_censor_weights = TRUE,
    cense = "C", cense_d_cov = ~ X1 + X2 + X3 + X4 + age_s,
    cense_n_cov = ~ X3 + X4,
    pool_cense = "both",
    save_weight_models = FALSE,
    glm_function = "parglm", nthreads = 2, method = "FAST",
    quiet = TRUE
  )

  expect_identical(result$N, 8795L)
  expect_identical(result$min_period, 0L)
  expect_identical(result$max_period, 9L)
  expect(nrow(result$data), result$N)
  expect_equal(
    result$censor_models$cens_pool_d$summary$estimate,
    c(1.37911964407242, 0.331586878535157, -0.582701095754271, 0.29591740519054, -0.0725139253274435, 0.94770056528085)
  )
  expect_equal(
    result$censor_models$cens_pool_n$summary$estimate,
    c(1.76997547565826, 0.307118590668955, -0.0870927737983157)
  )
})
