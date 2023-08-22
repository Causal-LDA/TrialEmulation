test_that("do_sampling works as expected when case exist", {
  data <- as.data.table(TrialEmulation::vignette_switch_data)[trial_period == 272 & followup_time == 5]
  set.seed(100)
  sample1 <- do_sampling(data, p_control = 0.1)
  expect_snapshot_value(as.data.frame(sample1), style = "json2")

  sample01 <- do_sampling(data, p_control = 0.01)
  expect_snapshot_value(as.data.frame(sample01), style = "json2")
})

test_that("do_sampling works as expected when no cases exist", {
  data <- as.data.table(TrialEmulation::vignette_switch_data)[trial_period == 5 & followup_time == 5]
  set.seed(1100)
  result_rows <- do_sampling(data, p_control = 0.1)
  expect_snapshot_value(as.data.frame(result_rows), style = "json2")
})

test_that("sample_from_period works as expected", {
  data <- as.data.table(TrialEmulation::vignette_switch_data)[trial_period == 272]
  set.seed(651)

  result <- sample_from_period(period_data = data, p_control = 0.01, use_subset = FALSE)

  expect_data_frame(result, nrow = 87, ncol = 15)
  expect_identical(unique(result$sample_id), 1L)

  result_cases <- result[outcome == 1, c("id", "trial_period", "followup_time")]
  expected_cases <- data[outcome == 1, c("id", "trial_period", "followup_time")]
  expect_equal(
    result_cases[order(result_cases$followup_time, result_cases$id)],
    expected_cases[order(expected_cases$followup_time, expected_cases$id)]
  )
  expect_data_frame(result[followup_time == 2], nrows = 1)
  expect_snapshot_value(
    as.data.frame(result[outcome == 0, c("id", "trial_period", "followup_time")]),
    style = "json2"
  )
})

test_that("sample_from_period works as expected with multiple proportions", {
  data <- as.data.table(TrialEmulation::vignette_switch_data)[trial_period == 272]
  set.seed(209)
  result <- sample_from_period(
    period_data = data,
    p_control = c(0.01, 0.05),
    use_subset = FALSE
  )

  expect_data_frame(result, nrow = 338, ncol = 15)
  expect_identical(unique(result$sample_id), c(1L, 2L))
  expect_data_frame(result[sample_id == 1], nrow = 87, ncol = 15)
  expect_data_frame(result[sample_id == 2], nrow = 251, ncol = 15)

  expect_snapshot_value(as.data.frame(result[1:30, ]), style = "json2")

  result_cases_1 <- result[outcome == 1 & sample_id == 1, c("id", "trial_period", "followup_time")]
  expected_cases <- data[outcome == 1, c("id", "trial_period", "followup_time")]
  expect_equal(
    result_cases_1[order(result_cases_1$followup_time, result_cases_1$id)],
    expected_cases[order(expected_cases$followup_time, expected_cases$id)]
  )

  result_cases_2 <- result[outcome == 1 & sample_id == 2, c("id", "trial_period", "followup_time")]
  expect_equal(
    result_cases_2[order(result_cases_2$followup_time, result_cases_2$id)],
    expected_cases[order(expected_cases$followup_time, expected_cases$id)]
  )
})

test_that("case_control_sampling_trials works with separate_files = TRUE", {
  set.seed(1001)
  save_dir <- withr::local_tempdir(pattern = "sampling", tempdir(TRUE))
  dat <- trial_example[trial_example$id < 200, ]
  expanded_data <- data_preparation(
    data = dat,
    data_dir = save_dir,
    outcome_cov = c("nvarA", "nvarB", "nvarC"),
    first_period = 260,
    last_period = 280,
    separate_files = TRUE,
    quiet = TRUE
  )
  samples <- case_control_sampling_trials(expanded_data, p_control = 0.01)
  expect_data_frame(samples, nrow = 714, ncol = 11)
  expect_snapshot_value(as.data.frame(samples[1:30, ]), style = "json2")
})

test_that("case_control_sampling_trials works with separate_files = FALSE", {
  set.seed(1001)
  dat <- trial_example[trial_example$id < 200, ]
  expanded_data <- data_preparation(
    data = dat,
    outcome_cov = c("nvarA", "nvarB", "nvarC"),
    first_period = 260,
    last_period = 280,
    separate_files = FALSE,
    quiet = TRUE
  )
  samples <- case_control_sampling_trials(expanded_data, p_control = 0.01)
  expect_data_frame(samples, nrow = 714, ncol = 11)
  expect_snapshot_value(as.data.frame(samples[1:30, ]), style = "json2")
})


test_that("case_control_sampling_trials works with separate_files = TRUE is reproducible", {
  save_dir <- withr::local_tempdir(pattern = "sampling", tempdir(TRUE))
  dat <- trial_example[trial_example$id < 200, ]
  expanded_data <- data_preparation(
    data = dat,
    data_dir = save_dir,
    outcome_cov = c("nvarA", "nvarB", "nvarC"),
    first_period = 260,
    last_period = 280,
    separate_files = TRUE,
    quiet = TRUE
  )
  set.seed(2090)
  samples_1 <- case_control_sampling_trials(expanded_data, p_control = 0.01)

  set.seed(2090)
  samples_2 <- case_control_sampling_trials(expanded_data, p_control = 0.01)

  expect_identical(samples_1, samples_2)
})


test_that("case_control_sampling_trials works with subsetting", {
  save_dir <- withr::local_tempdir(pattern = "sampling", tempdir(TRUE))
  data("te_data_ex")
  set.seed(2090)
  samples <- case_control_sampling_trials(
    te_data_ex,
    p_control = 0.01,
    subset_condition = catvarA == 2
  )

  expect_true(all(samples$catvarA == 2))
})



test_that("case_control_sampling_trials gives errors for arguments", {
  expect_error(
    case_control_sampling_trials(
      trial_example,
      p_control = 0.01,
      subset_condition = nvarC > 75
    ),
    "Unknown data_prep object"
  )

  expect_error(
    case_control_sampling_trials(
      trial_example,
      p_control = "a",
      subset_condition = nvarC > 75
    ),
    "Must be of type 'numeric'"
  )
})


test_that("case_control_sampling_trials works with multiple p_control", {

  data("te_data_ex")
  set.seed(2090)
  samples <- case_control_sampling_trials(
    te_data_ex,
    p_control = c(0.01, 0.1)
  )
  expect_list(samples, types = "data.frame", len = 2)
  expect_data_frame(samples[[1]], nrow = 696, ncol = 10)
  expect_data_frame(samples[[2]], nrow = 5259, ncol = 10)
})


test_that("case_control_sampling_trials works with sort = TRUE", {
  skip_on_cran()
  save_dir <- withr::local_tempdir(pattern = "sampling", tempdir(TRUE))


  dat <- trial_example[trial_example$id < 200, ]

  expanded_data_t <- data_preparation(
    data = dat,
    data_dir = save_dir,
    outcome_cov = c("nvarA", "nvarB", "nvarC"),
    first_period = 260,
    last_period = 280,
    separate_files = TRUE,
    quiet = TRUE
  )

  expanded_data_f <- data_preparation(
    data = dat,
    data_dir = save_dir,
    outcome_cov = c("nvarA", "nvarB", "nvarC"),
    first_period = 260,
    last_period = 280,
    separate_files = FALSE,
    quiet = TRUE
  )
  set.seed(9999)
  samples_t <- case_control_sampling_trials(expanded_data_t, p_control = 0.01, sort = TRUE)

  set.seed(9999)
  samples_f <- case_control_sampling_trials(expanded_data_f, p_control = 0.01, sort = TRUE)

  expect_identical(samples_f, samples_t)
})
