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
    estimand_type = "ITT",
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
    estimand_type = "ITT",
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
    estimand_type = "ITT",
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
    estimand_type = "ITT",
    first_period = 260,
    last_period = 280,
    separate_files = TRUE,
    quiet = TRUE
  )

  expanded_data_f <- data_preparation(
    data = dat,
    data_dir = save_dir,
    outcome_cov = c("nvarA", "nvarB", "nvarC"),
    estimand_type = "ITT",
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


test_that("sample_controls works with trial_sequence objects containing te_datastore_csv objects", {
  trial_itt_dir <- file.path(tempdir(), "trial_itt")
  dir.create(trial_itt_dir)

  trial_itt <- trial_sequence(estimand = "ITT") |>
    set_data(
      data = data_censored,
      id = "id",
      period = "period",
      treatment = "treatment",
      outcome = "outcome",
      eligible = "eligible"
    ) |>
    set_censor_weight_model(
      censor_event = "censored",
      numerator = ~ x1 + x2 + x3,
      denominator = ~x2,
      pool_models = "numerator",
      model_fitter = stats_glm_logit(save_path = file.path(trial_itt_dir, "switch_models"))
    ) |>
    calculate_weights() |>
    set_outcome_model(adjustment_terms = ~ x1 + x2)

  trial_itt_csv <- set_expansion_options(
    trial_itt,
    output = save_to_csv(file.path(trial_itt_dir, "trial_csvs")),
    chunk_size = 500
  ) |>
    expand_trials()

  # sample_controls works without additional arguments
  sc_01 <- sample_controls(trial_itt_csv, p_control = 0.01, seed = 1221)
  expect_equal(
    sort(sc_01@outcome_data@data$id),
    c(
      10, 14, 14, 15, 17, 21, 27, 29, 32, 38, 38, 44, 44, 49, 49,
      54, 54, 54, 59, 61, 68, 71, 71, 71, 74, 74, 89, 98, 98, 99
    )
  )

  random_01 <- sample_controls(trial_itt_csv, p_control = 0.01)

  # seed gets reset
  sc_01_1 <- sample_controls(trial_itt_csv, p_control = 0.01, seed = 1221)
  random_02 <- sample_controls(trial_itt_csv, p_control = 0.01)
  expect_false(identical(sort(random_01@outcome_data@data$id), sort(random_02@outcome_data@data$id)))

  # sample_controls works with p_control
  sc_02 <- sample_controls(trial_itt_csv, p_control = 0.5, seed = 5678)
  expect_equal(sc_02@outcome_data@n_rows, 765)

  # sample_controls works with p_control = 0
  sc_03 <- sample_controls(trial_itt_csv, p_control = 0)
  expect_equal(sc_03@outcome_data@n_rows, 14)

  # cases are kept
  expect_equal(sum(sc_01@outcome_data@data$outcome), 14)
  expect_equal(sum(sc_02@outcome_data@data$outcome), 14)
  expect_equal(sum(sc_03@outcome_data@data$outcome), 14)

  # all columns are kept and sample_weight column is added
  expect_equal(
    colnames(sc_01@outcome_data@data), c(colnames(trial_itt_csv@expansion@datastore@template), "sample_weight")
  )

  # sample_controls subsets data correctly
  sc_04 <- sample_controls(
    trial_itt_csv,
    period = 1:10,
    subset_condition = "followup_time <= 20 & treatment == 1",
    p_control = 0.2,
    seed = 2332
  )
  expect_equal(
    sort(sc_04@outcome_data@data$id),
    c(
      14, 16, 20, 27, 27, 33, 33, 33, 33, 34, 34, 34, 44, 44, 44, 44, 44, 44, 44, 44, 47, 54, 54, 54, 54,
      59, 59, 59, 59, 59, 59, 59, 60, 60, 60, 65, 71, 73, 74, 74, 74, 83, 95, 95, 95, 95, 95, 95, 95, 96
    )
  )

  # sample_controls returns the correct classes
  expect_class(sc_04, "trial_sequence_ITT")
  expect_class(sc_04@outcome_data, "te_outcome_data")
  expect_class(sc_04@outcome_data@data, "data.table")

  unlink(trial_itt_dir, recursive = TRUE)
})


test_that("load_expanded_data works with trial_sequence objects containing te_datastore_csv objects", {
  trial_itt_dir <- file.path(tempdir(), "trial_itt")
  dir.create(trial_itt_dir)

  trial_itt <- trial_sequence(estimand = "ITT") |>
    set_data(
      data = data_censored,
      id = "id",
      period = "period",
      treatment = "treatment",
      outcome = "outcome",
      eligible = "eligible"
    ) |>
    set_censor_weight_model(
      censor_event = "censored",
      numerator = ~ x1 + x2 + x3,
      denominator = ~x2,
      pool_models = "numerator",
      model_fitter = stats_glm_logit(save_path = file.path(trial_itt_dir, "switch_models"))
    ) |>
    calculate_weights() |>
    set_outcome_model(adjustment_terms = ~ x1 + x2)

  trial_itt_csv <- set_expansion_options(
    trial_itt,
    output = save_to_csv(file.path(trial_itt_dir, "trial_csvs")),
    chunk_size = 500
  ) |>
    expand_trials()

  # load_expanded_data works without additional arguments
  sc_00 <- load_expanded_data(trial_itt_csv)
  expect_equal(sc_00@outcome_data@n_rows, 1558)

  # load_expanded_data works with p_control
  sc_01 <- load_expanded_data(trial_itt_csv, p_control = 0.01, seed = 1221)
  expect_equal(
    sort(sc_01@outcome_data@data$id),
    c(
      10, 14, 14, 15, 17, 21, 27, 29, 32, 38, 38, 44, 44, 49, 49,
      54, 54, 54, 59, 61, 68, 71, 71, 71, 74, 74, 89, 98, 98, 99
    )
  )

  random_01 <- load_expanded_data(trial_itt_csv, p_control = 0.01)

  # seed gets reset
  sc_01_1 <- load_expanded_data(trial_itt_csv, p_control = 0.01, seed = 1221)
  random_02 <- load_expanded_data(trial_itt_csv, p_control = 0.01)
  expect_false(identical(sort(random_01@outcome_data@data$id), sort(random_02@outcome_data@data$id)))

  # load_expanded_data works with p_control
  sc_02 <- load_expanded_data(trial_itt_csv, p_control = 0.5, seed = 5678)
  expect_equal(sc_02@outcome_data@n_rows, 765)

  # load_expanded_data works with p_control = 0
  sc_03 <- load_expanded_data(trial_itt_csv, p_control = 0)
  expect_equal(sc_03@outcome_data@n_rows, 14)

  # cases are kept
  expect_equal(sum(sc_00@outcome_data@data$outcome), 14)
  expect_equal(sum(sc_01@outcome_data@data$outcome), 14)
  expect_equal(sum(sc_02@outcome_data@data$outcome), 14)
  expect_equal(sum(sc_03@outcome_data@data$outcome), 14)

  # all columns are kept and sample_weight column is added
  expect_equal(
    colnames(sc_00@outcome_data@data), c(colnames(trial_itt_csv@expansion@datastore@template), "sample_weight")
  )
  expect_equal(sort(sc_00@outcome_data@data$sample_weight), rep(1, 1558))

  expect_equal(
    colnames(sc_01@outcome_data@data), c(colnames(trial_itt_csv@expansion@datastore@template), "sample_weight")
  )

  # load_expanded_data subsets data correctly with p_control
  sc_04 <- load_expanded_data(
    trial_itt_csv,
    period = 1:10,
    subset_condition = "followup_time <= 20 & treatment == 1",
    p_control = 0.2,
    seed = 2332
  )
  expect_equal(
    sort(sc_04@outcome_data@data$id),
    c(
      14, 16, 20, 27, 27, 33, 33, 33, 33, 34, 34, 34, 44, 44, 44, 44, 44, 44, 44, 44, 47, 54, 54, 54, 54,
      59, 59, 59, 59, 59, 59, 59, 60, 60, 60, 65, 71, 73, 74, 74, 74, 83, 95, 95, 95, 95, 95, 95, 95, 96
    )
  )

  # load_expanded_data subsets data correctly without p_control
  sc_05 <- load_expanded_data(
    trial_itt_csv,
    period = 1:10,
    subset_condition = "followup_time <= 20 & treatment == 1",
  )
  expect_equal(
    sort(sc_05@outcome_data@data$id),
    c(
      2, 2, 2, 14, 14, 14, 14, 14, 14, 16, 16, 16, 16, 20, 20, 20, 20, 21, 21, 21, 21, 21, 21, 21, 21, 21, 27, 27, 27,
      27, 27, 27, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 34, 34, 34, 34, 34, 34, 34, 34, 38, 38,
      44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44,
      44, 44, 44, 44, 44, 44, 44, 44, 47, 47, 47, 49, 49, 49, 50, 50, 50, 50, 53, 53, 53, 53, 53, 53, 53, 54, 54, 54,
      54, 54, 54, 54, 54, 54, 54, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59,
      59, 59, 59, 59, 59, 59, 59, 60, 60, 60, 60, 60, 60, 60, 60, 65, 65, 65, 65, 70, 70, 70, 71, 71, 71, 71, 71, 71,
      71, 71, 73, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 83, 83, 83, 95, 95, 95, 95, 95, 95,
      95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
      95, 95, 96, 96, 96, 96, 96, 96
    )
  )

  # load_expanded_data returns the correct classes
  expect_class(sc_04, "trial_sequence_ITT")
  expect_class(sc_04@outcome_data, "te_outcome_data")
  expect_class(sc_04@outcome_data@data, "data.table")

  unlink(trial_itt_dir, recursive = TRUE)
})
