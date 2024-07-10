test_that("read_expanded_data can read te_datastore_csv data", {
  temp_dir <- tempfile("csv_dir_")
  dir.create(temp_dir)
  datastore <- save_to_csv(temp_dir)
  expanded_csv_data <- save_expanded_data(datastore, subset(vignette_switch_data, trial_period %in% 1:12))

  # check if no columns get added or removed by read_expanded_data
  expect_equal(ncol(read_expanded_data(expanded_csv_data)), length(expanded_csv_data@template))

  # check if omitting period reads in all data
  expect_equal(nrow(read_expanded_data(expanded_csv_data)), 25948)

  # check if period argument subsets data correctly
  expect_equal(nrow(read_expanded_data(expanded_csv_data, 1)), 1979)
  expect_equal(nrow(read_expanded_data(expanded_csv_data, c(5, 8, 12))), 6493)

  # check if method throws an error when using a character as period
  expect_error(read_expanded_data(expanded_csv_data, "1"))

  # check if no new NAs are introduced
  expect_equal(
    sum(is.na.data.frame(read_expanded_data(expanded_csv_data))),
    sum(is.na.data.frame(subset(vignette_switch_data, trial_period %in% 1:12)))
  )

  # check if subset_condition subsets data correctly
  expect_equal(
    nrow(read_expanded_data(expanded_csv_data, subset_condition = "followup_time <= 30 & nvarC == 40")), 371
  )

  unlink(temp_dir, recursive = TRUE)
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
      15, 16, 20, 20, 21, 29, 32, 38, 38, 40, 44, 44, 44, 49, 49, 49,
      50, 58, 61, 65, 68, 71, 71, 74, 74, 74, 74, 89, 95, 96, 98, 99
    )
  )

  random_01 <- sample_controls(trial_itt_csv, p_control = 0.01)

  # seed gets reset
  sc_01_1 <- sample_controls(trial_itt_csv, p_control = 0.01, seed = 1221)
  random_02 <- sample_controls(trial_itt_csv, p_control = 0.01)
  expect_false(identical(sort(random_01@outcome_data@data$id), sort(random_02@outcome_data@data$id)))

  # sample_controls works with p_control
  sc_02 <- sample_controls(trial_itt_csv, p_control = 0.5, seed = 5678)
  expect_equal(sc_02@outcome_data@n_rows, 780)

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
      21, 21, 21, 27, 27, 33, 33, 33, 34, 34, 44, 44, 44, 44, 44, 47, 50, 53, 54, 54, 59, 59, 59, 59, 59, 59,
      59, 60, 60, 60, 60, 60, 65, 65, 73, 74, 74, 74, 74, 83, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 96
    )
  )

  # if non-existing periods are entered sample_controls omits the missing periods, runs the code and returns a warning
  expect_warning(
    sc_05 <- sample_controls(
      trial_itt_csv,
      period = c(1:10, 20),
      subset_condition = "followup_time <= 20 & treatment == 1",
      p_control = 0.2,
      seed = 2332
    ),
    "The following periods don't exist in the data and were omitted: 20"
  )
  expect_equal(sort(sc_04@outcome_data@data$id), sort(sc_05@outcome_data@data$id))

  # sample_controls returns the correct classes
  expect_class(sc_04, "trial_sequence_ITT")
  expect_class(sc_04@outcome_data, "te_outcome_data")
  expect_class(sc_04@outcome_data@data, "data.table")

  unlink(trial_itt_dir, recursive = TRUE)
})
