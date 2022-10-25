test_that("check_expand_switch works as expected", {
  result <- expand_until_switch(c(0, 0, 0, 0, 0), 5)
  expect_equal(result, c(1, 1, 1, 1, 1))

  result <- expand_until_switch(c(0, 0, 1, 0, 0), 5)
  expect_equal(result, c(1, 1, 0, 0, 0))

  result <- expand_until_switch(c(1, 0, 0, 0, 0), 5)
  expect_equal(result, c(0, 0, 0, 0, 0))

  result <- expand_until_switch(c(0, 1, 0, 1, 0), 5)
  expect_equal(result, c(1, 0, 0, 0, 0))
})


test_that("expand works as expected", {
  test_data <- data.table(
    id = c(4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L),
    period = 0:7,
    X1 = c(0, 0, 1, 1, 0, 0, 1, 0),
    X2 = c(
      -1.00754755070536, 0.383910338749026, -0.435566392392335, 0.197691769760149,
      -0.453560924989805, 0.244505339400415, 0.204938469621909, -0.186469740102758
    ),
    X3 = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
    X4 = c(
      -0.307927216800277, -0.307927216800277, -0.307927216800277, -0.307927216800277,
      -0.307927216800277, -0.307927216800277, -0.307927216800277, -0.307927216800277
    ),
    age_s = c(
      1.5, 1.58333333333333, 1.66666666666667, 1.75, 1.83333333333333, 1.91666666666667, 2, 2.08333333333333
    ),
    C = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
    treatment = c(0, 0, 1, 1, 1, 1, 1, 0),
    outcome = c(0, 0, 0, 0, 0, 0, 0, 0),
    eligible = c(1, 1, 1, 0, 0, 0, 0, 0),
    time_of_event = c(9999, 9999, 9999, 9999, 9999, 9999, 9999, 9999),
    first = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    am_1 = c(0, 0, 0, 1, 1, 1, 1, 1),
    cumA = c(0, 0, 1, 2, 3, 4, 5, 5),
    switch = c(0, 0, 1, 0, 0, 0, 0, 1),
    regime_start = c(0L, 0L, 2L, 2L, 2L, 2L, 2L, 7L),
    time_on_regime = c(0, 1, 2, 1, 2, 3, 4, 5),
    eligible0 = c(1, 1, 1, 0, 0, 0, 0, 0),
    eligible1 = c(0, 0, 0, 1, 1, 1, 1, 1),
    p0_d = c(0.212604244116926, 0.386124116316987, 0.376609196163616, NA, NA, NA, NA, NA),
    p0_n = c(0.444263330717273, 0.444263330717273, 0.444263330717273, NA, NA, NA, NA, NA),
    p1_d = c(
      NA, NA, NA, 0.708317769732135, 0.497115761633231, 0.581487953858224, 0.693820072457607, 0.51375805304461
    ),
    p1_n = c(
      NA, NA, NA, 0.625635432675062, 0.625635432675062, 0.625635432675062, 0.625635432675062, 0.625635432675062
    ),
    pC_d0 = c(0.962745604189626, 0.931470702987998, 0.973506948893895, NA, NA, NA, NA, NA),
    pC_n0 = c(0.800815183245838, 0.800815183245838, 0.800815183245838, NA, NA, NA, NA, NA),
    pC_d1 = c(NA, NA, NA, 0.989563930801156, 0.98646241958083, 0.982423199990179, 0.992093559499191, 0.988282142133322),
    pC_n1 = c(
      NA, NA, NA, 0.933796639430995, 0.933796639430995, 0.933796639430995, 0.933796639430995, 0.933796639430995
    ),
    wt = c(
      0.587079260613061, 0.778308153988203, 0.970382130653937, 0.833492409839268,
      1.19133956566757, 1.02266716324287, 0.848738989416906, 0.727467631350025
    ),
    pC_n = c(
      0.800815183245838, 0.800815183245838, 0.800815183245838, 0.933796639430995,
      0.933796639430995, 0.933796639430995, 0.933796639430995, 0.933796639430995
    ),
    pC_d = c(
      0.962745604189626, 0.931470702987998, 0.973506948893895, 0.989563930801156,
      0.98646241958083, 0.982423199990179, 0.992093559499191, 0.988282142133322
    ),
    wtC = c(
      0.831803520847971, 0.859732013768077, 0.822608594787874, 0.943644579562422,
      0.946611468308935, 0.9505034484531, 0.941238485513781, 0.944868473910988
    )
  )

  result <- expand(
    sw_data = test_data,
    keeplist = c(
      "id", "for_period", "followup_time", "outcome", "weight", "treatment",
      "X1", "X2", "X3", "X4", "age_s", "assigned_treatment"
    ),
    lag_p_nosw = 1,
    maxperiod = 9L,
    minperiod = 0L,
    outcomeCov_var = c("X1", "X2", "X3", "X4", "age_s"),
    use_censor = 1,
    where_var = NULL
  )

  expect_data_table(result, nrows = 8, ncols = 12)

  expected <- data.table(
    id = c(4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L),
    for_period = c(0L, 0L, 1L, 2L, 2L, 2L, 2L, 2L),
    followup_time = c(0L, 1L, 0L, 0L, 1L, 2L, 3L, 4L),
    outcome = c(0, 0, 0, 0, 0, 0, 0, 0),
    weight = c(1, 0.778308153988203, 1, 1, 0.833492409839268, 0.99297248552513, 1.01548035495021, 0.861877770233159),
    treatment = c(0, 0, 0, 1, 1, 1, 1, 1),
    X1 = c(0, 0, 0, 1, 1, 1, 1, 1),
    X2 = c(
      -1.00754755070536, -1.00754755070536, 0.383910338749026, -0.435566392392335, -0.435566392392335,
      -0.435566392392335, -0.435566392392335, -0.435566392392335
    ),
    X3 = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
    X4 = c(
      -0.307927216800277, -0.307927216800277, -0.307927216800277, -0.307927216800277,
      -0.307927216800277, -0.307927216800277, -0.307927216800277, -0.307927216800277
    ),
    age_s = c(
      1.5, 1.5, 1.58333333333333, 1.66666666666667, 1.66666666666667, 1.66666666666667, 1.66666666666667,
      1.66666666666667
    ),
    assigned_treatment = c(0, 0, 0, 1, 1, 1, 1, 1)
  )

  expect_equal(result, expected)
})


test_that("data extensions works as expected with first and last periods", {
  data <- readRDS(test_path("data/pre_data_extension.rds"))

  result_limited <- RandomisedTrialsEmulation:::data_extension(
    data = data,
    keeplist = c(
      "id", "for_period", "followup_time", "outcome", "weight", "treatment",
      "X1", "X2", "X3", "X4", "age_s", "assigned_treatment"
    ),
    outcomeCov_var = c("X1", "X2", "X3", "X4", "age_s"),
    first_period = 2,
    last_period = 8,
    use_censor = 1,
    lag_p_nosw = 1,
    where_var = NULL,
    separate_files = FALSE
  )

  result <- RandomisedTrialsEmulation:::data_extension(
    data = data,
    keeplist = c(
      "id", "for_period", "followup_time", "outcome", "weight", "treatment",
      "X1", "X2", "X3", "X4", "age_s", "assigned_treatment"
    ),
    outcomeCov_var = c("X1", "X2", "X3", "X4", "age_s"),
    use_censor = 1,
    lag_p_nosw = 1,
    where_var = NULL,
    separate_files = FALSE
  )

  expect_equal(
    result_limited$data,
    result$data[result$data$for_period >= 2 & result$data$for_period <= 8]
  )
  expect_equal(result_limited$min_period, 2)
  expect_equal(result_limited$max_period, 8)
  expect_data_frame(
    result_limited$data,
    nrows = 1041,
    ncols = 12
  )
  expect_equal(result$min_period, 0)
  expect_equal(result$max_period, 9)
  expect_data_frame(
    result$data,
    nrows = 4138,
    ncols = 12
  )
})


test_that("data extensions works as expected with separate_files=TRUE", {
  data <- readRDS(test_path("data/pre_data_extension.RDS"))
  temp_path <- tempdir(check = TRUE)
  all_dir <- file.path(temp_path, "all")
  dir.create(all_dir)
  subset_dir <- file.path(temp_path, "subset")
  dir.create(subset_dir)

  result_limited <- data_extension(
    data = data,
    keeplist = c(
      "id", "for_period", "followup_time", "outcome", "weight", "treatment",
      "X1", "X2", "X3", "X4", "age_s", "assigned_treatment"
    ),
    outcomeCov_var = c("X1", "X2", "X3", "X4", "age_s"),
    first_period = 2,
    last_period = 8,
    use_censor = 1,
    lag_p_nosw = 1,
    where_var = NULL,
    separate_files = TRUE,
    data_dir = subset_dir
  )

  result <- data_extension(
    data = data,
    keeplist = c(
      "id", "for_period", "followup_time", "outcome", "weight", "treatment",
      "X1", "X2", "X3", "X4", "age_s", "assigned_treatment"
    ),
    outcomeCov_var = c("X1", "X2", "X3", "X4", "age_s"),
    use_censor = 1,
    lag_p_nosw = 1,
    where_var = NULL,
    separate_files = TRUE,
    data_dir = all_dir
  )

  expect_file(result_limited$data)
  expect_file(result$data)

  expect_character(result$data, len = 10)
  expect_character(result_limited$data, len = 7)

  expect_identical(
    read.csv(file.path(all_dir, "trial_2.csv")),
    read.csv(file.path(subset_dir, "trial_2.csv"))
  )
  expect_equal(list.files(subset_dir, full.names = TRUE), result_limited$data)
  expect_equal(list.files(all_dir, full.names = TRUE), result$data)


  result_trials <- rbindlist(c(
    list(result_limited$data_template),
    lapply(result_limited$data, data.table::fread)
  ))

  expected_limited <- RandomisedTrialsEmulation:::data_extension(
    data = data,
    keeplist = c(
      "id", "for_period", "followup_time", "outcome", "weight", "treatment",
      "X1", "X2", "X3", "X4", "age_s", "assigned_treatment"
    ),
    outcomeCov_var = c("X1", "X2", "X3", "X4", "age_s"),
    first_period = 2,
    last_period = 8,
    use_censor = 1,
    lag_p_nosw = 1,
    where_var = NULL,
    separate_files = FALSE
  )
  data.table::setorderv(expected_limited$data, c("for_period", "id", "followup_time"))
  expect_equal(as.data.frame(result_trials)$weight, as.data.frame(expected_limited$data)$weight)

  unlink(all_dir, recursive = TRUE)
  unlink(subset_dir, recursive = TRUE)
})
