test_that("h_extract_baseline works works as expected", {
  trial_file <- tempfile(
    "trial_data",
    tmpdir = tempdir(check = TRUE),
    fileext = ".csv"
  )

  baseline_file <- tempfile(
    "baseline_file",
    tmpdir = tempdir(check = TRUE),
    fileext = ".csv"
  )

  object <- data.frame(
    id = c(1, 1, 1, 2, 2, 3, 3, 3),
    followup_time = c(0, 1, 2, 0, 1, 0, 1, 2),
    outcome = c(0, 0, 1, 0, 1, 0, 0, 1)
  )
  write.csv(object, trial_file, row.names = FALSE)

  h_extract_baseline(trial_file, baseline_file)

  result <- read.csv(baseline_file, row.names = NULL)
  expected <- data.frame(
    id = c(1, 2, 3),
    followup_time = c(0, 0, 0),
    outcome = c(0, 0, 0)
  )

  expect_equal(result, expected)
})

test_that("predict_survival works as expected", {
  temp_dir <- tempdir(check = TRUE)

  object <- initiators(
    data = RandomisedTrialsEmulation::trial_example,
    id = "id",
    period = "period",
    eligible = "eligible",
    treatment = "treatment",
    outcome = "outcome",
    model_var = "assigned_treatment",
    outcomeCov = c("catvarA", "catvarB", "catvarC", "nvarA", "nvarB", "nvarC"),
    outcomeCov_var = c("catvarA", "catvarB", "catvarC", "nvarA", "nvarB", "nvarC"),
    outcomeClass = c("catvarA", "catvarB", "catvarC"),
    numCores = 1,
    data_dir = temp_dir,
    use_censor = 0,
    use_weight = 0
  )
  unlink(temp_dir, recursive = TRUE)

  result <- predict_survival(object, predict_times = c(1, 2, 3, 4, 5))
  expect_equal(
    result,
    list(
      trt_0 = c(0.00468820988443441, 0.00934429889707655, 0.0139688804282337, 0.0185625375205256, 0.0231257743083843),
      trt_1 = c(0.00359094291314119, 0.00716409318950961, 0.0107197162554207, 0.0142580669986666, 0.0177793599907887)
    )
  )
})

test_that("sum_up_ci works as expected", {
  object <- list(
    trial_1 = matrix(
      c(
        0.1, 0.1, 0.1,
        0.5, 0.2, 0.1
      ),
      nrow = 2,
      byrow = TRUE
    ),
    trial_2 = matrix(
      c(
        0.15, 0.15, 0.15,
        0.45, 0.25, 0.1
      ),
      nrow = 2,
      byrow = TRUE
    )
  )
  result <- sum_up_ci(object)
  expect_equal(result, c(0.3000000, 0.4137500, 0.48471875))
})

test_that("ci_up_to works as expected", {
  object <- matrix(
    c(0.1, 0.1, 0.1, 0.5, 0.2, 0.1),
    nrow = 2,
    byrow = TRUE
  )
  result <- ci_up_to(object)
  expect_equal(result, c(0.600, 0.790, 0.916))
})

test_that("survival_up_to works as expected", {
  object <- matrix(
    c(0.1, 0.1, 0.1, 0.5, 0.2, 0.1),
    nrow = 2,
    byrow = TRUE
  )
  result <- survival_up_to(object)
  expect_equal(result, c(1.0000, 0.7000, 0.6050, 0.5445))
})
