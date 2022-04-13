test_that("adding splines works as expected", {
  tmp <- tempdir(check = TRUE)
  dir_path <- file.path(tmp, "splines_test")
  dir.create(dir_path)
  csv_path <- file.path(dir_path,"test_data.csv")
  write.csv(vignette_switch_data[1:10, ], file = csv_path, row.names = FALSE)

  expect_silent(
    result <- add_splines(
      csv_path,
      period_spline = list(df = 3),
      followup_spline = list(df = 2)
    )
  )
  result_csv <- read.csv(csv_path)
  unlink(file.path(tmp, "splines_test"), recursive = TRUE)

  expected <- data.frame(
    for_period = c(261L, 261L, 262L, 261L, 262L, 263L, 261L, 262L, 263L, 264L),
    followup_time = c(0L, 1L, 0L, 2L, 1L, 0L, 3L, 2L, 1L, 0L),
    period_base_1 = c(-0.425837760904742, -0.425837760904742, 0.357971000744179, -0.425837760904742, 0.357971000744179,
                      0.363443412259694,-0.425837760904742, 0.357971000744179, 0.363443412259694, -0.149975076313728),
    period_base_2 = c(0.588555825840501, 0.588555825840501, 0.415378493002204, 0.588555825840501, 0.415378493002204,
                      0.360407987923074, 0.588555825840501, 0.415378493002204, 0.360407987923074, 0.384242034683389),
    period_base_3 = c(-0.3531334955043, -0.3531334955043, -0.182560429134656, -0.3531334955043, -0.182560429134656,
                      0.217088540579489, -0.3531334955043, -0.182560429134656, 0.217088540579489, 0.769454779189967),
    followup_base_1 = c(0, 0.493727669390505, 0, 0.54521734791026, 0.493727669390505, 0, 0.301881699182848,
                        0.54521734791026, 0.493727669390505, 0),
    followup_base_2 = c(0, -0.229569934967636, 0, 0.106202924587177, -0.229569934967636,
                        0, 0.818870980490291, 0.106202924587177, -0.229569934967636, 0)
  )

  expect_identical(
    result_csv[, c("for_period", "followup_time",
                   "period_base_1", "period_base_2", "period_base_3",
                   "followup_base_1", "followup_base_2")],
    expected
  )

  expected_attributes_for_period <- list(
    degree = 3L,
    knots = c(`33.33333%` = 261, `66.66667%` = 262),
    Boundary.knots = c(261L, 264L),
    intercept = FALSE,
    class = c("ns", "basis", "matrix"),
    dim = c(10L, 3L),
    dimnames = list(NULL, c("1", "2", "3"))
  )
  expect_identical(attributes(result$for_period), expected_attributes_for_period)

  expected_attributes_followup_time <- list(
    degree = 3L,
    knots = c(`50%` = 1),
    Boundary.knots = c(0L, 3L),
    intercept = FALSE,
    class = c("ns", "basis", "matrix"),
    dim = c(10L, 2L),
    dimnames = list(NULL, c("1", "2"))
  )
  expect_identical(attributes(result$followup_time), expected_attributes_followup_time)
})
