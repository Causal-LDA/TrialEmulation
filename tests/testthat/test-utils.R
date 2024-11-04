test_that("quiet_print works as expected", {
  expect_silent(quiet_print(quiet = TRUE, "hello"))
  expect_output(quiet_print(quiet = FALSE, "hello"), "hello")
})


test_that("quiet_msg works as expected", {
  expect_silent(quiet_msg(quiet = TRUE, "hello"))
  expect_message(quiet_msg(quiet = FALSE, "hello"), "hello")
})


test_that("quiet_line works as expected", {
  expect_silent(quiet_line(quiet = TRUE))
  expect_message(quiet_line(quiet = FALSE), "-------")
})

test_that("quiet_msg_time works as expected", {
  t <- system.time(for (i in 1:1000) x <- mean(rt(1000, df = 4)))
  expected_time <- sprintf("%0.1f s", t["elapsed"])
  expect_silent(quiet_msg_time(quiet = TRUE, "time: ", t))
  expect_message(
    quiet_msg_time(quiet = FALSE, "time: ", t),
    paste0("time: ", expected_time)
  )
})

test_that("extract_baseline works works as expected", {
  save_dir <- withr::local_tempdir(pattern = "curve", tempdir(TRUE))

  trial_file <- tempfile(
    "trial_data",
    tmpdir = save_dir,
    fileext = ".csv"
  )

  baseline_file <- tempfile(
    "baseline_file",
    tmpdir = save_dir,
    fileext = ".csv"
  )

  object <- data.frame(
    id = c(1, 1, 1, 2, 2, 3, 3, 3),
    followup_time = c(0, 1, 2, 0, 1, 0, 1, 2),
    outcome = c(0, 0, 1, 0, 1, 0, 0, 1)
  )
  write.csv(object, trial_file, row.names = FALSE)

  extract_baseline(trial_file, baseline_file)

  result <- read.csv(baseline_file, row.names = NULL)
  expected <- data.frame(
    id = c(1, 2, 3),
    followup_time = c(0, 0, 0),
    outcome = c(0, 0, 0)
  )

  expect_equal(result, expected)
})


test_that("as_formula works with a formula as string", {
  result <- as_formula("~age + sex")
  expected <- ~ age + sex
  environment(expected) <- environment(result) <- globalenv()
  expect_equal(result, expected)
})

test_that("as_formula works with strings", {
  result <- as_formula(c("age", "bmi", "sex"))
  expected <- ~ age + bmi + sex
  environment(expected) <- environment(result) <- globalenv()
  expect_equal(result, expected)
})

test_that("as_formula works with formulas", {
  result <- as_formula(~ age + sex + time)
  expected <- ~ age + sex + time
  environment(expected) <- environment(result) <- globalenv()
  expect_equal(result, expected)
})

test_that("assert_monotonic works as expected", {
  expect_error(assert_monotonic(c(1, 4, 3)), "Not monotonically increasing")
  expect_error(assert_monotonic(c(1, 4, 3), increasing = FALSE), "Not monotonically decreasing")
  expect_error(assert_monotonic(c(1, 2, 3), increasing = FALSE), "Not monotonically decreasing")
  expect_error(assert_monotonic(c(3, 2, 1), increasing = TRUE), "Not monotonically increasing")
  expect_null(assert_monotonic(1:4))
  expect_null(assert_monotonic(4:-1, increasing = FALSE))
})


test_that("rhs_vars works as expected", {
  result <- rhs_vars(a ~ b + c - 1 + e:f)
  expect_equal(result, c("b", "c", "e", "f"))
})
