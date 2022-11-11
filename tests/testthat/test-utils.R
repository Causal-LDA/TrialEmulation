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
