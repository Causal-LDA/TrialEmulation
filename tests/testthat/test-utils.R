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
