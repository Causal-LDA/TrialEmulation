# h_quiet_print ----
test_that("h_quiet_print works as expected", {
  expect_silent(h_quiet_print(quiet = TRUE, "hello"))
  expect_output(h_quiet_print(quiet = FALSE, "hello"), "hello")
})
