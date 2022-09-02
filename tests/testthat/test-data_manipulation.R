test_that("data_manipulation works as expected with default options", {
  result <- withr::with_tempdir(
    {
      dat_dir <- getwd()
      data_manipulation(
        trial_example,
        data_dir = dat_dir
      )
    },
    tmpdir = tempdir(check = TRUE)
  )
  expect_data_table(
    result,
    key = "id",
    nrow = 48400,
    ncol = 18
  )
  expect_subset(
    colnames(result),
    c(
      "id", "period", "treatment", "outcome", "eligible", "after_eligibility",
      "after_event", "time_of_event", "first", "am_1", "cumA", "switch",
      "regime_start", "time_on_regime", "time_on_regime2", "eligible0",
      "eligible1", "wt"
    )
  )
  expect_equal(length(unique(result$id)), 503L)
})
