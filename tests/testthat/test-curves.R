
test_that("predict.RTE_model works as expected", {
  trial_ex <- RandomisedTrialsEmulation::trial_example
  trial_ex$catvarA <- as.factor(trial_ex$catvarA)
  trial_ex$catvarB <- as.factor(trial_ex$catvarB)
  trial_ex$catvarC <- as.factor(trial_ex$catvarC)

  object <- initiators(
    data = trial_ex,
    id = "id",
    period = "period",
    eligible = "eligible",
    treatment = "treatment",
    outcome = "outcome",
    model_var = "assigned_treatment",
    outcome_cov = c("catvarA", "catvarB", "catvarC", "nvarA", "nvarB", "nvarC"),
    include_followup_time_case = ~followup_time,
    include_expansion_time_case = ~for_period,
    use_censor = 0,
    use_weight = 0,
    quiet = TRUE
  )

  set.seed(100)
  result <- predict(object, predict_times = 0:5, conf_int = FALSE)
  expect_list(result, "data.frame", any.missing = FALSE, len = 3)
  expect_snapshot_value(result, style = "json2", tolerance = 1e-06)

  set.seed(200)
  surv_result <- predict(object, predict_times = 0:8, conf_int = TRUE, type = "survival", samples = 5)
  expect_list(result, "data.frame", any.missing = FALSE, len = 3)
  expect_snapshot_value(result, style = "json2", tolerance = 1e-06)
})

test_that("predict.RTE_model works with newdata", {
  data <- as.data.table(RandomisedTrialsEmulation::vignette_switch_data)
  new_data <- data[data$followup_time == 0 & data$for_period == 300, ]
  data$catvarA <- factor(data$catvarA)

  object <- data_modelling(
    data,
    outcome_cov = ~ catvarA + nvarA,
    model_var = "assigned_treatment",
    include_followup_time_case = ~followup_time,
    include_expansion_time_case = ~for_period,
    use_sample_weights = FALSE,
    use_weight = 1,
    glm_function = "glm",
    quiet = TRUE
  )

  set.seed(300)
  expect_warning(
    result_newdata <- predict(object, newdata = new_data, predict_times = 0:8, conf_int = TRUE, samples = 5),
    "Attributes of newdata do not match data used for fitting. Attempting to fix."
  )
  expect_list(result_newdata, "data.frame", any.missing = FALSE, len = 3)
  expect_snapshot_value(result_newdata, style = "json2", tolerance = 1e-05)
})

test_that("calculate_cum_inc works as expected", {
  object <- matrix(
    c(0.1, 0.1, 0.1, 0.5, 0.2, 0.1),
    nrow = 2,
    byrow = TRUE
  )
  result <- calculate_cum_inc(object)
  expect_equal(result, c(0.3000, 0.3950, 0.4555))
})

test_that("calculate_survival works as expected", {
  object <- matrix(
    c(0.1, 0.1, 0.1, 0.5, 0.2, 0.1),
    nrow = 2,
    byrow = TRUE
  )
  result <- calculate_survival(object)
  expect_equal(result, c(0.7000, 0.6050, 0.5445))
})
