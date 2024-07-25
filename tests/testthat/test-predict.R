test_that("predict.TE_msm works as expected", {
  trial_ex <- TrialEmulation::trial_example
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
    estimand_type = "ITT",
    include_followup_time = ~followup_time,
    include_trial_period = ~trial_period,
    use_censor_weights = FALSE,
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

test_that("predict.TE_msm works with newdata", {
  data <- as.data.table(TrialEmulation::vignette_switch_data)
  new_data <- data[data$followup_time == 0 & data$trial_period == 300, ]
  data$catvarA <- factor(data$catvarA)

  object <- trial_msm(
    data,
    outcome_cov = ~ catvarA + nvarA,
    model_var = "assigned_treatment",
    include_followup_time = ~followup_time,
    include_trial_period = ~trial_period,
    use_sample_weights = FALSE,
    glm_function = "glm",
    quiet = TRUE
  )

  set.seed(300)
  expect_snapshot_value(
    mvtnorm::rmvnorm(n = 5, mean = object$model$coefficients, sigma = object$robust$matrix),
    style = "json2"
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


test_that("predict.TE_msm works with interactions", {
  data <- readRDS(test_path("data/ready_for_modelling.rds"))

  expect_warning(
    expect_warning(
      object <- trial_msm(
        data = data,
        outcome_cov = ~ X1 + X2 + age_s,
        model_var = ~ assigned_treatment:followup_time,
        include_followup_time = ~followup_time,
        include_trial_period = ~1,
        glm_function = c("glm"),
        use_sample_weights = FALSE,
        quiet = TRUE
      ),
      "non-integer #successes in a binomial glm",
    ),
    "fitted probabilities numerically 0 or 1 occurred"
  )

  set.seed(100)
  result <- predict(object, predict_times = 0:8, conf_int = TRUE, samples = 5)
  expect_snapshot_value(result, style = "json2", tolerance = 1e-05)
})


test_that("predict.TE_msm warns for As-Treated", {
  data <- readRDS(test_path("data/ready_for_modelling.rds"))

  expect_warning(
    expect_warning(
      object <- trial_msm(
        data = data,
        outcome_cov = ~ X1 + X2 + age_s,
        model_var = ~ assigned_treatment:followup_time,
        estimand_type = "As-Treated",
        include_followup_time = ~followup_time,
        include_trial_period = ~1,
        glm_function = c("glm"),
        use_sample_weights = FALSE,
        quiet = TRUE
      ),
      "non-integer #successes in a binomial glm",
    ),
    "fitted probabilities numerically 0 or 1 occurred"
  )
  expect_warning(
    predict(object, predict_times = 0:8, conf_int = TRUE, samples = 5),
    "As-Treated estimands are not currently supported by this predict method"
  )
})


# Compare with new methods
test_that("predict.TE_msm gives the same results as new predict", {
  trial_ex <- TrialEmulation::trial_example
  trial_ex$catvarA <- as.factor(trial_ex$catvarA)
  trial_ex$catvarB <- as.factor(trial_ex$catvarB)
  trial_ex$catvarC <- as.factor(trial_ex$catvarC)

  itt <- trial_sequence("ITT") |>
    set_data(trial_ex) |>
    set_outcome_model(
      ~ catvarA + catvarB + catvarC + nvarA + nvarB + nvarC,
      followup_time_terms = ~followup_time,
      trial_period_terms = ~trial_period
    ) |>
    set_expansion_options(save_to_datatable(), chunk_size = 500) |>
    expand_trials() |>
    load_expanded_data() |>
    fit_msm()

  expect_equal(
    itt@outcome_model@fitted@summary$tidy$estimate,
    c(
      -3.11805144344606, -0.268581621196876, 0.294499102735304, 0.135462387287878,
      -11.0349248424433, 0.447172140677408, -0.391385512377657, -0.413877810263137,
      -2.41681785503564, -0.702964112683514, -0.0484645695440711, -0.0646937103624789,
      -0.11571370119947, -0.0842238404471826, 0.00518779702893541,
      -0.0422259965077797, 0.00140020000546132, 0.00201835691715962
    )
  )
  set.seed(100)
  result_itt <- predict(itt, predict_times = 0:5, conf_int = FALSE)
  expect_list(result_itt, "data.frame", any.missing = FALSE, len = 3)
  expect_snapshot_value(result_itt, style = "json2", tolerance = 1e-06)

  # from test "predict.TE_msm works as expected" above
  expect_equal(
    result_itt,
    list(
      assigned_treatment_0 = data.frame(
        followup_time = 0:5,
        cum_inc = c(
          0.00468170395828404, 0.0093313839943624, 0.0139496508678079,
          0.0185370851551803, 0.0230942392369657, 0.0276216391451553
        )
      ),
      assigned_treatment_1 = data.frame(
        followup_time = 0:5,
        cum_inc = c(
          0.00358595019692831, 0.00715416360463761, 0.0107049044733909,
          0.0142384265736581, 0.0177549737297923, 0.021254780325148
        )
      ),
      difference = data.frame(
        followup_time = 0:5,
        cum_inc_diff = c(
          -0.00109575376135573, -0.00217722038972479, -0.00324474639441696,
          -0.00429865858152212, -0.00533926550717345, -0.0063668588200072
        )
      )
    )
  )

  set.seed(200)
  surv_result <- predict(itt, predict_times = 0:8, conf_int = TRUE, type = "survival", samples = 5)
  expect_list(surv_result, "data.frame", any.missing = FALSE, len = 3)
  expect_snapshot_value(surv_result, style = "json2", tolerance = 1e-06)
})
