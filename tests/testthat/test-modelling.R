# data_modelling ----
test_that("data_modelling can be quiet", {
  data <- vignette_switch_data
  expect_silent(
    result <- data_modelling(
      data,
      outcome_cov = c("catvarA", "nvarA"),
      model_var = "assigned_treatment",
      include_followup_time = ~followup_time,
      include_expansion_time = ~for_period,
      use_sample_weights = FALSE,
      quiet = TRUE,
      glm_function = "glm"
    )
  )
})

test_that("data_modelling gives expected results in example data", {
  data <- vignette_switch_data
  result <- data_modelling(
    data,
    outcome_cov = c("catvarA", "catvarB", "catvarC", "nvarA", "nvarB", "nvarC"),
    model_var = "assigned_treatment",
    include_followup_time = ~followup_time,
    include_expansion_time = ~for_period,
    use_sample_weights = FALSE,
    quiet = TRUE,
    glm_function = "parglm",
    control = parglm.control()
  )
  expect_class(result$model, "glm")
  expected_coefs <- c(
    `(Intercept)` = -3.44513044265409,
    assigned_treatment = -0.279511046771487,
    for_period = 0.00193709391469456,
    followup_time = 0.00139579193160405,
    catvarA = 0.0586518690748273,
    catvarB = -0.0545402601125559,
    catvarC = -0.0209619578642913,
    nvarA = -0.0794050643431851,
    nvarB = 0.00477012273880385,
    nvarC = -0.0404651039259053
  )
  expect_equal(result$model$coefficients, expected_coefs)

  expect_equal(
    result$robust$summary$names,
    c(
      "(Intercept)", "assigned_treatment", "for_period", "followup_time",
      "catvarA", "catvarB", "catvarC", "nvarA", "nvarB", "nvarC"
    )
  )
  expected_robust_se <- c(
    0.608156159520476, 0.309527599563302, 0.00159274700265935,
    0.0015016748931986, 0.0312208855654385, 0.0296048600610076, 0.0263579673428329,
    0.0447759173905258, 0.00231602984314177, 0.00671869054658421
  )
  expect_equal(result$robust$summary$robust_se, expected_robust_se)

  expect_matrix(result$robust$matrix, nrows = 10, ncols = 10, any.missing = FALSE)
})


test_that("data_modelling works with data.tables and weights", {
  data <- as.data.table(TrialEmulation::vignette_switch_data)
  expect_silent(
    result_parglm <- data_modelling(
      data,
      outcome_cov = c("catvarA", "nvarA"),
      model_var = "assigned_treatment",
      include_followup_time = ~followup_time,
      include_expansion_time = ~for_period,
      use_sample_weights = FALSE,
      use_weight = TRUE,
      glm_function = "parglm",
      control = parglm.control(nthreads = 2, method = "FAST"),
      quiet = TRUE
    )
  )

  expect_silent(
    result_glm <- data_modelling(
      data,
      outcome_cov = c("catvarA", "nvarA"),
      model_var = "assigned_treatment",
      include_followup_time = ~followup_time,
      include_expansion_time = ~for_period,
      use_sample_weights = FALSE,
      use_weight = TRUE,
      glm_function = "glm",
      quiet = TRUE
    )
  )
  expect_equal(result_glm$model$coefficients, result_parglm$model$coefficients)
})


test_that("Modelling works with where_case", {
  if (FALSE) {
    set.seed(20222022)
    simdata_censored <- data_gen_censored(1000, 10)
    prep_PP_data <- data_preparation(
      data = simdata_censored,
      id = "ID",
      period = "t",
      treatment = "A",
      outcome = "Y",
      eligible = "eligible",
      outcome_cov = ~ X1 + X2 + X3 + X4 + age_s,
      model_var = "assigned_treatment",
      use_weight = TRUE,
      use_censor = TRUE,
      switch_d_cov = ~ X1 + X2 + X3 + X4 + age_s,
      switch_n_cov = ~ X3 + X4,
      cense = "C",
      cense_d_cov = ~ X1 + X2 + X3 + X4 + age_s,
      cense_n_cov = ~ X3 + X4,
      separate_files = FALSE,
      last_period = 8,
      first_period = 2,
      where_var = "age",
      quiet = TRUE
    )
    saveRDS(simdata_censored, test_path("data/raw_data.rds"))
    saveRDS(prep_PP_data, test_path("data/prep_data_object.rds"))
    saveRDS(prep_PP_data$data, test_path("data/ready_for_modelling.rds"))
  }

  data <- readRDS(test_path("data/ready_for_modelling.rds"))

  expect_warning(
    result <- data_modelling(
      data = data,
      outcome_cov = ~ X1 + X2 + X3 + X4 + age_s,
      model_var = "assigned_treatment",
      use_weight = TRUE,
      use_censor = TRUE,
      include_followup_time = ~ factor(followup_time),
      include_expansion_time = ~1,
      glm_function = c("glm"),
      use_sample_weights = FALSE,
      first_followup = 0,
      last_followup = 4,
      where_case = "age > 30",
      quiet = TRUE
    ),
    "non-integer #successes in a binomial glm",
    fixed = TRUE
  )
  expect_class(result$model, "glm")
  expect_snapshot_value(as.data.frame(result$robust$summary), style = "json2")
})

test_that("data_modelling works with analysis_weights = unweighted", {
  data <- readRDS(test_path("data/ready_for_modelling.rds"))

  expect_silent(
    result_unweighted <- data_modelling(
      data,
      outcome_cov = ~ X1 + X2 + X3 + X4 + age_s,
      model_var = "assigned_treatment",
      include_followup_time = ~followup_time,
      include_expansion_time = ~for_period,
      use_sample_weights = FALSE,
      use_weight = TRUE,
      glm_function = "glm",
      quiet = TRUE,
      analysis_weights = "unweighted"
    )
  )

  expect_snapshot_value(as.data.frame(result_unweighted$robust$summary), style = "json2")
})

test_that("data_modelling works with analysis_weights = p99", {
  data <- readRDS(test_path("data/ready_for_modelling.rds"))

  expect_warning(
    result_p99 <- data_modelling(
      data,
      outcome_cov = ~ X1 + X2 + X3 + X4 + age_s,
      model_var = "assigned_treatment",
      include_followup_time = ~followup_time,
      include_expansion_time = ~for_period,
      use_sample_weights = FALSE,
      use_weight = TRUE,
      glm_function = "glm",
      quiet = TRUE,
      analysis_weights = "p99"
    ),
    "non-integer #successes in a binomial glm!"
  )
  expect_snapshot_value(as.data.frame(result_p99$robust$summary), style = "json2")
})


test_that("data_modelling works with analysis_weights = weight_limits", {
  data <- readRDS(test_path("data/ready_for_modelling.rds"))

  expect_warning(
    result_limits <- data_modelling(
      data,
      outcome_cov = ~ X1 + X2 + X3 + X4 + age_s,
      model_var = "assigned_treatment",
      include_followup_time = ~followup_time,
      include_expansion_time = ~for_period,
      use_sample_weights = FALSE,
      use_weight = TRUE,
      glm_function = "glm",
      quiet = TRUE,
      analysis_weights = "weight_limits",
      weight_limits = c(0, Inf)
    ),
    "non-integer #successes in a binomial glm!"
  )
  expect_snapshot_value(as.data.frame(result_limits$robust$summary), style = "json2")
})



test_that("data_modelling works with missing sample weights", {
  data <- readRDS(test_path("data/ready_for_modelling.rds"))
  expect_warning(
    expect_warning(
      result_sample <- data_modelling(
        data,
        outcome_cov = ~ X1 + X2 + X3 + X4 + age_s,
        model_var = "assigned_treatment",
        include_followup_time = ~followup_time,
        include_expansion_time = ~for_period,
        use_sample_weights = TRUE,
        use_weight = TRUE,
        glm_function = "glm",
        quiet = TRUE,
      ),
      "non-integer #successes in a binomial glm!"
    ),
    "'sample_weight' column not found in data. Using sample weights = 1."
  )

  expect_warning(
    expected_result <- data_modelling(
      data,
      outcome_cov = ~ X1 + X2 + X3 + X4 + age_s,
      model_var = "assigned_treatment",
      include_followup_time = ~followup_time,
      include_expansion_time = ~for_period,
      use_sample_weights = FALSE,
      use_weight = TRUE,
      glm_function = "glm",
      quiet = TRUE,
    ),
    "non-integer #successes in a binomial glm!"
  )
  expect_equal(result_sample$robust$summary, expected_result$robust$summary)
})

test_that("data_modelling works with sample weights", {
  data <- readRDS(test_path("data/prep_data_object.rds"))
  set.seed(2020)
  sampled_data <- case_control_sampling_trials(data, p_control = 0.5)

  expect_warning(
    result_sample <- data_modelling(
      sampled_data,
      outcome_cov = ~ X1 + X2 + X3 + X4 + age_s,
      model_var = "assigned_treatment",
      include_followup_time = ~followup_time,
      include_expansion_time = ~for_period,
      use_sample_weights = TRUE,
      use_weight = TRUE,
      glm_function = "glm",
      quiet = TRUE,
    ),
    "non-integer #successes in a binomial glm!"
  )
  expect_snapshot_value(as.data.frame(result_sample$robust$summary), style = "json2")
})


test_that("data_modelling makes model formula as expected with weight and censor", {
  data <- readRDS(test_path("data/prep_data_object.rds"))
  expect_warning(
    result_w_c <- data_modelling(
      data,
      outcome_cov = ~ X1 + X2 + X3 + X4 + age_s,
      include_followup_time = ~followup_time,
      include_expansion_time = ~for_period,
      use_sample_weights = FALSE,
      use_weight = TRUE,
      use_censor = TRUE,
      glm_function = "glm",
      quiet = TRUE,
    ),
    "non-integer #successes in a binomial glm!"
  )
  result_formula <- result_w_c$model$formula
  expected_formula <- outcome ~ assigned_treatment + for_period + followup_time + X1 + X2 + X3 + X4 + age_s
  environment(expected_formula) <- environment(result_formula) <- globalenv()
  expect_equal(result_formula, expected_formula)
})

test_that("data_modelling makes model formula as expected with no weight and no censor", {
  data <- readRDS(test_path("data/prep_data_object.rds"))
  result_w_c <- data_modelling(
    data,
    outcome_cov = ~ X1 + X2 + X3 + X4 + age_s,
    include_followup_time = ~followup_time,
    include_expansion_time = ~for_period,
    use_sample_weights = FALSE,
    use_weight = FALSE,
    use_censor = FALSE,
    glm_function = "glm",
    quiet = TRUE,
  )
  result_formula <- result_w_c$model$formula
  expected_formula <- outcome ~ assigned_treatment + for_period + followup_time + X1 + X2 + X3 + X4 + age_s
  environment(expected_formula) <- environment(result_formula) <- globalenv()
  expect_equal(result_formula, expected_formula)
})

test_that("data_modelling makes model formula as expected with weight and no censor", {
  set.seed(20222022)
  simdata_censored <- data_gen_censored(1000, 10)
  prep_PP_data <- data_preparation(
    data = simdata_censored,
    id = "ID",
    period = "t",
    treatment = "A",
    outcome = "Y",
    eligible = "eligible",
    outcome_cov = ~ X1 + X2,
    use_weight = TRUE,
    use_censor = FALSE,
    switch_d_cov = ~ X1 + X2 + X3 + X4 + age_s,
    switch_n_cov = ~ X3 + X4,
    separate_files = FALSE,
    last_period = 8,
    first_period = 2,
    where_var = "age",
    quiet = TRUE
  )
  expect_warning(
    result_w_c <- data_modelling(
      prep_PP_data,
      outcome_cov = ~ X1 + X2,
      include_followup_time = ~followup_time,
      include_expansion_time = ~for_period,
      use_sample_weights = FALSE,
      use_weight = TRUE,
      use_censor = FALSE,
      glm_function = "glm",
      quiet = TRUE,
    ),
    "non-integer #successes in a binomial glm!"
  )
  result_formula <- result_w_c$model$formula
  expected_formula <- outcome ~ dose + I(dose^2) + for_period + followup_time + X1 + X2
  environment(expected_formula) <- environment(result_formula) <- globalenv()
  expect_equal(result_formula, expected_formula)
})

test_that("data_modelling makes model formula as expected with no weight and censor", {
  data <- readRDS(test_path("data/prep_data_object.rds"))
  result_w_c <- data_modelling(
    data,
    outcome_cov = ~ X1 + X2 + X3 + X4 + age_s,
    include_followup_time = ~followup_time,
    include_expansion_time = ~for_period,
    use_sample_weights = FALSE,
    use_weight = FALSE,
    use_censor = TRUE,
    glm_function = "glm",
    quiet = TRUE,
  )

  result_formula <- result_w_c$model$formula
  expected_formula <- outcome ~ assigned_treatment + for_period + followup_time + X1 + X2 + X3 + X4 + age_s
  environment(expected_formula) <- environment(result_formula) <- globalenv()
  expect_equal(result_formula, expected_formula)
})
