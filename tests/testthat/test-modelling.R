# trial_msm ----
test_that("trial_msm can be quiet", {
  data("te_data_ex")
  expect_silent(
    result <- trial_msm(
      data = te_data_ex,
      outcome_cov = c("catvarA", "nvarA"),
      model_var = "assigned_treatment",
      include_followup_time = ~followup_time,
      include_trial_period = ~trial_period,
      use_sample_weights = FALSE,
      quiet = TRUE,
      glm_function = "glm"
    )
  )
})

test_that("trial_msm gives expected results in example data", {
  data <- vignette_switch_data
  result <- trial_msm(
    data,
    outcome_cov = c("catvarA", "catvarB", "catvarC", "nvarA", "nvarB", "nvarC"),
    model_var = "assigned_treatment",
    include_followup_time = ~followup_time,
    include_trial_period = ~trial_period,
    use_sample_weights = FALSE,
    quiet = TRUE,
    glm_function = "parglm",
    control = parglm.control(nthreads = 2)
  )
  expect_class(result$model, "glm")
  expected_coefs <- c(
    `(Intercept)` = -3.44513044265409,
    assigned_treatment = -0.279511046771487,
    trial_period = 0.00193709391469456,
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
      "(Intercept)", "assigned_treatment", "trial_period", "followup_time",
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


test_that("trial_msm works with data.tables and weights", {
  data <- as.data.table(TrialEmulation::vignette_switch_data)
  expect_silent(
    result_parglm <- trial_msm(
      data,
      outcome_cov = c("catvarA", "nvarA"),
      model_var = "assigned_treatment",
      include_followup_time = ~followup_time,
      include_trial_period = ~trial_period,
      use_sample_weights = FALSE,
      analysis_weights = "asis",
      glm_function = "parglm",
      control = parglm.control(nthreads = 2, method = "FAST"),
      quiet = TRUE
    )
  )

  expect_silent(
    result_glm <- trial_msm(
      data,
      outcome_cov = c("catvarA", "nvarA"),
      model_var = "assigned_treatment",
      include_followup_time = ~followup_time,
      include_trial_period = ~trial_period,
      use_sample_weights = FALSE,
      analysis_weights = "asis",
      glm_function = "glm",
      quiet = TRUE
    )
  )
  expect_equal(result_glm$model$coefficients, result_parglm$model$coefficients)
})


test_that("Modelling works with where_case", {
  skip_on_cran()
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
      use_censor_weights = TRUE,
      estimand_type = "PP",
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
    result <- trial_msm(
      data = data,
      outcome_cov = ~ X1 + X2 + X3 + X4 + age_s,
      model_var = "assigned_treatment",
      analysis_weights = "asis",
      include_followup_time = ~ factor(followup_time),
      include_trial_period = ~1,
      glm_function = c("glm"),
      use_sample_weights = FALSE,
      first_followup = 0,
      last_followup = 4,
      where_case = "age > 30",
      quiet = TRUE
    ),
    "non-integer #successes in a binomial glm"
  )
  expect_class(result$model, "glm")
  expect_snapshot_value(as.data.frame(result$robust$summary), style = "json2")
})

test_that("trial_msm works with analysis_weights = unweighted", {
  skip_on_cran()
  data <- readRDS(test_path("data/ready_for_modelling.rds"))

  expect_silent(
    result_unweighted <- trial_msm(
      data,
      outcome_cov = ~ X1 + X2 + X3 + X4 + age_s,
      model_var = "assigned_treatment",
      include_followup_time = ~followup_time,
      include_trial_period = ~trial_period,
      use_sample_weights = FALSE,
      glm_function = "glm",
      quiet = TRUE,
      analysis_weights = "unweighted"
    )
  )

  expect_snapshot_value(as.data.frame(result_unweighted$robust$summary), style = "json2")
})

test_that("trial_msm works with analysis_weights = p99", {
  skip_on_cran()
  data <- readRDS(test_path("data/ready_for_modelling.rds"))

  expect_warning(
    result_p99 <- trial_msm(
      data,
      outcome_cov = ~ X1 + X2 + X3 + X4 + age_s,
      model_var = "assigned_treatment",
      include_followup_time = ~followup_time,
      include_trial_period = ~trial_period,
      use_sample_weights = FALSE,
      glm_function = "glm",
      quiet = TRUE,
      analysis_weights = "p99"
    ),
    "non-integer #successes in a binomial glm!"
  )
  expect_snapshot_value(as.data.frame(result_p99$robust$summary), style = "json2")

  quantiles <- quantile(data$weight, prob = c(0.01, 0.99), type = 1)
  expect_equal(quantiles, c(`1%` = 0.264964755418739, `99%` = 1.67299290397343))

  w <- data$weight
  w[w > quantiles[2]] <- quantiles[2]
  w[w < quantiles[1]] <- quantiles[1]
  expect_equal(result_p99$model$prior.weights, w, ignore_attr = "names")
})


test_that("trial_msm works with analysis_weights = weight_limits", {
  skip_on_cran()
  data <- readRDS(test_path("data/ready_for_modelling.rds"))

  expect_warning(
    result_limits <- trial_msm(
      data,
      outcome_cov = ~ X1 + X2 + X3 + X4 + age_s,
      model_var = "assigned_treatment",
      include_followup_time = ~followup_time,
      include_trial_period = ~trial_period,
      use_sample_weights = FALSE,
      glm_function = "glm",
      quiet = TRUE,
      analysis_weights = "weight_limits",
      weight_limits = c(0, Inf)
    ),
    "non-integer #successes in a binomial glm!"
  )
  expect_snapshot_value(as.data.frame(result_limits$robust$summary), style = "json2")
})



test_that("trial_msm works with missing sample weights", {
  skip_on_cran()
  data <- readRDS(test_path("data/ready_for_modelling.rds"))
  expect_warning(
    expect_warning(
      result_sample <- trial_msm(
        data,
        outcome_cov = ~ X1 + X2 + X3 + X4 + age_s,
        model_var = "assigned_treatment",
        include_followup_time = ~followup_time,
        include_trial_period = ~trial_period,
        analysis_weights = "asis",
        glm_function = "glm",
        quiet = TRUE,
      ),
      "non-integer #successes in a binomial glm!"
    ),
    "'sample_weight' column not found in data. Using sample weights = 1."
  )

  expect_warning(
    expected_result <- trial_msm(
      data,
      outcome_cov = ~ X1 + X2 + X3 + X4 + age_s,
      model_var = "assigned_treatment",
      include_followup_time = ~followup_time,
      include_trial_period = ~trial_period,
      use_sample_weights = FALSE,
      analysis_weights = "asis",
      glm_function = "glm",
      quiet = TRUE,
    ),
    "non-integer #successes in a binomial glm!"
  )
  expect_equal(result_sample$robust$summary, expected_result$robust$summary)
})

test_that("trial_msm works with sample weights", {
  skip_on_cran()
  data <- readRDS(test_path("data/prep_data_object.rds"))
  set.seed(2020)
  sampled_data <- case_control_sampling_trials(data, p_control = 0.5)

  expect_warning(
    result_sample <- trial_msm(
      sampled_data,
      outcome_cov = ~ X1 + X2 + X3 + X4 + age_s,
      model_var = "assigned_treatment",
      include_followup_time = ~followup_time,
      include_trial_period = ~trial_period,
      use_sample_weights = TRUE,
      analysis_weights = "asis",
      glm_function = "glm",
      quiet = TRUE,
    ),
    "non-integer #successes in a binomial glm!"
  )
  expect_snapshot_value(as.data.frame(result_sample$robust$summary), style = "json2")
})


test_that("trial_msm makes model formula as expected with weight and censor", {
  skip_on_cran()
  data <- readRDS(test_path("data/prep_data_object.rds"))
  expect_warning(
    result_w_c <- trial_msm(
      data,
      outcome_cov = ~ X1 + X2 + X3 + X4 + age_s,
      include_followup_time = ~followup_time,
      include_trial_period = ~trial_period,
      use_sample_weights = FALSE,
      analysis_weights = "asis",
      estimand_type = "PP",
      glm_function = "glm",
      quiet = TRUE,
    ),
    "non-integer #successes in a binomial glm!"
  )
  result_formula <- result_w_c$model$formula
  expected_formula <- outcome ~ assigned_treatment + trial_period + followup_time + X1 + X2 + X3 + X4 + age_s
  environment(expected_formula) <- environment(result_formula) <- globalenv()
  expect_equal(result_formula, expected_formula)
})


test_that("trial_msm makes model formula as expected with estimand As-Treated", {
  skip_on_cran()
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
    estimand_type = "As-Treated",
    pool_cense = "none",
    use_censor_weights = FALSE,
    switch_d_cov = ~ X1 + X2 + X3 + X4 + age_s,
    switch_n_cov = ~ X3 + X4,
    separate_files = FALSE,
    last_period = 8,
    first_period = 2,
    where_var = "age",
    quiet = TRUE
  )
  expect_warning(
    result_w_c <- trial_msm(
      prep_PP_data,
      outcome_cov = ~ X1 + X2,
      include_followup_time = ~followup_time,
      include_trial_period = ~trial_period,
      use_sample_weights = FALSE,
      analysis_weights = "asis",
      estimand_type = "As-Treated",
      glm_function = "glm",
      quiet = TRUE,
    ),
    "non-integer #successes in a binomial glm!"
  )
  result_formula <- result_w_c$model$formula
  expected_formula <- outcome ~ dose + I(dose^2) + trial_period + followup_time + X1 + X2
  environment(expected_formula) <- environment(result_formula) <- globalenv()
  expect_equal(result_formula, expected_formula)
})

test_that("trial_msm makes model formula as expected with estimand_type ITT and unweighted", {
  skip_on_cran()
  data <- readRDS(test_path("data/prep_data_object.rds"))
  result_w_c <- trial_msm(
    data,
    outcome_cov = ~ X1 + X2 + X3 + X4 + age_s,
    include_followup_time = ~followup_time,
    include_trial_period = ~trial_period,
    estimand = "ITT",
    use_sample_weights = FALSE,
    analysis_weights = "unweighted",
    glm_function = "glm",
    quiet = TRUE,
  )

  result_formula <- result_w_c$model$formula
  expected_formula <- outcome ~ assigned_treatment + trial_period + followup_time + X1 + X2 + X3 + X4 + age_s
  environment(expected_formula) <- environment(result_formula) <- globalenv()
  expect_equal(result_formula, expected_formula)
})


test_that("fit_msm works", {
  trial_itt_dir <- file.path(tempdir(), "trial_itt")
  dir.create(trial_itt_dir)

  trial_itt <- trial_sequence(estimand = "ITT") |>
    set_data(
      data = data_censored,
      id = "id",
      period = "period",
      treatment = "treatment",
      outcome = "outcome",
      eligible = "eligible"
    ) |>
    set_censor_weight_model(
      censor_event = "censored",
      numerator = ~ x1 + x2 + x3,
      denominator = ~x2,
      pool_models = "numerator",
      model_fitter = stats_glm_logit(save_path = file.path(trial_itt_dir, "switch_models"))
    ) |>
    calculate_weights() |>
    set_outcome_model(adjustment_terms = ~ x1 + x2)

  trial_itt_expanded <- set_expansion_options(
    trial_itt,
    output = save_to_csv(file.path(trial_itt_dir, "trial_csvs")),
    chunk_size = 500
  ) |>
    expand_trials() |>
    load_expanded_data()

  # fit_msm returns a trial_sequence object
  expect_warning(
    expect_warning(
      {
        fm_01 <- fit_msm(trial_itt_expanded, analysis_weights = "asis")
      },
      "non-integer"
    ),
    "fitted probabilities numerically 0 or 1 occurred"
  )

  expect_class(fm_01, "trial_sequence")

  # all columns in @outcome_data@data are kept and w column is added
  expect_equal(
    colnames(fm_01@outcome_data@data), c(colnames(trial_itt_expanded@outcome_data@data), "w")
  )

  # fit_msm saves result into @outcome_model@fitted
  expect_class(fm_01@outcome_model@fitted@model$model, "glm")

  unlink(trial_itt_dir, recursive = TRUE)
})
