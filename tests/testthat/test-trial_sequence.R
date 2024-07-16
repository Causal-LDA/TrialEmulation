# constructor -------

test_that("trial_sequence creates an object for per-protocol", {
  result <- trial_sequence("PP")
  expect_class(result, "trial_sequence_PP")
  expect_equal(result@estimand, "Per-protocol")
})

test_that("trial_sequence creates an object for itt", {
  result <- trial_sequence("ITT")
  expect_class(result, "trial_sequence_ITT")
  expect_equal(result@estimand, "Intention-to-treat")
})

test_that("trial_sequence creates an object for as-treated", {
  result <- trial_sequence("AT")
  expect_class(result, "trial_sequence_AT")
  expect_equal(result@estimand, "As treated")
})


test_that("trial_sequence errors for unknown class", {
  expect_error(trial_sequence("AP"), "not a defined class")
  setClass("AP2")
  expect_error(trial_sequence("AP2"), "does not extend")
})

test_that("trial_sequence works for a new class", {
  setClass("trial_sequence_composite",
    contains = "trial_sequence",
    prototype = list(estimand = "composite")
  )
  result <- trial_sequence("trial_sequence_composite")
  expect_class(result, "trial_sequence_composite")
  expect_equal(result@estimand, "composite")
})

# Show ----

test_that("show works for trial_sequence_PP with nothing set", {
  expect_snapshot(show(trial_sequence("PP")))
})

test_that("show works for trial_sequence_ITT with nothing set", {
  expect_snapshot(show(trial_sequence("ITT")))
})

test_that("show works for trial_sequence_AT with nothing set", {
  expect_snapshot(show(trial_sequence("AT")))
})
# Set Data ----

test_that("set_data works for trial_sequence_ITT", {
  data(trial_example)
  result <- trial_sequence("ITT") |>
    set_data(
      data = trial_example,
      id = "id",
      period = "period",
      eligible = "eligible",
      treatment = "treatment"
    )
  expect_class(result, "trial_sequence_ITT")
  expect_class(result@data, "te_data")
  expect_data_table(result@data@data, nrows = 48400, ncols = 20)
})

test_that("set_data works for trial_sequence_PP", {
  data(trial_example)
  result <- trial_sequence("PP") |>
    set_data(
      data = trial_example,
      id = "id",
      period = "period",
      eligible = "eligible",
      treatment = "treatment"
    )
  expect_class(result, "trial_sequence_PP")
  expect_class(result@data, "te_data")
  expect_data_table(result@data@data, nrows = 38820, ncols = 20)
})


test_that("set_data works for trial_sequence_AT", {
  data(trial_example)
  result <- trial_sequence("AT") |>
    set_data(
      data = trial_example,
      id = "id",
      period = "period",
      eligible = "eligible",
      treatment = "treatment"
    )
  expect_class(result, "trial_sequence_AT")
  expect_class(result@data, "te_data")
  expect_data_table(result@data@data, nrows = 48400, ncols = 20)
})

test_that("set_data errors if reserved colnames are used", {
  dat <- trial_example
  colnames(dat)[6:10] <- c("wt", "wtC", "weight", "dose", "assigned_treatment")
  expect_error(
    trial_sequence("ITT") |>
      set_data(
        data = dat[1:500, ],
        id = "id",
        period = "period",
        eligible = "eligible",
        treatment = "treatment"
      ),
    fixed = TRUE,
    regexp = "has elements {'wt','wtC','weight','dose','assigned_treatment'}"
  )
})

test_that("set_data errors if column not found", {
  expect_error(
    trial_sequence("ITT") |>
      set_data(
        data = trial_example[1:500, ],
        id = "USUBJID",
        period = "period",
        eligible = "eligible",
        treatment = "treatment"
      ),
    fixed = TRUE,
    regexp = "must include the elements {'USUBJID'"
  )
})

test_that("set_data errors if columns used twice", {
  expect_error(
    trial_sequence("ITT") |>
      set_data(
        data = trial_example[1:500, ],
        id = "id",
        period = "id",
        eligible = "eligible",
        treatment = "eligible"
      ),
    fixed = TRUE,
    regexp = "Duplicate column names specified: period = \"id\", eligible = \"eligible\""
  )
})

# Set Switching Model -------

test_that("set_switch_weight_model fails for ITT", {
  expect_error(
    trial_sequence("ITT") |> set_switch_weight_model(),
    "Switching weights are not supported for intention-to-treat analyses",
    fixed = TRUE
  )
})

test_that("set_switch_weight_model works for PP", {
  save_dir <- withr::local_tempdir(pattern = "set_switch_weight_model", tempdir(TRUE))
  result <- trial_sequence("PP") |>
    set_data(data_censored) |>
    set_switch_weight_model(
      numerator = ~ age_s + x4,
      denominator = ~ age_s + x4 + x2 + x1,
      model_fitter = stats_glm_logit(save_dir)
    )
  expect_class(result, "trial_sequence_PP")
  expect_class(result@switch_weights, "te_weights_spec")
  expect_equal(result@switch_weights@numerator, treatment ~ age_s + x4)
  expect_equal(result@switch_weights@denominator, treatment ~ age_s + x4 + x2 + x1)
  expect_equal(result@switch_weights@fitted, list())
  expect_class(result@switch_weights@model_fitter, "te_stats_glm_logit")
})


# Set Censoring Model -------
test_that("set_censor_weight_model works for ITT", {
  save_dir <- withr::local_tempdir(pattern = "set_censor_weight_model", tempdir(TRUE))
  result <- trial_sequence("ITT") |>
    set_data(data_censored) |>
    set_censor_weight_model(
      censor_event = "censored",
      numerator = ~ age_s + x4,
      denominator = ~ age_s + x4 + x2 + x1,
      pool_models = "both",
      model_fitter = stats_glm_logit(save_dir)
    )
  expect_class(result, "trial_sequence_ITT")
  expect_class(result@censor_weights, "te_weights_spec")
  expect_equal(result@censor_weights@numerator, 1 - censored ~ age_s + x4)
  expect_equal(result@censor_weights@denominator, 1 - censored ~ age_s + x4 + x2 + x1)
  expect_equal(result@censor_weights@fitted, list())
  expect_class(result@censor_weights@model_fitter, "te_stats_glm_logit")
})

test_that("set_censor_weight_model works for PP", {
  save_dir <- withr::local_tempdir(pattern = "set_censor_weight_model", tempdir(TRUE))
  result <- trial_sequence("PP") |>
    set_data(data_censored) |>
    set_censor_weight_model(
      censor_event = "censored",
      numerator = ~ age_s + x4,
      denominator = ~ age_s + x4 + x2 + x1,
      pool_models = "both",
      model_fitter = stats_glm_logit(save_dir)
    )
  expect_class(result, "trial_sequence_PP")
  expect_class(result@censor_weights, "te_weights_spec")
  expect_equal(result@censor_weights@numerator, 1 - censored ~ age_s + x4)
  expect_equal(result@censor_weights@denominator, 1 - censored ~ age_s + x4 + x2 + x1)
  expect_equal(result@censor_weights@fitted, list())
  expect_class(result@censor_weights@model_fitter, "te_stats_glm_logit")
})

# Calculate weights -------

test_that("calculate_weights works for ITT with censor weights", {
  save_dir <- withr::local_tempdir(pattern = "set_censor_weight_model", tempdir(TRUE))
  result <- trial_sequence("ITT") |>
    set_data(data_censored) |>
    set_censor_weight_model(
      censor_event = "censored",
      numerator = ~ age_s + x4,
      denominator = ~ age_s + x4 + x2 + x1,
      pool_models = "both",
      model_fitter = stats_glm_logit(save_dir)
    ) |>
    calculate_weights()

  expect_class(result, "trial_sequence_ITT")

  expect_list(
    result@censor_weights@fitted,
    type = "te_weights_fitted",
    len = 2
  )

  # d fit
  expect_equal(
    result@censor_weights@fitted$d@summary$tidy$estimate,
    c(1.4747677540742, 1.20061999753867, -0.245231810434708, -0.611795279943629, 0.65960003624931)
  )
  expect_file_exists(result@censor_weights@fitted$d@summary$save_path$path)

  # n fit
  expect_equal(
    result@censor_weights@fitted$n@summary$tidy$estimate,
    c(1.70162923393751, 1.16018458629565, -0.213895618898143)
  )
  expect_file_exists(result@censor_weights@fitted$n@summary$save_path$path)

  expect_equal(
    result@data@data$wtC[1:10],
    c(
      1.05418950414145, 0.949183629638363, 0.992634260683877, 1.03453582489379,
      0.970342455507089, 1.02291134103583, 0.842883741601914, 0.843491345399578,
      1.13275199580273, 1.5870030303049
    )
  )
  expect_equal(mean(result@data@data$wtC), 1.0060411717533)
})


test_that("calculate_weights works for PP with switch_weights", {
  save_dir <- withr::local_tempdir(pattern = "set_switch_weight_model", tempdir(TRUE))
  result <- trial_sequence("PP") |>
    set_data(data_censored) |>
    set_switch_weight_model(
      numerator = ~ age_s + x4,
      denominator = ~ age_s + x4 + x2 + x1,
      model_fitter = stats_glm_logit(save_dir)
    ) |>
    calculate_weights()

  expect_class(result, "trial_sequence_PP")
  expect_class(result@switch_weights, "te_weights_spec")
  expect_equal(result@switch_weights@numerator, treatment ~ age_s + x4)
  expect_equal(result@switch_weights@denominator, treatment ~ age_s + x4 + x2 + x1)
  expect_class(result@switch_weights@model_fitter, "te_stats_glm_logit")

  expect_list(
    result@switch_weights@fitted,
    type = "te_weights_fitted",
    len = 4
  )

  # d0 fit
  expect_equal(
    result@switch_weights@fitted$d0@summary$tidy$estimate,
    c(-0.136991067859309, -0.0401389637202793, 1.09582602393862, 0.0968734645076216, 0.613849255481169)
  )
  expect_file_exists(result@switch_weights@fitted$d0@summary$save_path$path)

  # d1 fit
  expect_equal(
    result@switch_weights@fitted$d1@summary$tidy$estimate,
    c(0.67832867618045, -0.192420096200453, 0.853622418757505, 0.324634165340631, 0.792280439051165)
  )
  expect_file_exists(result@switch_weights@fitted$d1@summary$save_path$path)
  # n0 fit
  expect_equal(
    result@switch_weights@fitted$n0@summary$tidy$estimate,
    c(0.120305287326471, 0.012152335320315, 1.10863379169477)
  )
  expect_file_exists(result@switch_weights@fitted$n0@summary$save_path$path)
  # n1 fit
  expect_equal(
    result@switch_weights@fitted$n1@summary$tidy$estimate,
    c(0.747610836399856, -0.168569497152054, 0.793476254342812)
  )
  expect_file_exists(result@switch_weights@fitted$n1@summary$save_path$path)
})

# Compare calculate_weights() with old implementation -----

test_that("calculate_weights works for PP with switch_weights", {
  save_dir <- withr::local_tempdir(pattern = "set_switch_weight_model", tempdir(TRUE))
  result <- trial_sequence("PP") |>
    set_data(data_censored) |>
    set_switch_weight_model(
      numerator = ~ age_s + x4,
      denominator = ~ age_s + x4 + x2 + x1,
      model_fitter = stats_glm_logit(save_dir)
    ) |>
    calculate_weights() |>
    set_outcome_model() |>
    set_expansion_options(output = save_to_datatable(), chunk_size = 1000) |>
    expand_trials()

  result2 <- data_preparation(
    data = data_censored,
    estimand_type = "PP",
    switch_n_cov = ~ age_s + x4,
    switch_d_cov = ~ age_s + x4 + x2 + x1,
  )

  expect_equal(
    result2$data$weight,
    result@expansion@datastore@data$weight
  )
})

# Set Outcome Model -------
test_that("set_outcome_model works for PP with defaults", {
  result <- trial_sequence("PP") |>
    set_data(data_censored) |>
    set_outcome_model()

  expect_class(result, "trial_sequence_PP")
  expect_class(result@outcome_model, "te_outcome_model")
  expect_formula(result@outcome_model@formula)
  expect_equal(
    as.character(result@outcome_model@formula),
    "outcome ~ assigned_treatment + followup_time + I(followup_time^2) + trial_period + I(trial_period^2)"
  )
  expect_class(result@outcome_model@model_fitter, "te_stats_glm_logit")
})

test_that("set_outcome_model works for ITT", {
  result <- trial_sequence("ITT") |>
    set_data(data_censored) |>
    set_outcome_model()

  expect_class(result, "trial_sequence_ITT")
  expect_class(result@outcome_model, "te_outcome_model")
  expect_formula(result@outcome_model@formula)
  expect_equal(
    as.character(result@outcome_model@formula),
    "outcome ~ assigned_treatment + followup_time + I(followup_time^2) + trial_period + I(trial_period^2)"
  )
  expect_class(result@outcome_model@model_fitter, "te_stats_glm_logit")
})

test_that("set_outcome_model works for ATT", {
  result <- trial_sequence("AT") |>
    set_data(data_censored) |>
    set_outcome_model()

  expect_class(result, "trial_sequence_AT")
  expect_class(result@outcome_model, "te_outcome_model")
  expect_formula(result@outcome_model@formula)
  expect_equal(
    as.character(result@outcome_model@formula),
    "outcome ~ dose + followup_time + I(followup_time^2) + trial_period + I(trial_period^2)"
  )
  expect_class(result@outcome_model@model_fitter, "te_stats_glm_logit")
})

test_that("set_outcome_model doesn't work if data isn't set", {
  expect_error(trial_sequence("PP") |> set_outcome_model(), "set_data")
})

test_that("set_outcome_model doesn't work if with unknown variables", {
  expect_error(
    trial_sequence("PP") |>
      set_data(data_censored) |>
      set_outcome_model(adjustment_terms = ~birthday),
    "birthday"
  )
})

test_that("set_outcome_model works with strings", {
  result <- trial_sequence("PP") |>
    set_data(data_censored) |>
    set_outcome_model(adjustment_terms = c("age", "x2"))
  expect_formula(result@outcome_model@formula)
  expect_equal(
    as.character(result@outcome_model@formula),
    "outcome ~ assigned_treatment + age + x2 + followup_time + I(followup_time^2) + trial_period + I(trial_period^2)"
  )
})

# Outcome Data ----

test_that("outcome_data accessor/setter works", {
  data <- readRDS(test_path("data/ready_for_modelling.rds"))
  ts <- trial_sequence("ITT")
  outcome_data(ts) <- data
  expect_equal(ts@outcome_data@n_rows, 1041L)
  expect_equal(ts@outcome_data@n_ids, 181L)
  expect_equal(ts@outcome_data@periods, c(2, 3, 4, 5, 6, 7, 8))
  expect_data_table(ts@outcome_data@data, ncols = 13, nrows = 1041)

  new_data <- outcome_data(ts)
  expect_equal(data, new_data)
})
