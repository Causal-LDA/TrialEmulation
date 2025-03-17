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

test_that("show works for trial_sequence_PP with data and outcome_model set", {
  result <- trial_sequence("PP") |>
    set_data(data_censored) |>
    set_outcome_model()
  expect_snapshot(show(result))
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

# Access data -----

test_that("ipw_data works as expected", {
  ts <- trial_sequence("ITT") |>
    set_data(
      data = trial_example[1:500, ],
      id = "id",
      period = "period",
      eligible = "eligible",
      treatment = "treatment"
    )
  expect_equal(ipw_data(ts), ts@data@data)

  new_data <- copy(ipw_data(ts))
  new_data$sq_nvarC <- new_data$nvarC^2
  ipw_data(ts) <- new_data
  expect_equal(ipw_data(ts), new_data)
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

test_that("time_on_sequence doesn't get included in numerator", {
  expect_error(
    trial_sequence("PP") |>
      set_data(data_censored) |>
      set_switch_weight_model(
        numerator = ~ age_s + x4 + time_on_regime,
        denominator = ~ age_s + x4 + x2 + x1 + time_on_regime,
        model_fitter = stats_glm_logit(save_dir)
      ),
    "time_on_regime"
  )
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

test_that("time_on_sequence doesn't get included in numerator", {
  expect_error(
    trial_sequence("PP") |>
      set_data(data_censored) |>
      set_censor_weight_model(
        censor_event = "censored",
        numerator = ~ age_s + x4 + time_on_regime,
        denominator = ~ age_s + x4 + x2 + x1 + time_on_regime,
        pool_models = "both",
        model_fitter = stats_glm_logit(save_dir)
      ),
    "time_on_regime"
  )
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

test_that("stabilised weight terms are included in outcome model", {
  result <- trial_sequence("PP") |>
    set_data(data_censored) |>
    set_outcome_model(adjustment_terms = c("age", "x2"), trial_period_terms = ~trial_period)


  ex <- "outcome ~ assigned_treatment + age + x2 + followup_time + I(followup_time^2) + trial_period"
  expect_equal(
    as.character(result@outcome_model@formula),
    ex
  )

  result <- result |> set_switch_weight_model(
    numerator = ~x3,
    denominator = ~ x3 + x4,
    model_fitter = stats_glm_logit(NA_character_)
  )

  # adds x3 term from numerator
  expect_equal(
    as.character(result@outcome_model@formula),
    paste(ex, "+ x3")
  )

  result <- result |> set_censor_weight_model(
    numerator = ~x4,
    denominator = ~ x1 + x4,
    censor_event = "censored",
    model_fitter = stats_glm_logit(NA_character_)
  )

  # adds x4 term from numerator
  expect_equal(
    as.character(result@outcome_model@formula),
    paste(ex, "+ x4 + x3")
  )
})


test_that("interaction terms work as expected", {
  result <- trial_sequence("PP") |>
    set_data(data_censored) |>
    set_outcome_model(adjustment_terms = ~ assigned_treatment * x2)

  expect_equal(
    result@outcome_model@formula,
    outcome ~ assigned_treatment + x2 + followup_time + I(followup_time^2) +
      trial_period + I(trial_period^2) + assigned_treatment:x2,
    ignore_formula_env = TRUE
  )
  expect_equal(result@outcome_model@treatment_var, "assigned_treatment")
  expect_equal(result@outcome_model@adjustment_vars, "x2") # shouldn't include treatment
  expect_equal(
    result@outcome_model@adjustment_terms, # can include treatment
    ~ assigned_treatment * x2,
    ignore_formula_env = TRUE
  )
})

# Expand ---

test_that("weights are 1 when not calculated by calculate_weights", {
  trial_ex <- TrialEmulation::trial_example

  itt <- trial_sequence("ITT") |>
    set_data(trial_ex) |>
    set_outcome_model(
      ~ catvarA + catvarB,
      followup_time_terms = ~followup_time,
      trial_period_terms = ~trial_period
    ) |>
    set_expansion_options(save_to_datatable(), chunk_size = 500) |>
    expand_trials() |>
    load_expanded_data()
  expect_numeric(itt@data@data$wt, lower = 1, upper = 1)
  expect_numeric(outcome_data(itt)$weight, lower = 1, upper = 1)
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


test_that("load_expanded_data works with trial_sequence objects containing te_datastore_datatable objects", {
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

  trial_itt_datatable <- set_expansion_options(
    trial_itt,
    output = save_to_datatable(),
    chunk_size = 500
  ) |>
    expand_trials()

  # load_expanded_data works without additional arguments
  sc_00 <- load_expanded_data(trial_itt_datatable)
  expect_equal(sc_00@outcome_data@n_rows, 1558)

  # load_expanded_data works with p_control
  sc_01 <- load_expanded_data(trial_itt_datatable, p_control = 0.01, seed = 1221)
  expect_equal(
    sort(sc_01@outcome_data@data$id),
    c(
      10, 14, 14, 15, 17, 21, 27, 29, 32, 38, 38, 44, 44, 49, 49,
      54, 54, 54, 59, 61, 68, 71, 71, 71, 74, 74, 89, 98, 98, 99
    )
  )

  random_01 <- load_expanded_data(trial_itt_datatable, p_control = 0.01)

  # seed gets reset
  sc_01_1 <- load_expanded_data(trial_itt_datatable, p_control = 0.01, seed = 1221)
  random_02 <- load_expanded_data(trial_itt_datatable, p_control = 0.01)
  expect_false(identical(sort(random_01@outcome_data@data$id), sort(random_02@outcome_data@data$id)))

  # load_expanded_data works with p_control
  sc_02 <- load_expanded_data(trial_itt_datatable, p_control = 0.5, seed = 5678)
  expect_equal(sc_02@outcome_data@n_rows, 765)

  # load_expanded_data works with p_control = 0
  sc_03 <- load_expanded_data(trial_itt_datatable, p_control = 0)
  expect_equal(sc_03@outcome_data@n_rows, 14)

  # cases are kept
  expect_equal(sum(sc_00@outcome_data@data$outcome), 14)
  expect_equal(sum(sc_01@outcome_data@data$outcome), 14)
  expect_equal(sum(sc_02@outcome_data@data$outcome), 14)
  expect_equal(sum(sc_03@outcome_data@data$outcome), 14)

  # all columns are kept and sample_weight column is added
  expect_equal(
    colnames(sc_00@outcome_data@data), c(colnames(trial_itt_datatable@expansion@datastore@data), "sample_weight")
  )
  expect_equal(sort(sc_00@outcome_data@data$sample_weight), rep(1, 1558))

  expect_equal(
    colnames(sc_01@outcome_data@data), c(colnames(trial_itt_datatable@expansion@datastore@data), "sample_weight")
  )

  # load_expanded_data subsets data correctly with p_control
  sc_04 <- load_expanded_data(
    trial_itt_datatable,
    period = 1:10,
    subset_condition = "followup_time <= 20 & treatment == 1",
    p_control = 0.2,
    seed = 2332
  )
  expect_equal(
    sort(sc_04@outcome_data@data$id),
    c(
      14, 16, 20, 27, 27, 33, 33, 33, 33, 34, 34, 34, 44, 44, 44, 44, 44, 44, 44, 44, 47, 54, 54, 54, 54,
      59, 59, 59, 59, 59, 59, 59, 60, 60, 60, 65, 71, 73, 74, 74, 74, 83, 95, 95, 95, 95, 95, 95, 95, 96
    )
  )

  # load_expanded_data subsets data correctly without p_control
  sc_05 <- load_expanded_data(
    trial_itt_datatable,
    period = 1:10,
    subset_condition = "followup_time <= 20 & treatment == 1",
  )
  expect_equal(
    sort(sc_05@outcome_data@data$id),
    c(
      2, 2, 2, 14, 14, 14, 14, 14, 14, 16, 16, 16, 16, 20, 20, 20, 20, 21, 21, 21, 21, 21, 21, 21, 21, 21, 27, 27, 27,
      27, 27, 27, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 34, 34, 34, 34, 34, 34, 34, 34, 38, 38,
      44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44,
      44, 44, 44, 44, 44, 44, 44, 44, 47, 47, 47, 49, 49, 49, 50, 50, 50, 50, 53, 53, 53, 53, 53, 53, 53, 54, 54, 54,
      54, 54, 54, 54, 54, 54, 54, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59,
      59, 59, 59, 59, 59, 59, 59, 60, 60, 60, 60, 60, 60, 60, 60, 65, 65, 65, 65, 70, 70, 70, 71, 71, 71, 71, 71, 71,
      71, 71, 73, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 83, 83, 83, 95, 95, 95, 95, 95, 95,
      95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
      95, 95, 96, 96, 96, 96, 96, 96
    )
  )

  # load_expanded_data returns the correct classes
  expect_class(sc_04, "trial_sequence_ITT")
  expect_class(sc_04@outcome_data, "te_outcome_data")
  expect_class(sc_04@outcome_data@data, "data.table")

  unlink(trial_itt_dir, recursive = TRUE)
})
