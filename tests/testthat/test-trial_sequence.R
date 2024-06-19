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

# Set Switching Model -------

# Set Censoring Model -------

# Set Outcome Model -----
test_that("set_outcome_model works for PP with defaults", {
  result <- trial_sequence("PP") |>
    set_data(data_censored) |>
    set_outcome_model()

  expect_class(result, "trial_sequence_PP")
  expect_class(result@outcome_model, "te_outcome_model")
  expect_formula(result@outcome_model@formula)
  expect_equal(
    as.character(result@outcome_model@formula),
    "~assigned_treatment + followup_time + I(followup_time^2) + trial_period + I(trial_period^2)"
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
    "~assigned_treatment + followup_time + I(followup_time^2) + trial_period + I(trial_period^2)"
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
    "~dose + followup_time + I(followup_time^2) + trial_period + I(trial_period^2)"
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
    "~assigned_treatment + age + x2 + followup_time + I(followup_time^2) + trial_period + I(trial_period^2)"
  )
})
