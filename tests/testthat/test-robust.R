test_that("robust_calculation works as expected", {
  object <- glm(
    outcome ~ trial_period + followup_time + assigned_treatment,
    data = vignette_switch_data
  )

  result <- robust_calculation(object, vignette_switch_data$id)

  expect_list(result, len = 2)

  expected_summary <- data.frame(
    names = c("(Intercept)", "trial_period", "followup_time", "assigned_treatment"),
    estimate = c(0.00136295054903676, 1.38831096405389e-05, 6.66873154987971e-06, -0.000537493694584342),
    robust_se = c(0.00125180530919543, 5.41292248056937e-06, 6.51930925412827e-06, 0.00140485643532218),
    `2.5%` = c(-0.00109058785698629, 3.27378157862297e-06, -6.10911458821171e-06, -0.00329101230781582),
    `97.5%` = c(0.0038164889550598, 2.44924377024549e-05, 1.94465776879711e-05, 0.00221602491864713),
    z = c(1.08878796009642, 2.56480850970521, 1.02291995822361, -0.382596883973469),
    p_value = c(0.276247402202119, 0.0103232760157177, 0.306345692136839, 0.702018681018101),
    check.names = FALSE
  )

  expect_identical(result$summary, expected_summary, tolerance = 1e-10)

  expected_matrix <- matrix(
    c(
      1.56701653212986e-06, -6.16533629055698e-09, -5.36729872715042e-09,
      1.76020378803953e-07, -6.16533629055698e-09, 2.92997297806532e-11,
      1.70614513737659e-11, -1.61600587370185e-09, -5.36729872715043e-09,
      1.7061451373766e-11, 4.25013931509625e-11, -3.77624857145736e-10,
      1.76020378803953e-07, -1.61600587370185e-09, -3.77624857145733e-10,
      1.97362160386615e-06
    ),
    nrow = 4,
    ncol = 4,
    dimnames = list(
      c("(Intercept)", "trial_period", "followup_time", "assigned_treatment"),
      c("(Intercept)", "trial_period", "followup_time", "assigned_treatment")
    )
  )

  expect_identical(result$matrix, expected_matrix, tolerance = 1e-10)
})
