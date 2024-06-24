test_that("stats_glm_logit creates the fitter object", {
  result <- stats_glm_logit(save_path = NA)
  expect_class(result, "te_stats_glm_logit")
})

test_that("stats_glm_logit creates the fitter object with path", {
  result <- stats_glm_logit(save_path = tempdir())
  expect_class(result, "te_stats_glm_logit")
  expect_equal(result@save_path, tempdir())
})


test_that("fit_weights_model works for stats_glm_logit", {
  save_dir <- withr::local_tempdir(pattern = "model_fitter", tempdir(TRUE))
  object <- stats_glm_logit(save_dir)
  result <- fit_weights_model(object, data = data_censored, formula = treatment ~ age, "test_model")

  expect_class(result, "te_weights_fitted")
  expect_equal(result@label, "test_model")
  expect_equal(result@summary[["tidy"]]$estimate, c(1.88674470, -0.04206803))
  expect_equal(result@summary[["glance"]]$df.null, 724)

  expect_numeric(result@fitted, len = 725, any.missing = FALSE, lower = 0.1986804563, upper = 0.7479016204)
  expect_equal(mean(result@fitted), 0.4675862069)

  expect_file_exists(result@summary$save_path$path)
  saved_model <- readRDS(result@summary$save_path$path)
  expect_class(saved_model, "glm")
})


test_that("fit_outcome_model works for stats_glm_logit", {
  object <- stats_glm_logit(NA)
  result <- fit_outcome_model(
    object,
    data = vignette_switch_data,
    formula = outcome ~ assigned_treatment + followup_time + nvarC,
    weights = vignette_switch_data$weight
  )

  expect_class(result, "te_outcome_fitted")
  expect_class(result, "te_stats_glm_logit_outcome_fitted")

  expect_equal(
    result@summary[["tidy"]]$estimate,
    c(-3.23428770614918, 0.0429082229813285, -0.000849656189863374, -0.0370118224213905)
  )

  expect_equal(
    as.data.frame(result@summary[["tidy"]][2, c("conf.low", "conf.high")]),
    data.frame(conf.low = -0.501620572971525, conf.high = 0.587437018934182)
  )
  expect_equal(result@summary[["glance"]]$df.null, 1939052)

  expect_matrix(result@model$vcov, nrows = 4, ncols = 4)
  expect_equal(
    diag(result@model$vcov),
    c(
      `(Intercept)` = 0.145504452458149, assigned_treatment = 0.0771872414783003,
      followup_time = 1.59771524178973e-06, nvarC = 4.40825112262011e-05
    )
  )
})
