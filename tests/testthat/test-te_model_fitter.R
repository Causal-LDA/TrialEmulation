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

  expect_numeric(result@fitted, len = 725, any.missing = FALSE)
  expect_equal(
    summary(result@fitted) |> unclass(),
    c(
      Min. = 0.198680456313765, `1st Qu.` = 0.384837510987622, Median = 0.456463281021062,
      Mean = 0.467586206896568, `3rd Qu.` = 0.55082963205744, Max. = 0.747901620375074
    )
  )

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

test_that("fit_outcome_model works for stats_glm_logit with save_dir", {
  dir <- withr::local_tempdir(pattern = "glm_test")
  object <- stats_glm_logit(save_path = dir)
  data <- data.frame(
    y = rep(c(1, 0, 1, 0), times = c(15, 5, 5, 15)),
    x = rep(c(1, 0), times = c(20, 20))
  )
  result <- fit_outcome_model(
    object,
    data = data,
    formula = y ~ x
  )

  expect_class(result, "te_outcome_fitted")
  expect_class(result, "te_stats_glm_logit_outcome_fitted")

  expect_equal(result@summary[["tidy"]]$estimate, c(-1.0986123, 2.1972246))
  fitted_model <- readRDS(result@summary$save_path$save)
  expect_equal(
    fitted_model,
    result@model$model,
    ignore_function_env = TRUE,
    ignore_formula_env = TRUE
  )
})
