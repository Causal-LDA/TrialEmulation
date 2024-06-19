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
