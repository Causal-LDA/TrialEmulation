test_that("parsnip_model works", {
  skip_if_not_installed("parsnip")
  library(parsnip)

  spec <- parsnip::logistic_reg() |>
    set_mode("classification")

  result <- parsnip_model(
    model_spec = spec,
    save_path = tempdir()
  )
  expect_class(result, "te_parsnip_model")
  expect_equal(result@model_spec, spec)
})

test_that("parsnip_model fails for invalid model_specifications", {
  skip_if_not_installed("parsnip")
  library(parsnip)
  tmp <- tempdir()

  # regression mode
  spec <- parsnip::decision_tree(tree_depth = 30) |>
    set_mode("regression") |>
    set_engine("rpart")
  expect_error(parsnip_model(model_spec = spec, save_path = tmp), "mode")

  # no mode set
  spec2 <- parsnip::decision_tree(tree_depth = 30) |>
    set_engine("rpart")
  expect_error(parsnip_model(model_spec = spec2, save_path = tmp), "mode")

  # not a parsnip model_spec
  spec3 <- list(1:10)
  expect_error(parsnip_model(model_spec = spec3, save_path = tmp), "model_spec")
})

test_that("fit_weights_model works for parsnip models", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("rpart")
  library(parsnip)
  set.seed(12345)
  save_dir <- withr::local_tempdir(pattern = "model_fitter", tempdir(TRUE))

  spec <- parsnip::decision_tree(tree_depth = 5) |>
    set_mode("classification") |>
    set_engine("rpart")

  object <- parsnip_model(model_spec = spec, save_path = save_dir)
  result <- fit_weights_model(object, data = data_censored, formula = treatment ~ age, "test_model")

  expect_class(result, "te_weights_fitted")
  expect_equal(result@label, "test_model")

  expect_numeric(result@fitted, len = 725, any.missing = FALSE)
  expect_equal(
    summary(result@fitted) |> unclass(),
    c(
      Min. = 0.317343173431734, `1st Qu.` = 0.317343173431734, Median = 0.475247524752475,
      Mean = 0.467586206896552, `3rd Qu.` = 0.561224489795918, Max. = 0.662337662337662
    )
  )
  expect_file_exists(result@summary$save_path$path)
  saved_model <- readRDS(result@summary$save_path$path)
  expect_class(saved_model, c("_rpart", "model_fit"))
})

test_that("fit_weights_model works parsnip logistic regression", {
  skip_if_not_installed("parsnip")
  library(parsnip)
  set.seed(12345)
  save_dir <- withr::local_tempdir(pattern = "model_fitter", tempdir(TRUE))

  spec <- parsnip::logistic_reg() |>
    set_mode("classification") |>
    set_engine("glm")

  object <- parsnip_model(model_spec = spec, save_path = save_dir)
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
  saved_model <- readRDS(result@summary$save_path$path)
  expect_class(saved_model, c("_glm", "model_fit"))
})
