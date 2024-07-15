test_that("show works for te_weights_spec objects", {
  save_dir <- withr::local_tempdir(pattern = "set_switch_weight_model", tempdir(TRUE))
  object <- trial_sequence("PP") |>
    set_data(data_censored) |>
    set_censor_weight_model(
      numerator = ~ age + x4,
      denominator = ~ age + x2 + x4,
      pool_models = "numerator",
      censor_event = "censored",
      model_fitter = stats_glm_logit(save_dir)
    ) |>
    set_switch_weight_model(
      numerator = ~ age + x4,
      denominator = ~ age + x2 + x4,
      model_fitter = stats_glm_logit(save_dir)
    )



  expect_snapshot(show_weight_models(object))
  expect_snapshot(show(object@censor_weights))

  object_w_weights <- calculate_weights(object)

  # set dummy paths for the sake of snapshot
  object_w_weights@censor_weights@fitted$n@summary$save_path$path <- "/tempdir/model_n.rds"
  object_w_weights@censor_weights@fitted$d0@summary$save_path$path <- "/tempdir/model_d0.rds"
  object_w_weights@censor_weights@fitted$d1@summary$save_path$path <- "/tempdir/model_d1.rds"

  object_w_weights@switch_weights@fitted$n0@summary$save_path$path <- "/tempdir/model_n0.rds"
  object_w_weights@switch_weights@fitted$n1@summary$save_path$path <- "/tempdir/model_n1.rds"
  object_w_weights@switch_weights@fitted$d0@summary$save_path$path <- "/tempdir/model_d0.rds"
  object_w_weights@switch_weights@fitted$d1@summary$save_path$path <- "/tempdir/model_d1.rds"


  expect_snapshot(show(object_w_weights))

  expect_snapshot(show_weight_models(object_w_weights))

  expect_snapshot(show(object_w_weights@switch_weights@fitted$n1))
})
