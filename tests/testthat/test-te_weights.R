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


test_that("weight_model_data_indices works", {
  trial_pp <- trial_sequence("PP") |>
    set_data(data_censored) |>
    set_switch_weight_model(
      numerator = ~age,
      denominator = ~ age + x1 + x3,
      model_fitter = stats_glm_logit(tempdir())
    ) |>
    calculate_weights()

  result <- weight_model_data_indices(trial_pp, "switch", "d0")

  expect_logical(result[[1]])
  expect_equal(sum(result[[1]]), 170L)
  expect_equal(mean(result[[1]]), 0.52959502)

  rle_result <- rle(result[[1]])
  data.frame(values = rle_result$values, rle_result$lengths)
  expect_snapshot(data.frame(values = rle_result$values, lengths = rle_result$lengths))

  result2 <- weight_model_data_indices(trial_pp, "switch", "d0", set_col = "sw_d0")
  expect_data_table(result2)
  expect_logical(result2$sw_d0)
  expect_equal(sum(result2$sw_d0), 170L)
  expect_equal(mean(result2$sw_d0), 0.52959502)

  result3 <- weight_model_data_indices(trial_pp, "switch", "d1", set_col = "sw_d1")
  expect_data_table(result3)
  expect_logical(result3$sw_d1)
  expect_equal(sum(result3$sw_d1), 151L)
  expect_equal(mean(result3$sw_d1), 0.47040498)
})

test_that("weight_model_data_indices catches bad input", {
  trial_pp <- trial_sequence("PP") |>
    set_data(data_censored) |>
    set_switch_weight_model(
      numerator = ~age,
      denominator = ~ age + x1 + x3,
      model_fitter = stats_glm_logit(tempdir())
    ) |>
    calculate_weights()

  expect_error(
    weight_model_data_indices(trial_pp, "sw", "d0"),
    "type"
  )

  expect_error(
    weight_model_data_indices(trial_pp, "switch", "d__0"),
    "model"
  )

  expect_error(
    weight_model_data_indices(trial_pp, "switch", "d0", set_col = 99),
    "set_col"
  )
})
