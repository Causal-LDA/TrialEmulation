test_that("Same weights recalculated if we use the same weight model coefficients and original dataset", {
  set.seed(887)
  trial_pp_dir <- withr::local_tempdir("trial_pp", tempdir(TRUE))


  trial_pp <- trial_sequence(estimand = "PP") |>
    set_data(
      data = data_censored,
      id = "id",
      period = "period",
      treatment = "treatment",
      outcome = "outcome",
      eligible = "eligible"
    ) |>
    set_switch_weight_model(
      numerator = ~age,
      denominator = ~ age + x1 + x3,
      model_fitter = stats_glm_logit(save_path = file.path(trial_pp_dir, "switch_models"))
    ) |>
    set_censor_weight_model(
      censor_event = "censored",
      numerator = ~x2,
      denominator = ~ x2 + x1,
      pool_models = "none",
      model_fitter = stats_glm_logit(save_path = file.path(trial_pp_dir, "censor_models"))
    ) |>
    calculate_weights() |>
    set_outcome_model(
      model_fitter = stats_glm_logit(save_path = file.path(trial_pp_dir, "outcome_model")),
      adjustment_terms = ~x2
    ) |>
    set_expansion_options(
      output = save_to_datatable(),
      chunk_size = 500
    ) |>
    expand_trials() |>
    load_expanded_data() |>
    fit_msm(
      weight_cols = c("weight")
    ) |>
    suppressMatchingWarnings("fitted probabilities")

  preds <- predict(
    trial_pp,
    newdata = outcome_data(trial_pp)[trial_period == 1, ],
    predict_times = 0:10,
    type = "survival",
  )
  plot(preds$difference$followup_time, preds$difference$survival_diff,
    type = "l", xlab = "Follow up", ylab = "Survival difference"
  )
  lines(preds$difference$followup_time, preds$difference$`2.5%`, type = "l", col = "red", lty = 2)
  lines(preds$difference$followup_time, preds$difference$`97.5%`, type = "l", col = "red", lty = 2)
  result <- weight_func_bootstrap(
    object = trial_pp, remodel = FALSE, quiet = TRUE, boot_idx = unique(trial_pp@data@data$id),
    new_coef_sw_d0 = trial_pp@switch_weights@fitted$d0@summary$tidy$estimate,
    new_coef_sw_d1 = trial_pp@switch_weights@fitted$d1@summary$tidy$estimate,
    new_coef_sw_n0 = trial_pp@switch_weights@fitted$n0@summary$tidy$estimate,
    new_coef_sw_n1 = trial_pp@switch_weights@fitted$n1@summary$tidy$estimate,
    new_coef_c_d0 = trial_pp@censor_weights@fitted$d0@summary$tidy$estimate,
    new_coef_c_d1 = trial_pp@censor_weights@fitted$d1@summary$tidy$estimate,
    new_coef_c_n1 = trial_pp@censor_weights@fitted$n1@summary$tidy$estimate,
    new_coef_c_n0 = trial_pp@censor_weights@fitted$n0@summary$tidy$estimate
  )

  expect_equal(result$data$weight, trial_pp@outcome_data@data$weight)
})

test_that("Same weights refitted if we use original dataset", {
  set.seed(222)
  trial_pp_dir <- withr::local_tempdir("trial_pp", tempdir(TRUE))

  trial_pp <- trial_sequence(estimand = "PP") |>
    set_data(
      data = data_censored,
      id = "id",
      period = "period",
      treatment = "treatment",
      outcome = "outcome",
      eligible = "eligible"
    ) |>
    set_switch_weight_model(
      numerator = ~age,
      denominator = ~ age + x1 + x3,
      model_fitter = stats_glm_logit(save_path = file.path(trial_pp_dir, "switch_models"))
    ) |>
    set_censor_weight_model(
      censor_event = "censored",
      numerator = ~x2,
      denominator = ~ x2 + x1,
      pool_models = "none",
      model_fitter = stats_glm_logit(save_path = file.path(trial_pp_dir, "censor_models"))
    ) |>
    calculate_weights() |>
    set_outcome_model(model_fitter = stats_glm_logit(save_path = file.path(trial_pp_dir, "outcome_model"))) |>
    set_expansion_options(
      output = save_to_datatable(),
      chunk_size = 500
    ) |>
    expand_trials() |>
    load_expanded_data() |>
    fit_msm(
      weight_cols = c("weight")
    ) |>
    suppressMatchingWarnings("fitted probabilities")

  result <- weight_func_bootstrap(
    object = trial_pp,
    remodel = TRUE,
    quiet = TRUE,
    boot_idx = unique(trial_pp@data@data$id)
  )

  expect_equal(result$data$weight, trial_pp@outcome_data@data$weight)
})

test_that("Same weights refitted if we use example bootstrap sample", {
  set.seed(194)
  trial_pp_dir <- withr::local_tempdir("trial_pp", tempdir(TRUE))

  trial_pp <- trial_sequence(estimand = "PP") |>
    set_data(
      data = data_censored,
      id = "id",
      period = "period",
      treatment = "treatment",
      outcome = "outcome",
      eligible = "eligible"
    ) |>
    set_switch_weight_model(
      numerator = ~age,
      denominator = ~ age + x1 + x3,
      model_fitter = stats_glm_logit(save_path = file.path(trial_pp_dir, "switch_models"))
    ) |>
    set_censor_weight_model(
      censor_event = "censored",
      numerator = ~x2,
      denominator = ~ x2 + x1,
      pool_models = "none",
      model_fitter = stats_glm_logit(save_path = file.path(trial_pp_dir, "censor_models"))
    ) |>
    calculate_weights() |>
    set_outcome_model(model_fitter = stats_glm_logit(save_path = file.path(trial_pp_dir, "outcome_model"))) |>
    set_expansion_options(
      output = save_to_datatable(),
      chunk_size = 500
    ) |>
    expand_trials() |>
    load_expanded_data() |>
    fit_msm(
      weight_cols = c("weight")
    ) |>
    suppressMatchingWarnings("fitted probabilities")

  boot_idx <- sort(sample(unique(trial_pp@data@data$id), replace = TRUE))

  extract_idx <- lapply(boot_idx, function(i) which(data_censored$id == i))
  bootstrap_sample <- data_censored[unlist(extract_idx), ]
  bootstrap_sample$id <- rep(seq_along(extract_idx), lengths(extract_idx))


  trial_pp_boot <- trial_sequence(estimand = "PP") |>
    set_data(
      data = bootstrap_sample,
      id = "id",
      period = "period",
      treatment = "treatment",
      outcome = "outcome",
      eligible = "eligible"
    ) |>
    set_switch_weight_model(
      numerator = ~age,
      denominator = ~ age + x1 + x3,
      model_fitter = stats_glm_logit(save_path = file.path(trial_pp_dir, "switch_models"))
    ) |>
    set_censor_weight_model(
      censor_event = "censored",
      numerator = ~x2,
      denominator = ~ x2 + x1,
      pool_models = "none",
      model_fitter = stats_glm_logit(save_path = file.path(trial_pp_dir, "censor_models"))
    ) |>
    calculate_weights() |>
    set_outcome_model(model_fitter = stats_glm_logit(save_path = file.path(trial_pp_dir, "outcome_model"))) |>
    set_expansion_options(
      output = save_to_datatable(),
      chunk_size = 500
    ) |>
    expand_trials() |>
    load_expanded_data() |>
    fit_msm(
      weight_cols = c("weight")
    ) |>
    suppressMatchingWarnings("fitted probabilities")

  result <- weight_func_bootstrap(object = trial_pp, remodel = TRUE, quiet = TRUE, boot_idx = boot_idx)
  result$data <- result$data[order(id, trial_period, followup_time)]

  expect_equal(trial_pp_boot@switch_weights@fitted$d0@summary$tidy$estimate,
    result$switch_models$switch_d0$summary$estimate,
    tolerance = 1e-7
  )
  expect_equal(trial_pp_boot@switch_weights@fitted$d1@summary$tidy$estimate,
    result$switch_models$switch_d1$summary$estimate,
    tolerance = 1e-7
  )
  expect_equal(trial_pp_boot@switch_weights@fitted$n0@summary$tidy$estimate,
    result$switch_models$switch_n0$summary$estimate,
    tolerance = 1e-7
  )
  expect_equal(trial_pp_boot@switch_weights@fitted$n1@summary$tidy$estimate,
    result$switch_models$switch_n1$summary$estimate,
    tolerance = 1e-7
  )

  expect_equal(trial_pp_boot@censor_weights@fitted$d0@summary$tidy$estimate,
    result$censor_models$cens_d0$summary$estimate,
    tolerance = 1e-7
  )
  expect_equal(trial_pp_boot@censor_weights@fitted$d1@summary$tidy$estimate,
    result$censor_models$cens_d1$summary$estimate,
    tolerance = 1e-7
  )
  expect_equal(trial_pp_boot@censor_weights@fitted$n0@summary$tidy$estimate,
    result$censor_models$cens_n0$summary$estimate,
    tolerance = 1e-7
  )
  expect_equal(trial_pp_boot@censor_weights@fitted$n1@summary$tidy$estimate,
    result$censor_models$cens_n1$summary$estimate,
    tolerance = 1e-7
  )

  test_data <- trial_pp_boot@outcome_data@data
  test_data$id <- boot_idx[test_data$id]
  test_data <- test_data[!duplicated(test_data), ]
  test_data$weight_boot <- sapply(test_data$id, function(i) sum(boot_idx == i))
  test_data$weight <- test_data$weight * test_data$weight_boot
  test_data <- merge(
    x = trial_pp@outcome_data@data[, c("id", "trial_period", "followup_time")],
    y = test_data,
    all.x = TRUE
  )
  test_data$weight[is.na(test_data$weight)] <- 0
  test_data <- test_data[order(id, trial_period, followup_time)]

  expect_equal(result$data$weight, test_data$weight, tolerance = 1e-8)
})

test_that("Correct weights recalculated if we use new weight model coefficients and bootstrap sample", {
  set.seed(978)
  trial_pp_dir <- withr::local_tempdir("trial_pp", tempdir(TRUE))

  trial_pp <- trial_sequence(estimand = "PP") |>
    set_data(
      data = data_censored,
      id = "id",
      period = "period",
      treatment = "treatment",
      outcome = "outcome",
      eligible = "eligible"
    ) |>
    set_switch_weight_model(
      numerator = ~age,
      denominator = ~ age + x1 + x3,
      model_fitter = stats_glm_logit(save_path = file.path(trial_pp_dir, "switch_models"))
    ) |>
    set_censor_weight_model(
      censor_event = "censored",
      numerator = ~x2,
      denominator = ~ x2 + x1,
      pool_models = "none",
      model_fitter = stats_glm_logit(save_path = file.path(trial_pp_dir, "censor_models"))
    ) |>
    calculate_weights() |>
    set_outcome_model(model_fitter = stats_glm_logit(save_path = file.path(trial_pp_dir, "outcome_model"))) |>
    set_expansion_options(
      output = save_to_datatable(),
      chunk_size = 500
    ) |>
    expand_trials() |>
    load_expanded_data() |>
    fit_msm(
      weight_cols = c("weight")
    ) |>
    suppressMatchingWarnings("fitted probabilities")
  boot_idx <- sort(sample(unique(trial_pp@data@data$id), replace = TRUE))

  extract_idx <- lapply(boot_idx, function(i) which(data_censored$id == i))
  bootstrap_sample <- data_censored[unlist(extract_idx), ]
  bootstrap_sample$id <- rep(seq_along(extract_idx), lengths(extract_idx))

  trial_pp_boot <- trial_sequence(estimand = "PP") |>
    set_data(
      data = bootstrap_sample,
      id = "id",
      period = "period",
      treatment = "treatment",
      outcome = "outcome",
      eligible = "eligible"
    ) |>
    set_switch_weight_model(
      numerator = ~age,
      denominator = ~ age + x1 + x3,
      model_fitter = stats_glm_logit(save_path = file.path(trial_pp_dir, "switch_models"))
    ) |>
    set_censor_weight_model(
      censor_event = "censored",
      numerator = ~x2,
      denominator = ~ x2 + x1,
      pool_models = "none",
      model_fitter = stats_glm_logit(save_path = file.path(trial_pp_dir, "censor_models"))
    ) |>
    calculate_weights() |>
    set_outcome_model(model_fitter = stats_glm_logit(save_path = file.path(trial_pp_dir, "outcome_model"))) |>
    set_expansion_options(
      output = save_to_datatable(),
      chunk_size = 500
    ) |>
    expand_trials() |>
    load_expanded_data() |>
    fit_msm(
      weight_cols = c("weight")
    ) |>
    suppressMatchingWarnings("fitted probabilities")

  result <- weight_func_bootstrap(
    object = trial_pp, remodel = FALSE, quiet = TRUE, boot_idx = boot_idx,
    new_coef_sw_d0 = trial_pp_boot@switch_weights@fitted$d0@summary$tidy$estimate,
    new_coef_sw_d1 = trial_pp_boot@switch_weights@fitted$d1@summary$tidy$estimate,
    new_coef_sw_n0 = trial_pp_boot@switch_weights@fitted$n0@summary$tidy$estimate,
    new_coef_sw_n1 = trial_pp_boot@switch_weights@fitted$n1@summary$tidy$estimate,
    new_coef_c_d0 = trial_pp_boot@censor_weights@fitted$d0@summary$tidy$estimate,
    new_coef_c_d1 = trial_pp_boot@censor_weights@fitted$d1@summary$tidy$estimate,
    new_coef_c_n1 = trial_pp_boot@censor_weights@fitted$n1@summary$tidy$estimate,
    new_coef_c_n0 = trial_pp_boot@censor_weights@fitted$n0@summary$tidy$estimate
  )

  result$data <- result$data[order(id, trial_period, followup_time)]


  test_data <- trial_pp_boot@outcome_data@data
  test_data$id <- boot_idx[test_data$id]
  test_data <- test_data[!duplicated(test_data), ]
  test_data$weight_boot <- sapply(test_data$id, function(i) sum(boot_idx == i))
  test_data$weight <- test_data$weight * test_data$weight_boot
  test_data <- merge(
    x = trial_pp@outcome_data@data[, c("id", "trial_period", "followup_time")],
    y = test_data,
    all.x = TRUE
  )
  test_data$weight[is.na(test_data$weight)] <- 0
  test_data <- test_data[order(id, trial_period, followup_time)]

  expect_equal(result$data$weight, test_data$weight, tolerance = 1e-7)
})

test_that("predict works with bootstrap", {
  set.seed(194)
  trial_pp_dir <- withr::local_tempdir("trial_pp", tempdir(TRUE))

  trial_pp <- trial_sequence(estimand = "PP") |>
    set_data(
      data = data_censored,
      id = "id",
      period = "period",
      treatment = "treatment",
      outcome = "outcome",
      eligible = "eligible"
    ) |>
    set_switch_weight_model(
      numerator = ~age,
      denominator = ~ age + x1 + x3,
      model_fitter = stats_glm_logit(save_path = file.path(trial_pp_dir, "switch_models"))
    ) |>
    set_censor_weight_model(
      censor_event = "censored",
      numerator = ~x2,
      denominator = ~ x2 + x1,
      pool_models = "none",
      model_fitter = stats_glm_logit(save_path = file.path(trial_pp_dir, "censor_models"))
    ) |>
    calculate_weights() |>
    set_outcome_model(model_fitter = stats_glm_logit(save_path = file.path(trial_pp_dir, "outcome_model"))) |>
    set_expansion_options(
      output = save_to_datatable(),
      chunk_size = 500
    ) |>
    expand_trials() |>
    load_expanded_data() |>
    fit_msm(
      weight_cols = c("weight")
    ) |>
    suppressMatchingWarnings("fitted probabilities")

  suppressWarnings(ci_np_bs <- predict(trial_pp, predict_times = 1:20, ci_type = "Nonpara. bootstrap"))
  expect_snapshot(ci_np_bs)
  suppressWarnings(ci_sandwich <- predict(trial_pp, predict_times = 1:20, ci_type = "sandwich")[[3]])
  expect_snapshot(ci_sandwich)
  suppressWarnings(ci_lef_outcome <- predict(trial_pp, predict_times = 1:20, ci_type = "LEF outcome"))
  expect_snapshot(ci_lef_outcome)
  suppressWarnings(ci_lef_both <- predict(trial_pp, predict_times = 1:20, ci_type = "LEF both"))
  expect_snapshot(ci_lef_both)

  # TODO check if there is there is some overwriting going on due to data.table, so the results would not be
  # reproducible if we calculate different methods.
})
