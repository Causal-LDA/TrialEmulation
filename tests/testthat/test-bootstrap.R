test_that("Same weights recalculated if we use the same weight model coefficients and original dataset", {
  trial_pp_dir <- file.path(tempdir(), "trial_pp")
  dir.create(trial_pp_dir)

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
      chunk_size = 500) |>
    expand_trials() |>
    load_expanded_data() |>
    fit_msm(
      weight_cols = c("weight")
    )

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
  result <- weight_func_bootstrap(object= trial_pp, remodel = F, quiet = T, boot_idx = unique(trial_pp@data@data$id),
                                        new_coef_sw_d0 = trial_pp@switch_weights@fitted$d0@summary$tidy$estimate,
                                        new_coef_sw_d1 = trial_pp@switch_weights@fitted$d1@summary$tidy$estimate,
                                        new_coef_sw_n0 = trial_pp@switch_weights@fitted$n0@summary$tidy$estimate,
                                        new_coef_sw_n1 = trial_pp@switch_weights@fitted$n1@summary$tidy$estimate,
                                        new_coef_c_d0 = trial_pp@censor_weights@fitted$d0@summary$tidy$estimate,
                                        new_coef_c_d1 = trial_pp@censor_weights@fitted$d1@summary$tidy$estimate,
                                        new_coef_c_n1 = trial_pp@censor_weights@fitted$n1@summary$tidy$estimate,
                                        new_coef_c_n0 = trial_pp@censor_weights@fitted$n0@summary$tidy$estimate)

  expect_equal(result$data$weight, trial_pp@outcome_data@data$weight)
})

test_that("Same weights refitted if we use original dataset", {
  trial_pp_dir <- file.path(tempdir(), "trial_pp")
  dir.create(trial_pp_dir)

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
      chunk_size = 500) |>
    expand_trials() |>
    load_expanded_data() |>
    fit_msm(
      weight_cols = c("weight")
    )
  result <- weight_func_bootstrap(object= trial_pp, remodel = T, quiet = T, boot_idx = unique(trial_pp@data@data$id))

  expect_equal(result$data$weight, trial_pp@outcome_data@data$weight)
})

test_that("Same weights refitted if we use example bootstrap sample", {
  trial_pp_dir <- file.path(tempdir(), "trial_pp")
  dir.create(trial_pp_dir)

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
      chunk_size = 500) |>
    expand_trials() |>
    load_expanded_data() |>
    fit_msm(
      weight_cols = c("weight")
    )

  boot_idx <- sort(sample(unique(trial_pp@data@data$id), replace = TRUE))

  bootstrap_sample <- data_censored[data_censored$id == boot_idx[1],] %>%
    dplyr::mutate(id = 1)
  for (i in 2:length(boot_idx)){
    bootstrap_sample <- rbind(bootstrap_sample,
                              data_censored[data_censored$id == boot_idx[i],] %>%
                                dplyr::mutate(id = i))
  }

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
      chunk_size = 500) |>
    expand_trials() |>
    load_expanded_data() |>
    fit_msm(
      weight_cols = c("weight")
    )

  test_data <- trial_pp_boot@outcome_data@data %>%
    rowwise() %>%
    dplyr::mutate(id = boot_idx[[id]]) %>%
    dplyr::distinct() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(weight_boot = length(boot_idx[boot_idx == id])) %>%
    dplyr::mutate(weight = ifelse(weight_boot !=0,weight*weight_boot, 0)) %>%
    dplyr::right_join(trial_pp@outcome_data@data %>%  dplyr::select(id, trial_period, followup_time),
                      by = c("id", "trial_period", "followup_time")) %>%
    dplyr::mutate(weight = ifelse(is.na(weight), 0, weight)) %>%
    dplyr::arrange(id, trial_period, followup_time)

  result <- weight_func_bootstrap(object= trial_pp, remodel = T, quiet = T, boot_idx = boot_idx)
  result$data <- result$data %>%
    dplyr::arrange(id, trial_period, followup_time)


  expect_equal(trial_pp_boot@switch_weights@fitted$d0@summary$tidy$estimate, result$switch_models$switch_d0$summary$estimate, tolerance = 1e-7)
  expect_equal(trial_pp_boot@switch_weights@fitted$d1@summary$tidy$estimate, result$switch_models$switch_d1$summary$estimate, tolerance = 1e-7)
  expect_equal(trial_pp_boot@switch_weights@fitted$n0@summary$tidy$estimate, result$switch_models$switch_n0$summary$estimate, tolerance = 1e-7)
  expect_equal(trial_pp_boot@switch_weights@fitted$n1@summary$tidy$estimate, result$switch_models$switch_n1$summary$estimate, tolerance = 1e-7)

  expect_equal(trial_pp_boot@censor_weights@fitted$d0@summary$tidy$estimate, result$censor_models$cens_d0$summary$estimate, tolerance = 1e-7)
  expect_equal(trial_pp_boot@censor_weights@fitted$d1@summary$tidy$estimate, result$censor_models$cens_d1$summary$estimate, tolerance = 1e-7)
  expect_equal(trial_pp_boot@censor_weights@fitted$n0@summary$tidy$estimate, result$censor_models$cens_n0$summary$estimate, tolerance = 1e-7)
  expect_equal(trial_pp_boot@censor_weights@fitted$n1@summary$tidy$estimate, result$censor_models$cens_n1$summary$estimate, tolerance = 1e-7)



  expect_equal(result$data$weight, test_data$weight, tolerance = 1e-8)
})

test_that("Correct weights recalculated if we use new weight model coefficients and bootstrap sample", {
  trial_pp_dir <- file.path(tempdir(), "trial_pp")
  dir.create(trial_pp_dir)

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
      chunk_size = 500) |>
    expand_trials() |>
    load_expanded_data() |>
    fit_msm(
      weight_cols = c("weight")
    )
  boot_idx <- sort(sample(unique(trial_pp@data@data$id), replace = TRUE))

  bootstrap_sample <- data_censored[data_censored$id == boot_idx[1],] %>%
    dplyr::mutate(id = 1)
  for (i in 2:length(boot_idx)){
    bootstrap_sample <- rbind(bootstrap_sample,
                              data_censored[data_censored$id == boot_idx[i],] %>%
                                dplyr::mutate(id = i))
  }

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
      chunk_size = 500) |>
    expand_trials() |>
    load_expanded_data() |>
    fit_msm(
      weight_cols = c("weight")
    )

  test_data <- trial_pp_boot@outcome_data@data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(id = boot_idx[[id]]) %>%
    dplyr::distinct() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(weight_boot = length(boot_idx[boot_idx == id])) %>%
    dplyr::mutate(weight = ifelse(weight_boot !=0,weight*weight_boot, 0)) %>%
    dplyr::right_join(trial_pp@outcome_data@data %>%  dplyr::select(id, trial_period, followup_time),
                      by = c("id", "trial_period", "followup_time")) %>%
    dplyr::mutate(weight = ifelse(is.na(weight), 0, weight)) %>%
    dplyr::arrange(id, trial_period, followup_time)

  result <- weight_func_bootstrap(object= trial_pp, remodel = F, quiet = T, boot_idx = boot_idx,
                                  new_coef_sw_d0 = trial_pp_boot@switch_weights@fitted$d0@summary$tidy$estimate,
                                  new_coef_sw_d1 = trial_pp_boot@switch_weights@fitted$d1@summary$tidy$estimate,
                                  new_coef_sw_n0 = trial_pp_boot@switch_weights@fitted$n0@summary$tidy$estimate,
                                  new_coef_sw_n1 = trial_pp_boot@switch_weights@fitted$n1@summary$tidy$estimate,
                                  new_coef_c_d0 = trial_pp_boot@censor_weights@fitted$d0@summary$tidy$estimate,
                                  new_coef_c_d1 = trial_pp_boot@censor_weights@fitted$d1@summary$tidy$estimate,
                                  new_coef_c_n1 = trial_pp_boot@censor_weights@fitted$n1@summary$tidy$estimate,
                                  new_coef_c_n0 = trial_pp_boot@censor_weights@fitted$n0@summary$tidy$estimate)

  result$data <- result$data %>%
    dplyr::arrange(id, trial_period, followup_time)

  expect_equal(result$data$weight, test_data$weight, tolerance = 1e-7)
})
