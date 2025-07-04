#' Weighting for bootstrap samples
#'
#' @param object trial_sequence class object with weight and outcome models fitted
#' @param remodel indicates whether user requires refitting the weight models, default = TRUE
#' @param new_coef_sw_d0 new model coefficients for switching weight denominator model in previous treatment = 0
#' @param new_coef_sw_n0 new model coefficients for switching weight denominator model in previous treatment = 0
#' @param new_coef_sw_d1 new model coefficients for switching weight denominator model in previous treatment = 0
#' @param new_coef_sw_n1 new model coefficients for switching weight denominator model in previous treatment = 0
#' @param new_coef_c_d0 new model coefficients for switching weight denominator model in previous treatment = 0
#' @param new_coef_c_n0 new model coefficients for switching weight denominator model in previous treatment = 0
#' @param new_coef_c_d1 new model coefficients for switching weight denominator model in previous treatment = 0
#' @param new_coef_c_n1 new model coefficients for switching weight denominator model in previous treatment = 0
#' @param new_coef_c_d new model coefficients for switching weight denominator model in previous treatment = 0
#' @param new_coef_c_n new model coefficients for switching weight denominator model in previous treatment = 0
#' @param boot_idx vector containing bootstrap sample of patient ids
#' @param quiet indicates whether function messages are printed or not, default = TRUE
#' @param glm_function glm function to be used for weight model fitting, default is "glm"
#' @param ... Passed to `fit_switch_weights_bootstrap()`
#'
#' @returns List containing
#' \describe{
#'  \item{model}{Outcome data with new refitted/recalculated weights from given bootstrap sample}
#'  \item{switch_models}{a list containing a summary table of regression models for switching weights}
#'  \item{cense_models}{a list containing a summary table of regression models for censoring weights}
#' }
#'
#' @importFrom stats predict.glm
#' @noRd
weight_func_bootstrap <- function(object,
                                  remodel = TRUE,
                                  new_coef_sw_d0 = NA,
                                  new_coef_sw_n0 = NA,
                                  new_coef_sw_d1 = NA,
                                  new_coef_sw_n1 = NA,
                                  new_coef_c_d0 = NA,
                                  new_coef_c_n0 = NA,
                                  new_coef_c_d1 = NA,
                                  new_coef_c_n1 = NA,
                                  new_coef_c_d = NA,
                                  new_coef_c_n = NA,
                                  boot_idx,
                                  quiet = TRUE,
                                  glm_function = "glm",
                                  ...) {
  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  eligible0 <- eligible1 <- id <- period <- eligible0.y <- eligible1.y <- am_1 <- eligible_wts_0 <-
    eligible_wts_1 <- treatment <- wt <- wtC <- p0_n <- p0_d <- p1_n <- p1_d <- pC_n0 <- pC_d0 <-
    pC_n1 <- pC_d1 <- pC_n <- pC_d <- wtprod <- weight0 <- period_new <- trial_period <-
    index <- followup_time <- weight <- weight_boot <- NULL


  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Switching weights --------------------
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  quiet_msg(quiet, "Starting switching weights")
  switch_models <- list()
  if (remodel == TRUE) {
    switch_d_cov <- object@switch_weights@denominator
    switch_n_cov <- object@switch_weights@numerator
    sw_data <- object@data@data

    switch_results <- fit_switch_weights_bootstrap(
      switch_d_cov = switch_d_cov,
      switch_n_cov = switch_n_cov,
      sw_data = sw_data,
      boot_idx = boot_idx,
      quiet = quiet,
      save_dir = NA,
      save_weight_models = FALSE,
      glm_function = glm_function,
      ...
    )
    sw_data <- switch_results$sw_data
    switch_models <- switch_results$switch_models
    rm(switch_results)
  } else { # only need to fetch the glm objects if we are recalculating weights from new coefficients
    weight_model_d0 <- readRDS(object@switch_weights@fitted$d0@summary$save_path$path)
    weight_model_n0 <- readRDS(object@switch_weights@fitted$n0@summary$save_path$path)
    weight_model_d1 <- readRDS(object@switch_weights@fitted$d1@summary$save_path$path)
    weight_model_n1 <- readRDS(object@switch_weights@fitted$n1@summary$save_path$path)

    weight_model_d0$coefficients <- new_coef_sw_d0
    weight_model_n0$coefficients <- new_coef_sw_n0
    weight_model_d1$coefficients <- new_coef_sw_d1
    weight_model_n1$coefficients <- new_coef_sw_n1

    switch_d0 <- cbind(
      p0_d = predict.glm(weight_model_d0, weight_model_d0$data, type = "response"),
      weight_model_d0$data[, c("eligible0", "id", "period")]
    )

    switch_n0 <- cbind(
      p0_n = predict.glm(weight_model_n0, weight_model_n0$data, type = "response"),
      weight_model_n0$data[, c("eligible0", "id", "period")]
    )

    switch_d1 <- cbind(
      p1_d = predict.glm(weight_model_d1, weight_model_d1$data, type = "response"),
      weight_model_d1$data[, c("eligible1", "id", "period")]
    )

    switch_n1 <- cbind(
      p1_n = predict.glm(weight_model_n1, weight_model_n1$data, type = "response"),
      weight_model_n1$data[, c("eligible1", "id", "period")]
    )

    switch_0 <- merge.data.table(switch_d0, switch_n0,
      by = c("id", "period", "eligible0")
    )
    switch_1 <- merge.data.table(switch_d1, switch_n1,
      by = c("id", "period", "eligible1")
    )

    rm(switch_d0, switch_d1, switch_n0, switch_n1)

    sw_data <- merge.data.table(object@data@data, switch_0[, !"eligible0"], by = c("id", "period"), all = TRUE)
    sw_data <- merge.data.table(sw_data, switch_1[, !"eligible1"], by = c("id", "period"), all = TRUE)

    rm(switch_1, switch_0)
  }
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Censoring weights --------------------
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  quiet_msg(quiet, "Starting censor weights")
  censor_models <- list()

  if (remodel == TRUE) {
    cense_d_cov <- object@censor_weights@denominator
    cense_n_cov <- object@censor_weights@numerator
    pool_cense_d <- object@censor_weights@pool_denominator
    pool_cense_n <- object@censor_weights@pool_numerator

    censor_results <- fit_censor_weights_bootstrap(
      cense_d_cov = cense_d_cov,
      cense_n_cov = cense_n_cov,
      pool_cense_d = pool_cense_d,
      pool_cense_n = pool_cense_n,
      sw_data = sw_data,
      boot_idx = boot_idx,
      quiet = quiet,
      save_dir = NA,
      save_weight_models = FALSE,
      glm_function = glm_function,
      ...
    )
    sw_data <- censor_results$sw_data
    censor_models <- censor_results$censor_models
    rm(censor_results)
  } else {
    if (object@censor_weights@pool_denominator) { # Fit pooled denominator models
      cense_model_d <- readRDS(object@censor_weights@fitted$d@summary$save_path$path)
      cense_model_d$coeefficients <- new_coef_c_d

      cense_d <- cbind(
        pC_d = predict.glm(cense_model_d, cense_model_d$data, type = "response"),
        cense_model_d$data[, c("id", "period")]
      )

      rm(cense_model_d)
    } else {
      cense_model_d0 <- readRDS(object@censor_weights@fitted$d0@summary$save_path$path)
      cense_model_d1 <- readRDS(object@censor_weights@fitted$d1@summary$save_path$path)

      cense_model_d0$coefficients <- new_coef_c_d0
      cense_model_d1$coefficients <- new_coef_c_d1


      cense_d0 <- cbind(
        pC_d0 = predict.glm(cense_model_d0, cense_model_d0$data, type = "response"),
        cense_model_d0$data[, c("id", "period")]
      )
      rm(cense_model_d0)

      cense_d1 <- cbind(
        pC_d1 = predict.glm(cense_model_d1, cense_model_d1$data, type = "response"),
        cense_model_d1$data[, c("id", "period")]
      )

      rm(cense_model_d1)
    }

    if (object@censor_weights@pool_numerator) {
      cense_n <- readRDS(object@censor_weights@fitted$n@summary$save_path$path)

      cense_model_n$coefficients <- new_coef_c_n

      cense_n <- cbind(
        pC_n = predict.glm(cense_model_n, cense_model_n$data, type = "response"),
        cense_model_n$data[, c("id", "period")]
      )

      rm(cense_model_n)
    } else {
      cense_model_n0 <- readRDS(object@censor_weights@fitted$n0@summary$save_path$path)
      cense_model_n1 <- readRDS(object@censor_weights@fitted$n1@summary$save_path$path)

      cense_model_n0$coefficients <- new_coef_c_n0
      cense_model_n1$coefficients <- new_coef_c_n1

      cense_n0 <- cbind(
        pC_n0 = predict.glm(cense_model_n0, cense_model_n0$data, type = "response"),
        cense_model_n0$data[, c("id", "period")]
      )

      rm(cense_model_n0)

      cense_n1 <- cbind(
        pC_n1 = predict.glm(cense_model_n1, cense_model_n1$data, type = "response"),
        cense_model_n1$data[, c("id", "period")]
      )

      rm(cense_model_n1)
    }

    # combine ------------------------------
    if (object@censor_weights@pool_denominator && object@censor_weights@pool_numerator) {
      # all pooled
      sw_data <- merge.data.table(sw_data, cense_d, by = c("id", "period"), all = TRUE)
      sw_data <- merge.data.table(sw_data, cense_n, by = c("id", "period"), all = TRUE)
      rm(cense_d, cense_n)
    } else if (!object@censor_weights@pool_denominator && !object@censor_weights@pool_numerator) {
      # no pooled
      cense_0 <- cense_d0[cense_n0, on = list(id = id, period = period)]
      cense_1 <- cense_d1[cense_n1, on = list(id = id, period = period)]
      rm(cense_n1, cense_d1, cense_n0, cense_d0)

      sw_data <- merge.data.table(sw_data, cense_0, by = c("id", "period"), all = TRUE)
      sw_data <- merge.data.table(sw_data, cense_1, by = c("id", "period"), all = TRUE)

      rm(cense_0, cense_1)
      sw_data[am_1 == 0, `:=`(pC_n = pC_n0, pC_d = pC_d0)]
      sw_data[am_1 == 1, `:=`(pC_n = pC_n1, pC_d = pC_d1)]
    } else if (!object@censor_weights@pool_denominator && object@censor_weights@pool_numerator) {
      # only numerator pooled
      sw_data <- sw_data[cense_n, on = list(id = id, period = period)]
      sw_data <- merge.data.table(sw_data, cense_d0, by = c("id", "period"), all = TRUE)
      sw_data <- merge.data.table(sw_data, cense_d1, by = c("id", "period"), all = TRUE)
      rm(cense_d1, cense_n, cense_d0)

      sw_data[am_1 == 0, `:=`(pC_d = pC_d0)]
      sw_data[am_1 == 1, `:=`(pC_d = pC_d1)]
    } else if (object@censor_weights@pool_denominator && !object@censor_weights@pool_numerator) {
      # only denominator pooled
      stop("Check the arguments for pooling censoring models!")
    }
  }




  # Calculate switching weights
  # use calculated weights
  if (any(!is.na(eligible_wts_0))) {
    sw_data[
      (am_1 == 0 & eligible_wts_0 == 1 & treatment == 0 & !is.na(p0_n) & !is.na(p0_d)),
      wt := (1.0 - p0_n) / (1.0 - p0_d)
    ]
    sw_data[
      (am_1 == 0 & eligible_wts_0 == 1 & treatment == 1 & !is.na(p0_n) & !is.na(p0_d)),
      wt := p0_n / p0_d
    ]
    sw_data[(am_1 == 0 & eligible_wts_0 == 0), wt := 1.0]
  } else {
    sw_data[
      (am_1 == 0 & treatment == 0 & !is.na(p0_n) & !is.na(p0_d)),
      wt := (1.0 - p0_n) / (1.0 - p0_d)
    ]
    sw_data[
      (am_1 == 0 & treatment == 1 & !is.na(p0_n) & !is.na(p0_d)),
      wt := p0_n / p0_d
    ]
  }
  if (any(!is.na(eligible_wts_1))) {
    sw_data[
      (am_1 == 1 & eligible_wts_1 == 1 & treatment == 0 & !is.na(p1_n) & !is.na(p1_d)),
      wt := (1.0 - p1_n) / (1.0 - p1_d)
    ]
    sw_data[
      (am_1 == 1 & eligible_wts_1 == 1 & treatment == 1 & !is.na(p1_n) & !is.na(p1_d)),
      wt := p1_n / p1_d
    ]
    sw_data[(am_1 == 1 & eligible_wts_1 == 0), wt := 1.0]
  } else {
    sw_data[
      (am_1 == 1 & treatment == 0 & !is.na(p1_n) & !is.na(p1_d)),
      wt := (1.0 - p1_n) / (1.0 - p1_d)
    ]
    sw_data[
      (am_1 == 1 & treatment == 1 & !is.na(p1_n) & !is.na(p1_d)),
      wt := p1_n / p1_d
    ]
  }


  sw_data[is.na(pC_d), pC_d := 1]
  sw_data[is.na(pC_n), pC_n := 1]
  sw_data[, wtC := pC_n / pC_d]

  sw_data[, wt := wt * wtC]

  censor_models <- censor_models[intersect(
    c("cens_pool_d", "cens_d0", "cens_n0", "cens_d1", "cens_n1", "cens_pool_n"),
    names(censor_models)
  )]

  sw_data[, first := !duplicated(sw_data[, id])]
  sw_data <- sw_data[!is.na(wt)]
  temp_data <- data.table(
    id = sw_data[, id],
    period = sw_data[, period]
  )
  temp_data[, wtprod := 1.0, by = id]

  sw_data[first == TRUE, weight0 := 1.0]
  sw_data[, weight0 := cumprod(wt), by = id]
  temp_data[, wtprod := sw_data[, weight0]]

  expand_index <- rep(seq_len(nrow(sw_data)), sw_data[, period] + 1)

  quiet_msg(quiet, "Adding new weights to expanded data")

  ### new_data only contains ID, trial_period, followup_time adn the new IP weights
  new_data <- data.table(id = sw_data[expand_index, id])
  new_data[, period_new := sw_data[expand_index, period]]

  new_data[, weight0 := sw_data[expand_index, weight0]]

  new_data[, trial_period := trial_period_func(sw_data)]

  new_data[, index := seq_len(.N)]

  new_data <- new_data[temp_data, on = list(id = id, trial_period = period)]
  setorder(new_data, index)
  new_data[, followup_time := period_new - trial_period]
  new_data[, weight := (weight0 / wtprod)]

  #### New data is merged with existing expanded data to add the new weights

  output_data <- new_data[object@outcome_data@data,
    on = list(id = id, trial_period = trial_period, followup_time = followup_time)
  ]
  output_data[, weight_boot := sapply(id, function(i) sum(i == boot_idx))]
  output_data[, weight := ifelse(weight_boot != 0, weight * weight_boot, 0)]

  list(
    data = output_data[, names(object@outcome_data@data), with = FALSE],
    switch_models = switch_models,
    censor_models = censor_models
  )
}


#' Calculate various types of confidence intervals based on bootstrapping methods
#'
#' @param object object of clas `trial_sequence`
#' @param ci_type which CI to generate, options: `'Nonpara. bootstrap', 'LEF outcome', 'LEF both'`
#' @param bootstrap_sample_size Sample size of bootstrap resampling, default = 200
#'
#' @returns 2 by x matrix of CI lower and upper bound, x is number of time points
#' @noRd
#' @importFrom stats predict.glm vcov
#' @importFrom future.apply future_replicate
calculate_bootstrap_CIs <- function(object,
                                    ci_type,
                                    bootstrap_sample_size = 200,
                                    predict_times,
                                    point_estimate,
                                    pred_fun) {
  weight_boot <- trial_period <- NULL
  ######################################################################################################################
  #  All code below needs to be changed to match variable names with function input, some of which are class type #
  ######################################################################################################################
  if (ci_type != "Nonpara. bootstrap") {
    X <- model.matrix(object@outcome_model@fitted@model$model)
    e <- object@outcome_model@fitted@model$model$model$outcome - object@outcome_model@fitted@model$model$fitted.values
    if (ci_type == "LEF both") {
      switch_d0 <- readRDS(object@switch_weights@fitted$d0@summary$save_path$path)

      X_sw_d0 <- model.matrix(switch_d0)
      e_sw_d0 <- switch_d0$y - switch_d0$fitted.values


      switch_n0 <- readRDS(object@switch_weights@fitted$n0@summary$save_path$path)
      X_sw_n0 <- model.matrix(switch_n0)
      e_sw_n0 <- switch_n0$y - switch_n0$fitted.values


      switch_d1 <- readRDS(object@switch_weights@fitted$d1@summary$save_path$path)
      X_sw_d1 <- model.matrix(switch_d1)
      e_sw_d1 <- switch_d1$y - switch_d1$fitted.values


      switch_n1 <- readRDS(object@switch_weights@fitted$n1@summary$save_path$path)
      X_sw_n1 <- model.matrix(switch_n1)
      e_sw_n1 <- switch_n1$y - switch_n1$fitted.values


      if (object@censor_weights@pool_denominator) {
        cense_d <- readRDS(object@censor_weights@fitted$d@summary$save_path$path)

        X_c_d <- model.matrix(cense_d)
        e_c_d <- cense_d$y - cense_d$fitted.values
      } else {
        cense_d0 <- readRDS(object@censor_weights@fitted$d0@summary$save_path$path)

        X_c_d0 <- model.matrix(cense_d0)
        e_c_d0 <- cense_d0$y - cense_d0$fitted.values


        cense_d1 <- readRDS(object@censor_weights@fitted$d1@summary$save_path$path)

        X_c_d1 <- model.matrix(cense_d1)
        e_c_d1 <- cense_d1$y - cense_d1$fitted.values
      }

      if (object@censor_weights@pool_numerator) {
        cense_n <- readRDS(object@censor_weights@fitted$n@summary$save_path$path)

        X_c_n <- model.matrix(cense_n)
        e_c_n <- cense_n$y - cense_n$fitted.values
      } else {
        cense_n0 <- readRDS(object@censor_weights@fitted$n0@summary$save_path$path)

        X_c_n0 <- model.matrix(cense_n0)
        e_c_n0 <- cense_n0$y - cense_n0$fitted.values


        cense_n1 <- readRDS(object@censor_weights@fitted$n1@summary$save_path$path)

        X_c_n1 <- model.matrix(cense_n1)
        e_c_n1 <- cense_n1$y - cense_n1$fitted.values
      }
    }
  }

  # Step 1: for each bootstrap sample:
  bootstrapped_MRDs <- future.apply::future_replicate(
    n = bootstrap_sample_size,
    expr = {
      # Bootstrap sample with patient id as sampling unit
      boot_idx <- sort(sample(unique(object@data@data$id), replace = TRUE))

      weights_table_boot <- data.table(id = unique(object@data@data$id))
      weights_table_boot[, weight_boot := sapply(weights_table_boot$id, function(i) sum(i == boot_idx))]


      # Step 2: refit/recalculate weights
      if (ci_type == "LEF both") {
        # Calculate the weight models' coefficient LEF approximates
        data_0 <- merge(weights_table_boot, switch_d0$data, by = "id", all.y = TRUE)
        data_1 <- merge(weights_table_boot, switch_d1$data, by = "id", all.y = TRUE)

        LEF_sw_d0_boot <- t(X_sw_d0) %*% (data_0$weight_boot * e_sw_d0)
        LEF_sw_n0_boot <- t(X_sw_n0) %*% (data_0$weight_boot * e_sw_n0)
        LEF_sw_d1_boot <- t(X_sw_d1) %*% (data_1$weight_boot * e_sw_d1)
        LEF_sw_n1_boot <- t(X_sw_n1) %*% (data_1$weight_boot * e_sw_n1)

        # Calculate \hat \beta(b)
        beta_sw_d0 <- switch_d0$coefficients + vcov(switch_d0) %*% LEF_sw_d0_boot
        beta_sw_n0 <- switch_n0$coefficients + vcov(switch_n0) %*% LEF_sw_n0_boot
        beta_sw_d1 <- switch_d1$coefficients + vcov(switch_d1) %*% LEF_sw_d1_boot
        beta_sw_n1 <- switch_n1$coefficients + vcov(switch_n1) %*% LEF_sw_n1_boot

        if (object@censor_weights@pool_denominator) {
          # Calculate the weight models' coefficient LEF approximates
          data <- merge(weights_table_boot, cense_d$data, by = "id", all.y = TRUE)
          LEF_c_d_boot <- t(X_c_d) %*% (data$weight_boot * e_c_d)

          # Calculate \hat \beta(b)
          beta_c_d <- cense_d$coefficients + vcov(cense_d) %*% LEF_c_d_boot
        } else {
          data_0 <- merge(weights_table_boot, cense_d0$data, by = "id", all.y = TRUE)
          data_1 <- merge(weights_table_boot, cense_d1$data, by = "id", all.y = TRUE)

          LEF_c_d0_boot <- t(X_c_d0) %*% (data_0$weight_boot * e_c_d0)
          LEF_c_d1_boot <- t(X_c_d1) %*% (data_1$weight_boot * e_c_d1)


          # Calculate \hat \beta(b)
          beta_c_d0 <- cense_d0$coefficients + vcov(cense_d0) %*% LEF_c_d0_boot
          beta_c_d1 <- cense_d1$coefficients + vcov(cense_d1) %*% LEF_c_d1_boot
        }

        if (object@censor_weights@pool_numerator) {
          # Calculate the weight models' coefficient LEF approximates
          data <- merge(weights_table_boot, cense_n$data, by = "id", all.y = TRUE)
          LEF_c_n_boot <- t(X_c_n) %*% (data$weight_boot * e_c_n)

          # Calculate \hat \beta(b)
          beta_c_n <- cense_n$coefficients + vcov(cense_n) %*% LEF_c_n_boot
        } else {
          data_0 <- merge(weights_table_boot, cense_n0$data, by = "id", all.y = TRUE)
          data_1 <- merge(weights_table_boot, cense_n1$data, by = "id", all.y = TRUE)

          LEF_c_n0_boot <- t(X_c_n0) %*% (data_0$weight_boot * e_c_n0)
          LEF_c_n1_boot <- t(X_c_n1) %*% (data_1$weight_boot * e_c_n1)


          # Calculate \hat \beta(b)
          beta_c_n0 <- cense_n0$coefficients + vcov(cense_n0) %*% LEF_c_n0_boot
          beta_c_n1 <- cense_n1$coefficients + vcov(cense_n1) %*% LEF_c_n1_boot
        }

        boot_design_data <- weight_func_bootstrap(
          object = object,
          new_coef_sw_d0 = beta_sw_d0,
          new_coef_sw_n0 = beta_sw_n0,
          new_coef_sw_d1 = beta_sw_d1,
          new_coef_sw_n1 = beta_sw_n1,
          new_coef_c_d0 = beta_c_d0,
          new_coef_c_n0 = beta_c_n0,
          new_coef_c_d1 = beta_c_d1,
          new_coef_c_n1 = beta_c_n1,
          new_coef_c_d = beta_c_d,
          new_coef_c_n = beta_c_n,
          boot_idx = boot_idx,
          remodel = FALSE,
          quiet = TRUE
        )
      } else {
        boot_design_data <- weight_func_bootstrap(
          object = object,
          boot_idx = boot_idx,
          remodel = TRUE,
          quiet = TRUE
        )
      }
      # Step 3. A) : refit MSM (only for nonparametric bootstrap)
      PP_boot <- object

      if (ci_type == "Nonpara. bootstrap") {
        PP_boot@outcome_data@data <- boot_design_data$data
        PP_boot <- fit_msm(PP_boot, weight_cols = c("weight"))
      } else { # ci_type %in% c("LEF outcome", "LEF both")
        # Step 3. B): recalculate MSM coefficient (for LEF)
        LEFs <- t(X) %*% (boot_design_data$data$weight * e)
        LEFs[is.na(LEFs)] <- 0
        variance_mat <- vcov(PP_boot@outcome_model@fitted@model$model)
        variance_mat[is.na(variance_mat)] <- 0
        # Calculate \hat \beta(b)
        PP_boot@outcome_model@fitted@model$model$coefficients <-
          PP_boot@outcome_model@fitted@model$model$coefficients + variance_mat %*% LEFs
      }

      # Step 4: get prediction in bootstrap sample

      bootstrap_sample <- object@outcome_data@data[
        unlist(lapply(boot_idx, function(i) which(object@outcome_data@data$id == i))),
      ]

      newdata <- check_newdata(
        bootstrap_sample[trial_period == 0, ],
        model = PP_boot@outcome_model@fitted@model$model,
        predict_times
      )

      pred_list_boot <- calculate_predictions(
        newdata = newdata,
        model = PP_boot@outcome_model@fitted@model$model,
        treatment_values = c(assigned_treatment_0 = 0, assigned_treatment_1 = 1),
        pred_fun = pred_fun,
        coefs_mat = matrix(coef(PP_boot@outcome_model@fitted@model$model), nrow = 1),
        matrix_n_col = length(predict_times)
      )

      # return difference estimate:
      pred_list_boot$assigned_treatment_1[, 1] - pred_list_boot$assigned_treatment_0[, 1]
      ## end replicate
    }
  )

  # Step 5: generate pivot CIs and return
  (2 * point_estimate) - t(apply(bootstrapped_MRDs, 1, quantile, probs = c(0.025, 0.975)))[, 2:1]
}

fit_switch_weights_bootstrap <- function(switch_d_cov,
                                         switch_n_cov,
                                         eligible_wts_0 = NA,
                                         eligible_wts_1 = NA,
                                         sw_data,
                                         boot_idx,
                                         quiet,
                                         save_dir,
                                         save_weight_models,
                                         glm_function,
                                         ...) {
  eligible0 <- eligible1 <- id <- period <- weight_boot <- NULL
  # Fit the models for the weights in the four scenarios
  switch_models <- list()
  # ------------------- eligible0 == 1 --------------------

  data_0_expr <- if ("eligible_wts_1" %in% colnames(sw_data)) {
    expression(am_1 == 0 & eligible_wts_1 == 0)
  } else {
    expression(am_1 == 0)
  }
  model_0_index <- sw_data[eval(data_0_expr), which = TRUE]

  # --------------- denominator ------------------
  model1 <- fit_glm(
    data = sw_data[model_0_index, ][, weight_boot := sapply(id, function(i) sum(i == boot_idx))],
    formula = switch_d_cov,
    weights = weight_boot,
    ...,
    glm_function = glm_function
  )

  switch_d0 <- cbind(p0_d = model1$fitted.values, model1$data[, c("eligible0", "id", "period")])

  switch_models$switch_d0 <- process_weight_model(
    model1,
    save_weight_models,
    save_dir,
    "weight_model_switch_d0.rds",
    "P(treatment = 1 | previous treatment = 0) for denominator",
    quiet
  )
  rm(model1)

  # -------------- numerator --------------------
  model2 <- fit_glm(
    data = sw_data[model_0_index, ][, weight_boot := sapply(id, function(i) sum(i == boot_idx))],
    formula = switch_n_cov,
    weights = weight_boot,
    ...,
    glm_function = glm_function
  )

  switch_n0 <- cbind(p0_n = model2$fitted.values, model2$data[, c("eligible0", "id", "period")])

  switch_models$switch_n0 <- process_weight_model(
    model2,
    save_weight_models,
    save_dir,
    "weight_model_switch_n0.rds",
    "P(treatment = 1 | previous treatment = 0) for numerator",
    quiet
  )
  rm(model2)

  # ------------------- eligible1 == 1 --------------------
  data_1_expr <- if ("eligible_wts_1" %in% colnames(sw_data)) {
    expression(am_1 == 1 & eligible_wts_1 == 1)
  } else {
    expression(am_1 == 1)
  }
  model_1_index <- sw_data[eval(data_1_expr), which = TRUE]
  # --------------- denominator ------------------
  model3 <- fit_glm(
    data = sw_data[model_1_index, ][, weight_boot := sapply(id, function(i) sum(i == boot_idx))],
    formula = switch_d_cov,
    weights = weight_boot,
    ...,
    glm_function = glm_function
  )

  switch_d1 <- cbind(p1_d = model3$fitted.values, model3$data[, c("eligible1", "id", "period")])

  switch_models$switch_d1 <- process_weight_model(
    model3,
    save_weight_models,
    save_dir,
    "weight_model_switch_d1.rds",
    "P(treatment = 1 | previous treatment = 1) for denominator",
    quiet
  )

  rm(model3)

  # -------------------- numerator ---------------------------
  model4 <- fit_glm(
    data = sw_data[model_1_index, ][, weight_boot := sapply(id, function(i) sum(i == boot_idx))],
    formula = switch_n_cov,
    weights = weight_boot,
    ...,
    glm_function = glm_function
  )

  switch_n1 <- cbind(p1_n = model4$fitted.values, model4$data[, c("eligible1", "id", "period")])

  switch_models$switch_n1 <- process_weight_model(
    model4,
    save_weight_models,
    save_dir,
    "weight_model_switch_n1.rds",
    "P(treatment = 1 | previous treatment = 1) for numerator",
    quiet
  )
  rm(model4)

  # -------------- Combine results --------------------

  switch_0 <- merge.data.table(switch_d0, switch_n0,
    by = c("id", "period", "eligible0")
  )
  switch_1 <- merge.data.table(switch_d1, switch_n1,
    by = c("id", "period", "eligible1")
  )

  rm(switch_d0, switch_d1, switch_n0, switch_n1)

  sw_data <- merge.data.table(sw_data, switch_0[, !"eligible0"], by = c("id", "period"), all = TRUE)
  sw_data <- merge.data.table(sw_data, switch_1[, !"eligible1"], by = c("id", "period"), all = TRUE)

  rm(switch_1, switch_0)
  list(sw_data = sw_data, switch_models = switch_models)
}



#' Fit Informative Censoring Models
#'
#' @param cense Censoring variable
#' @param cense_d_cov Formula for denominator model
#' @param cense_n_cov Formula for numerator model
#' @param pool_cense_d Pool denominator models for treatment arms?
#' @param pool_cense_n Pool numerator models for treatment arms?
#' @param sw_data Data.table object which is modified in place
#' @param quiet Print messages or be quiet?
#' @param save_weight_models Save weight models?
#' @param save_dir Directory to save `glm` model objects
#' @param glm_function `glm` function for model fitting? eg `stats::glm` or `bigglm`
#' @param ... Other parameters passed to `glm`.
#'
#' @return List of model summaries and modified `sw_data` including informative censoring weights
#' @noRd
fit_censor_weights_bootstrap <- function(cense_d_cov,
                                         cense_n_cov,
                                         pool_cense_d,
                                         pool_cense_n,
                                         sw_data,
                                         boot_idx,
                                         quiet,
                                         save_dir,
                                         save_weight_models,
                                         glm_function,
                                         ...) {
  am_1 <- eligible0 <- eligible1 <- id <- pC_d0 <- pC_d1 <- pC_n0 <- pC_n1 <- period <- weight_boot <- NULL

  censor_models <- list()

  if (pool_cense_d) { # Fit pooled denominator models
    model1.cense <- fit_glm(
      data = sw_data[, weight_boot := sapply(id, function(i) sum(i == boot_idx))],
      formula = cense_d_cov,
      weights = weight_boot,
      ...,
      glm_function = glm_function
    )

    cense_d <- cbind(pC_d = model1.cense$fitted.values, model1.cense$data[, c("id", "period")])

    censor_models$cens_pool_d <- process_weight_model(
      model1.cense,
      save_weight_models,
      save_dir,
      "cense_model_pool_d.rds",
      "Model for P(cense = 0 | X) for denominator",
      quiet
    )
    rm(model1.cense)
  } else { # Fit separate denominator models for each arm
    # ---------------------- denominator -----------------------
    # ---------------------- eligible0 ---------------------------
    model1.cense <- fit_glm(
      data = sw_data[eligible0 == 1][, weight_boot := sapply(id, function(i) sum(i == boot_idx))],
      formula = cense_d_cov,
      weights = weight_boot,
      ...,
      glm_function = glm_function
    )

    cense_d0 <- cbind(pC_d0 = model1.cense$fitted.values, model1.cense$data[, c("id", "period")])

    censor_models$cens_d0 <- process_weight_model(
      model1.cense,
      save_weight_models,
      save_dir,
      "cense_model_d0.rds",
      "Model for P(cense = 0 | X, previous treatment = 0) for denominator",
      quiet
    )
    rm(model1.cense)

    # ------------------------- denominator ---------------------
    # ------------------------ eligible1 -------------------------
    model3.cense <- fit_glm(
      data = sw_data[eligible1 == 1][, weight_boot := sapply(id, function(i) sum(i == boot_idx))],
      formula = cense_d_cov,
      weights = weight_boot,
      ...,
      glm_function = glm_function
    )

    cense_d1 <- cbind(pC_d1 = model3.cense$fitted.values, model3.cense$data[, c("id", "period")])

    censor_models$cens_d1 <- process_weight_model(
      model3.cense,
      save_weight_models,
      save_dir,
      "cense_model_d1.rds",
      "Model for P(cense = 0 | X, previous treatment = 1) for denominator",
      quiet
    )
    rm(model3.cense)
  }

  if (pool_cense_n) { # Fit pooled numerator models
    model2.cense <- fit_glm(
      data = sw_data[, weight_boot := sapply(id, function(i) sum(i == boot_idx))],
      formula = cense_n_cov,
      weights = weight_boot,
      ...,
      glm_function = glm_function
    )

    cense_n <- cbind(pC_n = model2.cense$fitted.values, model2.cense$data[, c("id", "period")])

    censor_models$cens_pool_n <- process_weight_model(
      model2.cense,
      save_weight_models,
      save_dir,
      "cense_model_pool_n.rds",
      "Model for P(cense = 0 | X) for numerator",
      quiet
    )
    rm(model2.cense)
  } else { # Fit separate numerator models for each arm
    # -------------------------- numerator ----------------------
    #--------------------------- eligible0 -----------------------
    model2.cense <- fit_glm(
      data = sw_data[eligible0 == 1][, weight_boot := sapply(id, function(i) sum(i == boot_idx))],
      formula = cense_n_cov,
      weights = weight_boot,
      ...,
      glm_function = glm_function
    )
    cense_n0 <- cbind(pC_n0 = model2.cense$fitted.values, model2.cense$data[, c("id", "period")])

    censor_models$cens_n0 <- process_weight_model(
      model2.cense,
      save_weight_models,
      save_dir,
      "cense_model_n0.rds",
      "Model for P(cense = 0 | X, previous treatment = 0) for numerator",
      quiet
    )

    rm(model2.cense)

    # ------------------------ numerator -------------------------
    # ------------------------- eligible1 -----------------------
    model4.cense <- fit_glm(
      data = sw_data[eligible1 == 1][, weight_boot := sapply(id, function(i) sum(i == boot_idx))],
      formula = cense_n_cov,
      weights = weight_boot,
      ...,
      glm_function = glm_function
    )
    cense_n1 <- cbind(pC_n1 = model4.cense$fitted.values, model4.cense$data[, c("id", "period")])

    censor_models$cens_n1 <- process_weight_model(
      model4.cense,
      save_weight_models,
      save_dir,
      "cense_model_n1.rds",
      "Model for P(cense = 0 | X, previous treatment = 1) for numerator",
      quiet
    )
    rm(model4.cense)
  }

  # combine ------------------------------
  if (pool_cense_d && pool_cense_n) {
    # all pooled
    sw_data <- merge.data.table(sw_data, cense_d, by = c("id", "period"), all = TRUE)
    sw_data <- merge.data.table(sw_data, cense_n, by = c("id", "period"), all = TRUE)
    rm(cense_d, cense_n)
  } else if (!pool_cense_d && !pool_cense_n) {
    # no pooled
    cense_0 <- merge.data.table(cense_d0, cense_n0,
      by = c("id", "period")
    )
    cense_1 <- merge.data.table(cense_d1, cense_n1,
      by = c("id", "period")
    )
    rm(cense_n1, cense_d1, cense_n0, cense_d0)

    sw_data <- merge.data.table(sw_data, cense_0, by = c("id", "period"), all = TRUE)
    sw_data <- merge.data.table(sw_data, cense_1, by = c("id", "period"), all = TRUE)

    rm(cense_0, cense_1)
    sw_data[am_1 == 0, `:=`(pC_n = pC_n0, pC_d = pC_d0)]
    sw_data[am_1 == 1, `:=`(pC_n = pC_n1, pC_d = pC_d1)]
  } else if (!pool_cense_d && pool_cense_n) {
    # only numerator pooled
    sw_data <- sw_data[cense_n, on = list(id = id, period = period)]
    sw_data <- merge.data.table(sw_data, cense_d0, by = c("id", "period"), all = TRUE)
    sw_data <- merge.data.table(sw_data, cense_d1, by = c("id", "period"), all = TRUE)
    rm(cense_d1, cense_n, cense_d0)

    sw_data[am_1 == 0, `:=`(pC_d = pC_d0)]
    sw_data[am_1 == 1, `:=`(pC_d = pC_d1)]
  } else if (pool_cense_d && !pool_cense_n) {
    # only denominator pooled
    stop("Check the arguments for pooling censoring models!")
  }

  list(sw_data = sw_data, censor_models = censor_models)
}

trial_period_func <- function(x) {
  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  period <- id <- trial_period <- NULL

  x_new <- x[rep(seq_len(.N), period + 1), list(id, period)]
  x_new[, trial_period := f(.BY), by = list(id, period)]
  x_new[, trial_period]
}

f <- function(y) {
  last <- !duplicated(y$period, fromLast = TRUE)
  last_ind <- which(last == TRUE)
  seq(0, y$period[last_ind])
}
