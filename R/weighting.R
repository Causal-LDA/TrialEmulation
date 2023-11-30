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
fit_censor_weights <- function(cense,
                               cense_d_cov,
                               cense_n_cov,
                               pool_cense_d,
                               pool_cense_n,
                               sw_data,
                               quiet,
                               save_dir,
                               save_weight_models,
                               glm_function,
                               ...) {
  am_1 <- eligible0 <- eligible1 <- id <- pC_d0 <- pC_d1 <- pC_n0 <- pC_n1 <- period <- NULL

  censor_models <- list()

  if (isTRUE(pool_cense_d)) { # Fit pooled denominator models
    model1.cense <- fit_glm(
      data = sw_data,
      formula = cense_d_cov,
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
      data = sw_data[eligible0 == 1],
      formula = cense_d_cov,
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
      data = sw_data[eligible1 == 1],
      formula = cense_d_cov,
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

  if (isTRUE(pool_cense_n)) { # Fit pooled numerator models
    model2.cense <- fit_glm(
      data = sw_data,
      formula = cense_n_cov,
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
      data = sw_data[eligible0 == 1],
      formula = cense_n_cov,
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
      data = sw_data[eligible1 == 1],
      formula = cense_n_cov,
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
    cense_0 <- cense_d0[cense_n0, on = list(id = id, period = period)]
    cense_1 <- cense_d1[cense_n1, on = list(id = id, period = period)]
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


#' Fit Switching Censoring Models
#'
#' @param switch_d_cov Formula for denominator model
#' @param switch_n_cov Formula for numerator model
#' @param sw_data Data.table object which is modified in place
#' @param quiet Print messages or be quiet?
#' @param save_weight_models Save weight models?
#' @param save_dir Directory to save `glm` model objects
#' @param glm_function `glm` function for model fitting? eg `stats::glm` or `bigglm`
#' @param ... Other parameters passed to `glm`.
#'
#' @return List of model summaries and modified `sw_data` including informative censoring weights
#' @noRd
fit_switch_weights <- function(switch_d_cov,
                               switch_n_cov,
                               eligible_wts_0 = NA,
                               eligible_wts_1 = NA,
                               sw_data,
                               quiet,
                               save_dir,
                               save_weight_models,
                               glm_function,
                               ...) {
  eligible0 <- eligible1 <- id <- period <- NULL
  # Fit the models for the weights in the four scenarios
  switch_models <- list()
  # ------------------- eligible0 == 1 --------------------
  # --------------- denominator ------------------
  model1 <- fit_glm(
    data = sw_data[if (any(!is.na(eligible_wts_0))) (eligible0 == 1 & eligible_wts_0 == 1) else eligible0 == 1],
    formula = switch_d_cov,
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
    data = sw_data[if (any(!is.na(eligible_wts_0))) (eligible0 == 1 & eligible_wts_0 == 1) else eligible0 == 1],
    formula = switch_n_cov,
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
  # --------------- denominator ------------------
  model3 <- fit_glm(
    data = sw_data[if (any(!is.na(eligible_wts_1))) (eligible1 == 1 & eligible_wts_1 == 1) else eligible1 == 1],
    formula = switch_d_cov,
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
    data = sw_data[if (any(!is.na(eligible_wts_1))) (eligible1 == 1 & eligible_wts_1 == 1) else eligible1 == 1],
    formula = switch_n_cov,
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

  switch_0 <- switch_d0[switch_n0, on = list(
    id = id, period = period,
    eligible0 = eligible0
  )]
  switch_1 <- switch_d1[switch_n1, on = list(
    id = id, period = period,
    eligible1 = eligible1
  )]

  rm(switch_d0, switch_d1, switch_n0, switch_n1)

  sw_data <- merge.data.table(sw_data, switch_0[, -c("eligible0")], by = c("id", "period"), all = TRUE)
  sw_data <- merge.data.table(sw_data, switch_1[, -c("eligible1")], by = c("id", "period"), all = TRUE)

  rm(switch_1, switch_0)
  list(sw_data = sw_data, switch_models = switch_models)
}
