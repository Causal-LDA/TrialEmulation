#' Select Data Columns
#'
#' Select the required columns from the data and rename
#'
#' @param data A `data.frame` containing all the required columns
#' @param id Name of the `data` column for id feature Defaults to "id"
#' @param period Name of the `data` column for period feature Defaults to "period"
#' @param treatment Name of the `data` column for treatment feature Defaults to "treatment"
#' @param outcome Name of the `data` column for outcome feature Defaults to "outcome"
#' @param eligible Name of `data` column for indicator of whether or not an observation is eligible for a trial.
#' Defaults to "eligible".
#' @param eligible_wts_0 Eligibility criteria used in weights for model condition Am1 = 0
#' @param eligible_wts_1 Eligibility criteria used in weights for model condition Am1 = 1
#' @param cense Censoring variable
#' @param where_var Variables used in where conditions used in subsetting the data used in final analysis (where_case),
#' @param formula_vars Variables used in outcome or weight models.
#'  the variables not included in the final model.
select_data_cols <- function(data,
                             id = "id",
                             period = "period",
                             treatment = "treatment",
                             outcome = "outcome",
                             eligible = "eligible",
                             eligible_wts_0,
                             eligible_wts_1,
                             formula_vars,
                             cense,
                             where_var) {
  if (!eligible %in% colnames(data)) {
    warning(paste0("Eligibility variable not found in data: ", eligible))
    warning("Eligibility set to 1 for all patients for all periods")
    data[eligible] <- 1
  }

  cols <- unique(c(
    eligible_wts_0, eligible_wts_1, formula_vars, cense, where_var,
    id, period, treatment, outcome, eligible
  ))
  cols <- cols[!is.na(cols)]
  assert_subset(cols, colnames(data))

  data_new <- setDT(data)[, cols, with = FALSE]

  setnames(
    data_new,
    old = c(id, period, outcome, eligible, treatment),
    new = c("id", "period", "outcome", "eligible", "treatment")
  )

  if (test_string(eligible_wts_0)) setnames(data_new, c(eligible_wts_0), c("eligible_wts_0"))
  if (test_string(eligible_wts_1)) setnames(data_new, c(eligible_wts_1), c("eligible_wts_1"))

  data_new[order(id, period)]
}


#' Period Expanding Function
#'
#' This function get the data.table with period column and expand it based on it
#' @param y The data.table with period column

f <- function(y) {
  last <- !duplicated(y$period, fromLast = TRUE)
  last_ind <- which(last == TRUE)
  return(seq(0, y$period[last_ind]))
}

#' For_period Feature Function
#'
#' This function get the data.table with period and id columns and generate the for_period feature
#' @param x The data.table with id and period columns
#' for_period_func()

for_period_func <- function(x) {
  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  period <- id <- for_period <- NULL

  x_new <- x[rep(seq_len(.N), period + 1), list(id, period)]
  x_new[, for_period := f(.BY), by = list(id, period)]
  return(x_new[, for_period])
}

#' Weight Calculation Function
#'
#' This function performs the calculation for weight of the data
#' @param sw_data A data.table
#' @param save_dir Directory to save tidy weight model summaries in as 'weight_models.rda'
#' @inheritParams initiators
#'
weight_func <- function(sw_data,
                        switch_n_cov = NA,
                        switch_d_cov = NA,
                        eligible_wts_0 = NA,
                        eligible_wts_1 = NA,
                        cense = NA,
                        pool_cense = 0,
                        cense_d_cov = NA,
                        cense_n_cov = NA,
                        include_regime_length = 0,
                        save_weight_models = FALSE,
                        save_dir,
                        quiet = FALSE,
                        glm_function = "glm",
                        ...) {
  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  eligible0 <- eligible1 <- id <- period <- eligible0.y <- eligible1.y <- am_1 <-
    treatment <- wt <- wtC <- p0_n <- p0_d <- p1_n <- p1_d <- pC_n0 <- pC_d0 <-
    pC_n1 <- pC_d1 <- pC_n <- pC_d <- NULL

  if (save_weight_models) assert_directory_exists(save_dir)

  switch_d_cov <- update.formula(switch_d_cov, treatment ~ .)
  switch_n_cov <- update.formula(switch_n_cov, treatment ~ .)

  if (include_regime_length == 1) {
    switch_d_cov <- update.formula(switch_d_cov, ~ . + time_on_regime + I(time_on_regime^2))
    switch_n_cov <- update.formula(switch_n_cov, ~ . + time_on_regime + I(time_on_regime^2))
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Switching weights --------------------
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Fit the models for the weights in the four scenarios
  weight_models <- list()
  # ------------------- eligible0 == 1 --------------------
  # --------------- denominator ------------------
  model1 <- fit_glm(
    data = sw_data[if (any(!is.na(eligible_wts_0))) (eligible0 == 1 & eligible_wts_0 == 1) else eligible0 == 1],
    formula = switch_d_cov,
    ...,
    glm_function = glm_function
  )

  switch_d0 <- cbind(p0_d = model1$fitted.values, model1$data[, c("eligible0", "id", "period")])

  weight_models$switch_d0 <- process_weight_model(
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

  weight_models$switch_n0 <- process_weight_model(
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

  weight_models$switch_d1 <- process_weight_model(
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

  weight_models$switch_n1 <- process_weight_model(
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

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Censoring weights --------------------
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  censor_models <- list()
  if (!is.na(cense)) {
    cense_d_cov <- update(cense_d_cov, paste("1 -", cense, "~ ."))
    cense_n_cov <- update(cense_n_cov, paste("1 -", cense, "~ ."))

    if (pool_cense == 1) {
      # -------------------- denominator -------------------------
      model1.cense <- fit_glm(
        data = sw_data,
        formula = cense_d_cov,
        ...,
        glm_function = glm_function
      )

      cense_d0 <- cbind(pC_d = model1.cense$fitted.values, model1.cense$data[, c("id", "period")])

      censor_models$cens_pool_d <- process_weight_model(
        model1.cense,
        save_weight_models,
        save_dir,
        "cense_model_pool_d.rds",
        "Model for P(cense = 0 | X) for denominator",
        quiet
      )
      rm(model1.cense)

      # --------------------- numerator ---------------------------
      model2.cense <- fit_glm(
        data = sw_data,
        formula = cense_n_cov,
        ...,
        glm_function = glm_function
      )

      cense_n0 <- cbind(pC_n = model2.cense$fitted.values, model2.cense$data[, c("id", "period")])

      censor_models$cens_pool_n <- process_weight_model(
        model2.cense,
        save_weight_models,
        save_dir,
        "cense_model_pool_n.rds",
        "Model for P(cense = 0 | X) for numerator",
        quiet
      )
      rm(model2.cense)

      sw_data <- merge.data.table(sw_data, cense_d0, by = c("id", "period"), all = TRUE)
      sw_data <- merge.data.table(sw_data, cense_n0, by = c("id", "period"), all = TRUE)

      rm(cense_d0, cense_n0)
    } else {
      # when pool_cense != 1

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

      # combine ------------------------------
      cense_0 <- cense_d0[cense_n0, on = list(id = id, period = period)]
      cense_1 <- cense_d1[cense_n1, on = list(id = id, period = period)]
      rm(cense_n1, cense_d1, cense_n0, cense_d0)

      sw_data <- merge.data.table(sw_data, cense_0, by = c("id", "period"), all = TRUE)
      sw_data <- merge.data.table(sw_data, cense_1, by = c("id", "period"), all = TRUE)

      rm(cense_0, cense_1)
    }
  }
  # wt and wtC calculation
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

  if (is.na(cense)) {
    sw_data[, wtC := 1.0]
  } else {
    if (pool_cense == 0) {
      sw_data[am_1 == 0, `:=`(pC_n = pC_n0, pC_d = pC_d0)]
      sw_data[am_1 == 1, `:=`(pC_n = pC_n1, pC_d = pC_d1)]
    }
    sw_data[is.na(pC_d), pC_d := 1]
    sw_data[is.na(pC_n), pC_n := 1]
    sw_data[, wtC := pC_n / pC_d]
  }
  sw_data[, wt := wt * wtC]

  list(
    data = sw_data,
    switch_models = weight_models,
    censor_models = censor_models
  )
}

#' Helper to Process Weight Models
#'
#' @param model glm model object
#' @param save_weight_models whether to save model objects TRUE/FALSE
#' @param save_dir directory to save to
#' @param filename filename for saved model object
#' @param description short description of model
#' @param quiet Don't print model summary
#' @noRd
process_weight_model <- function(model, save_weight_models, save_dir, filename, description, quiet) {
  quiet_msg(quiet, description)
  quiet_print(quiet, summary(model))
  result <- list(
    description = description,
    summary = broom::tidy(model),
    fit_summary = broom::glance(model)
  )
  if (save_weight_models) {
    result$path <- file.path(save_dir, filename)
    saveRDS(model, file = result$path)
  }
  class(result) <- c("TE_weight_summary", "list")
  result
}
