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
                        save_weight_models = "tidy",
                        save_dir,
                        quiet = FALSE) {
  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  eligible0 <- eligible1 <- id <- period <- eligible0.y <- eligible1.y <- am_1 <-
    treatment <- wt <- wtC <- p0_n <- p0_d <- p1_n <- p1_d <- pC_n0 <- pC_d0 <-
    pC_n1 <- pC_d1 <- pC_n <- pC_d <- NULL

  save_tidy <- ("tidy" %in% save_weight_models)
  save_object <- ("object" %in% save_weight_models)
  if (save_tidy || save_object) assert_directory_exists(save_dir)

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
  h_quiet_print(quiet, "P(treatment=1 | treatment=0) for denominator")

  model1 <- weight_lr(
    sw_data[if (any(!is.na(eligible_wts_0))) (eligible0 == 1 & eligible_wts_0 == 1) else eligible0 == 1],
    switch_d_cov
  )

  h_quiet_print(quiet, summary(model1))
  switch_d0 <- data.table(
    p0_d = model1$fitted.values,
    eligible0 = unlist(model1$data$eligible0),
    id = model1$data[, id],
    period = model1$data[, period]
  )

  if (save_tidy) {
    weight_models$switch_d0 <- broom::tidy(model1)
    weight_models$switch_d0_statistics <- broom::glance(model1)
  } else if (save_object) {
    saveRDS(model1, file = file.path(save_dir, "weight_model_switch_d0.rds"))
  }
  rm(model1)

  # -------------- numerator --------------------

  h_quiet_print(quiet, "P(treatment=1 | treatment=0) for numerator")

  model2 <- weight_lr(
    sw_data[if (any(!is.na(eligible_wts_0))) (eligible0 == 1 & eligible_wts_0 == 1) else eligible0 == 1],
    switch_n_cov,
  )

  h_quiet_print(quiet, summary(model2))
  switch_n0 <- data.table(
    p0_n = model2$fitted.values,
    eligible0 = unlist(model2$data$eligible0),
    id = model2$data[, id],
    period = model2$data[, period]
  )

  if (save_tidy) {
    weight_models$switch_n0 <- broom::tidy(model2)
    weight_models$switch_n0_statistics <- broom::glance(model2)
  } else if (save_object) {
    saveRDS(model2, file = file.path(save_dir, "weight_model_switch_n0.rds"))
  }
  rm(model2)

  # ------------------- eligible1 == 1 --------------------
  # --------------- denominator ------------------
  h_quiet_print(quiet, "P(treatment=1 | treatment=1) for denominator")
  model3 <- weight_lr(
    sw_data[if (any(!is.na(eligible_wts_1))) (eligible1 == 1 & eligible_wts_1 == 1) else eligible1 == 1],
    switch_d_cov
  )

  h_quiet_print(quiet, summary(model3))
  switch_d1 <- data.table(
    p1_d = model3$fitted.values,
    eligible1 = unlist(model3$data$eligible1),
    id = model3$data[, id],
    period = model3$data[, period]
  )

  if (save_tidy) {
    weight_models$switch_d1 <- broom::tidy(model3)
    weight_models$switch_statistics <- broom::glance(model3)
  } else if (save_object) {
    saveRDS(model3, file = file.path(save_dir, "weight_model_switch_d1.rds"))
  }
  rm(model3)

  # -------------------- numerator ---------------------------
  h_quiet_print(quiet, "P(treatment=1 | treatment=1) for numerator")
  model4 <- weight_lr(
    sw_data[if (any(!is.na(eligible_wts_1))) (eligible1 == 1 & eligible_wts_1 == 1) else eligible1 == 1],
    switch_n_cov
  )

  h_quiet_print(quiet, summary(model4))
  switch_n1 <- data.table(
    p1_n = model4$fitted.values,
    eligible1 = unlist(model4$data$eligible1),
    id = model4$data[, id],
    period = model4$data[, period]
  )


  if (save_tidy) {
    weight_models$switch_n1 <- broom::tidy(model4)
    weight_models$switch_n1_statistics <- broom::glance(model4)
  } else if (save_object) {
    saveRDS(model4, file = file.path(save_dir, "weight_model_switch_n1.rds"))
  }
  rm(model4)


  # -------------- Combine results --------------------
  if (save_tidy) {
    save(weight_models, file = file.path(save_dir, "tidy_weight_models.rda"))
  }
  rm(weight_models)

  switch_0 <- switch_d0[switch_n0, on = list(
    id = id, period = period,
    eligible0 = eligible0
  )]
  switch_1 <- switch_d1[switch_n1, on = list(
    id = id, period = period,
    eligible1 = eligible1
  )]

  rm(switch_d0, switch_d1, switch_n0, switch_n1)

  sw_data <- merge.data.table(sw_data, switch_0, by = c("id", "period"), all = TRUE)
  sw_data <- merge.data.table(sw_data, switch_1, by = c("id", "period"), all = TRUE)

  rm(switch_1, switch_0)

  sw_data[, eligible0.y := NULL]
  sw_data[, eligible1.y := NULL]
  setnames(sw_data, c("eligible0.x", "eligible1.x"), c("eligible0", "eligible1"))


  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Censoring weights --------------------
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.na(cense)) {
    cens_models <- list()
    cense_d_cov <- update(cense_d_cov, paste("1 -", cense, "~ ."))
    cense_n_cov <- update(cense_n_cov, paste("1 -", cense, "~ ."))

    if (pool_cense == 1) {
      # -------------------- denominator -------------------------
      h_quiet_print(quiet, "Model for P(cense = 0 |  X ) for denominator")
      # -----------------------------------------------------------
      model1.cense <- weight_lr(sw_data, cense_d_cov)
      h_quiet_print(quiet, summary(model1.cense))
      cense_d0 <- data.table(
        pC_d = model1.cense$fitted.values,
        id = model1.cense$data[, id],
        period = model1.cense$data[, period]
      )

      if (save_tidy) {
        cens_models$cens_pool_d <- broom::tidy(model1.cense)
        cens_models$cens_pool_d_statistics <- broom::glance(model1.cense)
      } else if (save_object) {
        saveRDS(model1.cense, file = file.path(save_dir, "cense_model_pool_d.rds"))
      }
      rm(model1.cense)

      # --------------------- numerator ---------------------------
      h_quiet_print(quiet, "Model for P(cense = 0 |  X ) for numerator")
      # ---------------------------------------------------------
      model2.cense <- weight_lr(sw_data, cense_n_cov)
      h_quiet_print(quiet, summary(model2.cense))
      cense_n0 <- data.table(
        pC_n = model2.cense$fitted.values,
        id = model2.cense$data[, id],
        period = model2.cense$data[, period]
      )

      if (save_tidy) {
        cens_models$cens_pool_n <- broom::tidy(model2.cense)
        cens_models$cens_pool_n_statistics <- broom::glance(model2.cense)
      } else if (save_object) {
        saveRDS(model2.cense, file = file.path(save_dir, "cense_model_pool_n.rds"))
      }
      rm(model2.cense)
      sw_data <- merge.data.table(sw_data, cense_d0, by = c("id", "period"), all = TRUE)
      sw_data <- merge.data.table(sw_data, cense_n0, by = c("id", "period"), all = TRUE)

      rm(cense_d0, cense_n0)
    } else {
      # when pool_cense != 1

      # ---------------------- denominator -----------------------
      h_quiet_print(quiet, "Model for P(cense = 0 |  X, Am1=0) for denominator")
      # ---------------------- eligible0 ---------------------------

      model1.cense <- weight_lr(sw_data[eligible0 == 1], cense_d_cov)
      h_quiet_print(quiet, summary(model1.cense))
      cense_d0 <- data.table(
        pC_d0 = model1.cense$fitted.values,
        id = model1.cense$data[, id],
        period = model1.cense$data[, period]
      )

      if (save_tidy) {
        cens_models$cens_d0 <- broom::tidy(model1.cense)
        cens_models$cens_d0_statistics <- broom::glance(model1.cense)
      } else if (save_object) {
        saveRDS(model1.cense, file = file.path(save_dir, "cense_model_d0.rds"))
      }
      rm(model1.cense)
      # -------------------------- numerator ----------------------
      h_quiet_print(quiet, "Model for P(cense = 0 |  X, Am1=0) for numerator")
      #--------------------------- eligible0 -----------------------
      model2.cense <- weight_lr(sw_data[eligible0 == 1], cense_n_cov)
      h_quiet_print(quiet, summary(model2.cense))
      cense_n0 <- data.table(
        pC_n0 = model2.cense$fitted.values,
        id = model2.cense$data[, id],
        period = model2.cense$data[, period]
      )

      if (save_tidy) {
        cens_models$cens_n0 <- broom::tidy(model2.cense)
        cens_models$cens_n0_statistics <- broom::glance(model2.cense)
      } else if (save_object) {
        saveRDS(model2.cense, file = file.path(save_dir, "cense_model_n0.rds"))
      }
      rm(model2.cense)
      # ------------------------- denominator ---------------------
      h_quiet_print(quiet, "Model for P(cense = 0 |  X, Am1=1) for denominator")
      # ------------------------ eligible1 -------------------------
      model3.cense <- weight_lr(sw_data[eligible1 == 1], cense_d_cov)
      h_quiet_print(quiet, summary(model3.cense))
      cense_d1 <- data.table(
        pC_d1 = model3.cense$fitted.values,
        id = model3.cense$data[, id],
        period = model3.cense$data[, period]
      )

      if (save_tidy) {
        cens_models$cens_d1 <- broom::tidy(model3.cense)
        cens_models$cens_d1_statistics <- broom::glance(model3.cense)
      } else if (save_object) {
        saveRDS(model3.cense, file = file.path(save_dir, "cense_model_d1.rds"))
      }
      rm(model3.cense)
      # ------------------------ numerator -------------------------
      h_quiet_print(quiet, "Model for P(cense = 0 |  X, Am1=1) for numerator")
      # ------------------------- eligible1 -----------------------
      model4.cense <- weight_lr(sw_data[eligible1 == 1], cense_n_cov)
      h_quiet_print(quiet, summary(model4.cense))
      cense_n1 <- data.frame(
        pC_n1 = model4.cense$fitted.values,
        id = model4.cense$data[, id],
        period = model4.cense$data[, period]
      )

      if (save_tidy) {
        cens_models$cens_n1 <- broom::tidy(model4.cense)
        cens_models$cens_n1_statistics <- broom::glance(model4.cense)
      } else if (save_object) {
        saveRDS(model4.cense, file = file.path(save_dir, "cense_model_n1.rds"))
      }
      rm(model4.cense)

      # combine ------------------------------
      if (save_tidy) {
        save(cens_models, file = file.path(save_dir, "tidy_cens_models.rda"))
      }
      rm(cens_models)

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

  return(sw_data)
}
