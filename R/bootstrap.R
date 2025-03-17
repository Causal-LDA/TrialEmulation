#' Title
#'
#' @param trial_class
#' @param remodel
#' @param new_coef_sw_d0
#' @param new_coef_sw_n0
#' @param new_coef_sw_d1
#' @param new_coef_sw_n1
#' @param new_coef_c_d0
#' @param new_coef_c_n0
#' @param new_coef_c_d1
#' @param new_coef_c_n1
#' @param new_coef_c_d
#' @param new_coef_c_n
#' @param boot_idx
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
weight_func_bootstrap <- function(object = trial_pp,
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
                                  boot_idx = NA,
                                  quiet = T,
                                  ...) {
  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  eligible0 <- eligible1 <- id <- period <- eligible0.y <- eligible1.y <- am_1 <- eligible_wts_0 <- eligible_wts_1 <-
    treatment <- wt <- wtC <- p0_n <- p0_d <- p1_n <- p1_d <- pC_n0 <- pC_d0 <-
    pC_n1 <- pC_d1 <- pC_n <- pC_d <- NULL


  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Switching weights --------------------
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  quiet_msg(quiet, "Starting switching weights")
  switch_models <- list()
    if(remodel == TRUE){

      switch_results <- fit_switch_weights(
        switch_d_cov =  object@switch_weights@denominator,
        switch_n_cov =  object@switch_weights@numerator,
        sw_data = object@data@data,
        weights = weights,
        quiet = quiet,
        save_dir = data_dir,
        save_weight_models = save_weight_models,
        glm_function = glm_function,
        ...
      )
      sw_data <<- switch_results$sw_data
      switch_models <- switch_results$switch_models
      rm(switch_results)
    } else{ #only need to fetch the glm objects if we are recalculating weights from new coefficients
      weight_model_d0 <- readRDS(object@switch_weights@fitted$d0@summary$save_path$path)
      weight_model_n0 <- readRDS(object@switch_weights@fitted$n0@summary$save_path$path)
      weight_model_d1 <- readRDS(object@switch_weights@fitted$d1@summary$save_path$path)
      weight_model_n1 <- readRDS(object@switch_weights@fitted$n1@summary$save_path$path)
      cense_model_d0 = cense_d0
      cense_model_n0 = cense_n0
      cense_model_d1 = cense_d1
      cense_model_n1 = cense_n1

      weight_model_d0$coefficients <- new_coef_sw_d0
      weight_model_n0$coefficients <- new_coef_sw_n0
      weight_model_d1$coefficients <- new_coef_sw_d1
      weight_model_n1$coefficients <- new_coef_sw_n1

      switch_d0 <- cbind(p0_d = predict.glm(weight_model_d0, weight_model_d0_data, type = 'response' ),
                         weight_model_d0_data[, c("eligible0", "id", "period")])

      switch_n0 <- cbind(p0_n = predict.glm(weight_model_n0, weight_model_n0_data, type = 'response' ),
                         weight_model_n0_data[, c("eligible0", "id", "period")])

      switch_d1 <- cbind(p1_d = predict.glm(weight_model_d1, weight_model_d1_data, type = 'response' ),
                         weight_model_d1_data[, c("eligible1", "id", "period")])

      switch_n1 <- cbind(p1_n = predict.glm(weight_model_n1, weight_model_n1_data, type = 'response' ),
                         weight_model_n1_data[, c("eligible1", "id", "period")])

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

    }
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Censoring weights --------------------
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  censor_models <- list()

    if (remodel == TRUE){
      censor_results <- fit_censor_weights(
        cense = cense,
        cense_d_cov = object@censor_weights@denominator,
        cense_n_cov = object@censor_weights@numerator,
        pool_cense_d = object@censor_weights@pool_denominator,
        pool_cense_n = object@censor_weights@pool_numerator,
        sw_data = sw_data,
        quiet = quiet,
        save_dir = data_dir,
        save_weight_models = save_weight_models,
        glm_function = glm_function,
        ...
      )
      sw_data <- censor_results$sw_data
      censor_models <- censor_results$censor_models
      rm(censor_results)
    } else{
      if (isTRUE(pool_cense_d)) { # Fit pooled denominator models
        cense_model_d$coeefficients <- new_coef_c_d

        cense_d <- cbind(pC_d = predict.glm(cense_model_d, cense_model_d_data, type = 'response'),
                         cense_model_d_data[, c("id", "period")])

        censor_models$cens_pool_d <- process_weight_model(
          cense_model_d,
          save_weight_models,
          save_dir,
          "cense_model_pool_d.rds",
          "Model for P(cense = 0 | X) for denominator",
          quiet
        )
      } else{
        cense_model_d0$coefficients <- new_coef_c_d0
        cense_model_d1$coefficients <- new_coef_c_d1

        # Fit separate denominator models for each arm
        # ---------------------- denominator -----------------------
        # ---------------------- eligible0 ---------------------------

        cense_d0 <- cbind(pC_d0 = predict.glm(cense_model_d0, cense_model_d0_data, type = 'response'),
                          cense_model_d0_data[, c("id", "period")])

        rm(cense_model_d0)
        # ------------------------- denominator ---------------------
        # ------------------------ eligible1 -------------------------

        cense_d1 <- cbind(pC_d1 = predict.glm(cense_model_d1, cense_model_d1_data, type = 'response'),
                          cense_model_d1_data[, c("id", "period")])

        rm(cense_model_d1)
      }

      if (isTRUE(pool_cense_n)) { # Fit pooled numerator models
        cense_model_n$coefficients <- new_coef_c_n

        cense_n <- cbind(pC_n = predict.glm(cense_model_n, cense_model_n_data, type = 'response'), cense_model_n_data[, c("id", "period")])

        censor_models$cens_pool_n <- process_weight_model(
          cense_model_n,
          save_weight_models,
          save_dir,
          "cense_model_pool_n.rds",
          "Model for P(cense = 0 | X) for numerator",
          quiet
        )
        rm(cense_model_n)
      } else{ # Fit separate numerator models for each arm
        # -------------------------- numerator ----------------------
        #--------------------------- eligible0 -----------------------
        cense_model_n0$coefficients <- new_coef_c_n0
        cense_model_n1$coefficients <- new_coef_c_n1

        cense_n0 <- cbind(pC_n0 = predict.glm(cense_model_n0, cense_model_n0_data, type = 'response'), cense_model_n0_data[, c("id", "period")])


        rm(cense_model_n0)

        # ------------------------ numerator -------------------------
        # ------------------------- eligible1 -----------------------

        cense_n1 <- cbind(pC_n1 = predict.glm(cense_model_n1, cense_model_n1_data, type = 'response'), cense_model_n1_data[, c("id", "period")])

        rm(cense_model_n1)
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

  sw_data[, first := !duplicated(sw_data[,id])]
  sw_data <- sw_data[!is.na(wt)]
  temp_data <- data.table(
    id = sw_data[, id],
    period = sw_data[, period]
  )
  temp_data[, wtprod := 1.0, by = id]
  #[, elgcount := 0.0, by = id][, expand := 0.0, by = id]
  #temp_data[, treat := 0.0, by = id][, dosesum := 0.0, by = id]

  sw_data[first == TRUE, weight0 := 1.0]
  sw_data[, weight0 := cumprod(wt), by = id]
  temp_data[, wtprod := sw_data[, weight0]]
  #temp_data[, treat := data[, A]]
  #temp_data[, dosesum := data[, CAp]]
  #temp_data[, elgcount := data[, eligible]]
  #temp_data[data[, eligible] == 1, init := data[eligible == 1, A]]
  #temp_data[, init_shift := shift(data[, A])]
  #temp_data[data[, eligible] == 0, init := init_shift, by = id]
  #temp_data[, init_shift := NULL]

  expand_index <- rep(seq_len(nrow(sw_data)), sw_data[, period] + 1)

  quiet_msg(quiet, "Adding new weights to expanded data")

  ### new_data only contains ID, trial_period, followup_time adn the new IP weights
  new_data <- data.table(id = sw_data[expand_index, id])
  new_data[, period_new := sw_data[expand_index, period]]
  #new_data[, cumA_new := data[expand_index, CAp]]
  #new_data[, treatment_new := data[expand_index, A]]

  quiet_msg(quiet, "Placer 1")
  #new_data[, outcome_new := data[expand_index, Y]]
  new_data[, weight0 := sw_data[expand_index, weight0]]
  quiet_msg(quiet, "Placer 1")
  new_data[, trial_period := trial_period_func(sw_data)]
  quiet_msg(quiet, "Placer 1")
  new_data[, index := seq_len(.N)]

  quiet_msg(quiet, "Placer 2")
  new_data <- new_data[temp_data, on = list(id = id, trial_period = period)]
  setorder(new_data, index)
  new_data[, followup_time := period_new - trial_period]
  new_data[, weight := (weight0 / wtprod)]

  quiet_msg(quiet, "Placer 3")
  #### New data is merged with existing expanded data to add the new weights
  output_data <- new_data[object@outcome_data@data, on = list( id = id, trial_period = trial_period,
                                                    followup_time = followup_time)] %>%
    dplyr::select(names(object@outcome_data@data))

  list(
    data = output_data,
    switch_models = switch_models,
    censor_models = censor_models
  )
}

fit_switch_weights <- function(switch_d_cov,
                               switch_n_cov,
                               eligible_wts_0 = NA,
                               eligible_wts_1 = NA,
                               sw_data,
                               weights,
                               quiet,
                               save_dir,
                               save_weight_models,
                               glm_function,
                               ...) {
  eligible0 <- eligible1 <- id <- period <- NULL
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
    data = sw_data[model_0_index, ] %>% rowwise() %>%
      dplyr::mutate(weight_boot = length(boot_idx[boot_idx == id])),
    formula = switch_d_cov,
    weights = weight_boot,
    ...,
    glm_function = glm_function
  )

  switch_d0 <<- cbind(p0_d = model1$fitted.values, model1$data[, c("eligible0", "id", "period")])

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
    data = sw_data[model_0_index, ] %>% rowwise() %>%
      dplyr::mutate(weight_boot = length(boot_idx[boot_idx == id])),
    formula = switch_n_cov,
    weights = weight_boot,
    ...,
    glm_function = glm_function
  )

  switch_n0 <<- cbind(p0_n = model2$fitted.values, model2$data[, c("eligible0", "id", "period")])

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
  data_1_expr <- if ("eligible_wts_1" %in% colnames(object@data@data)) {
    expression(am_1 == 1 & eligible_wts_1 == 1)
  } else {
    expression(am_1 == 1)
  }
  model_1_index <- object@data@data[eval(data_1_expr), which = TRUE]
  # --------------- denominator ------------------
  model3 <- fit_glm(
    data = sw_data[model_1_index, ] %>% rowwise() %>%
      dplyr::mutate(weight_boot = length(boot_idx[boot_idx == id])),
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
    data = sw_data[model_1_index, ] %>% rowwise() %>%
      dplyr::mutate(weight_boot = length(boot_idx[boot_idx == id])),
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

  switch_0 <- merge.data.table(switch_d0,switch_n0,
                               by= c("id", "period", "eligible0"))
  switch_1 <- merge.data.table(switch_d1,switch_n1,
                               by= c("id", "period", "eligible1"))

  rm(switch_d0, switch_d1, switch_n0, switch_n1)

  sw_data <- merge.data.table(sw_data, switch_0 %>% dplyr::select(-eligible0), by = c("id", "period"), all = TRUE)
  sw_data <- merge.data.table(sw_data, switch_1 %>% dplyr::select(-eligible1), by = c("id", "period"), all = TRUE)

  rm(switch_1, switch_0)
  list(sw_data = sw_data, switch_models = switch_models)
}
fit_glm <- function(formula, data, weights, ..., glm_function = "glm") {
  this_call <- match.call(expand.dots = FALSE)
  dots <- list(...)
  this_call$`...` <- NULL
  this_call$glm_function <- NULL
  if (is.null(this_call$family)) this_call$family <- quote(binomial(link = "logit"))
  this_call$formula <- formula
  this_call$data <- quote(data)


  if (glm_function == "parglm") {
    if (!any(c("nthreads", "control", "method") %in% names(dots))) {
      warning(
        "Argument glm_function = \"parglm\" but no `nthreads`, `method` or `control` specified.\n",
        "Using `control = parglm.control(nthreads = 4, method = \"FAST\")`"
      )
      this_call$control <- parglm::parglm.control(nthreads = 4, method = "FAST")
    }
  }
  this_call[[1]] <- call(glm_function)[[1]]
  for (i in names(dots)) {
    this_call[[i]] <- dots[[i]]
  }
  eval(this_call)
}

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

library(data.table)

f <- function(y) {
  last <- !duplicated(y$period, fromLast = TRUE)
  last_ind <- which(last == TRUE)
  return(seq(0, y$period[last_ind]))
}

trial_period_func <- function(x) {
  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  period <- id <- trial_period <- NULL

  x_new <- x[rep(seq_len(.N), period + 1), list(id, period)]
  x_new[, trial_period := f(.BY), by = list(id, period)]
  return(x_new[, trial_period])
}

quiet_print <- function(quiet, x, ...) {
  if (isFALSE(quiet)) {
    print(x, ...)
  }
}

#' Conditional Messages
#'
#' @param quiet (`logical`) Messages printed if `FALSE`
#' @param x Object to print.
#' @param ... Passed to `message` function.
#'
#' @noRd
quiet_msg <- function(quiet, x, ...) {
  if (isFALSE(quiet)) {
    message(x, ...)
  }
}

#' Print a line
#'
#' @param quiet Print if `TRUE `
#' @noRd
quiet_line <- function(quiet) {
  quiet_msg(quiet, paste0(strrep("-", 0.75 * getOption("width")), "\n"))
}

#' Print with timing statement
#'
#' @param quiet Print if `TRUE `,
#' @param x Message to print
#' @param proc_time Result of `system.time()`. Elapsed time will be extracted,
#' formatted for printing and `paste0()`ed to `x`.
#' @noRd
quiet_msg_time <- function(quiet, msg, proc_time) {
  time <- proc_time["elapsed"]
  time <- if (time < 10) sprintf("%0.1f s", time) else sprintf("%.5g s", time)
  quiet_msg(quiet, paste0(msg, time))
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

