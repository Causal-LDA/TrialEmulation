#' Select Data Columns
#'
#' Select the required columns from the data and rename
#'
#' @param data A `data.frame` containing all the required columns
#' @param args List of arguments from `data_preparation`
#'
#' @returns A data.table with the required columns
#' @keywords internal
select_data_cols <- function(data, args) {
  id <- args$id
  period <- args$period
  treatment <- args$treatment
  outcome <- args$outcome
  eligible <- args$eligible
  eligible_wts_0 <- args$eligible_wts_0
  eligible_wts_1 <- args$eligible_wts_1
  cense <- args$cense
  where_var <- args$where_var

  setnames(
    data,
    old = c(id, period, outcome, eligible, treatment),
    new = c("id", "period", "outcome", "eligible", "treatment")
  )

  formula_vars <- unlist(
    lapply(args[c("outcome_cov", "switch_n_cov", "switch_d_cov", "cense_n_cov", "cense_n_cov")], all.vars)
  )

  cols <- unique(c(eligible_wts_0, eligible_wts_1, cense, where_var, formula_vars))
  cols <- cols[!is.na(cols)]
  derived_col_names <- c("time_on_regime")
  assert_names(cols, subset.of = c(colnames(data), derived_col_names))

  data <- data[,
    c("id", "period", "outcome", "eligible", "treatment", setdiff(cols, derived_col_names)),
    with = FALSE
  ]

  if (test_string(eligible_wts_0)) setnames(data, c(eligible_wts_0), c("eligible_wts_0"))
  if (test_string(eligible_wts_1)) setnames(data, c(eligible_wts_1), c("eligible_wts_1"))

  data[order(id, period)]
}


#' Weight Calculation Function
#'
#' This function performs the calculation for weight of the data
#' @param sw_data A data.table
#' @inheritParams initiators
#' @noRd
#'
weight_func <- function(sw_data,
                        use_switch_weights = use_switch_weights,
                        use_censor_weights = use_censor_weights,
                        switch_n_cov = NA,
                        switch_d_cov = NA,
                        eligible_wts_0 = NA,
                        eligible_wts_1 = NA,
                        cense = NA,
                        pool_cense_n = FALSE,
                        pool_cense_d = FALSE,
                        cense_d_cov = NA,
                        cense_n_cov = NA,
                        save_weight_models = FALSE,
                        data_dir,
                        quiet = FALSE,
                        glm_function = "glm",
                        ...) {
  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  eligible0 <- eligible1 <- id <- period <- eligible0.y <- eligible1.y <- am_1 <-
    treatment <- wt <- wtC <- p0_n <- p0_d <- p1_n <- p1_d <- pC_n0 <- pC_d0 <-
    pC_n1 <- pC_d1 <- pC_n <- pC_d <- NULL

  if (save_weight_models) assert_directory_exists(data_dir)

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Switching weights --------------------
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  switch_models <- list()
  if (use_switch_weights) {
    switch_d_cov <- update.formula(switch_d_cov, treatment ~ .)
    switch_n_cov <- update.formula(switch_n_cov, treatment ~ .)

    switch_results <- fit_switch_weights(
      switch_d_cov = switch_d_cov,
      switch_n_cov = switch_n_cov,
      eligible_wts_0 = eligible_wts_0,
      eligible_wts_1 = eligible_wts_1,
      sw_data = sw_data,
      quiet = quiet,
      save_dir = data_dir,
      save_weight_models = save_weight_models,
      glm_function = glm_function,
      ...
    )
    sw_data <- switch_results$sw_data
    switch_models <- switch_results$switch_models
    rm(switch_results)
  }
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Censoring weights --------------------
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  censor_models <- list()
  if (use_censor_weights) {
    cense_d_cov <- update(cense_d_cov, paste("1 -", cense, "~ ."))
    cense_n_cov <- update(cense_n_cov, paste("1 -", cense, "~ ."))

    censor_results <- fit_censor_weights(
      cense = cense,
      cense_d_cov = cense_d_cov,
      cense_n_cov = cense_n_cov,
      pool_cense_d = pool_cense_d,
      pool_cense_n = pool_cense_n,
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
  }

  # Calculate switching weights
  if (isFALSE(use_switch_weights)) {
    # default weights
    sw_data[, wt := 1.0]
  } else {
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
  }
  if (isFALSE(use_censor_weights)) {
    sw_data[, wtC := 1.0]
  } else {
    sw_data[is.na(pC_d), pC_d := 1]
    sw_data[is.na(pC_n), pC_n := 1]
    sw_data[, wtC := pC_n / pC_d]
  }
  sw_data[, wt := wt * wtC]

  censor_models <- censor_models[intersect(
    c("cens_pool_d", "cens_d0", "cens_n0", "cens_d1", "cens_n1", "cens_pool_n"),
    names(censor_models)
  )]

  list(
    data = sw_data,
    switch_models = switch_models,
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
