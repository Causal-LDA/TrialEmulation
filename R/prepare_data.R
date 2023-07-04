#' Prepare Data for Trial Emulation
#'
#' @param data A data frame
#' @param setup A `TE_setup` object from [trial_emulation_setup()]
#'
#' @return A `TE_data` object containing a data frame `data`
#' @export
prepare_data <- function(data, setup) {
  validate_TE_setup(setup, data)
  data_setup <- get_data_setup(setup)

  TE_data <- select_data_cols(
    data,
    id = data_setup$id,
    period = data_setup$period,
    treatment = data_setup$treatment,
    outcome = data_setup$outcome,
    eligible = data_setup$eligible,
    eligible_wts_0 = data_setup$eligible_wts_0,
    eligible_wts_1 = data_setup$eligible_wts_1,
    formula_vars = data_setup$formula_vars,
    cense = data_setup$cense,
    where_var = data_setup$where_var
  )

  TE_data <- data_manipulation(TE_data, use_censor = data_setup$use_censor)

  result <- list(data = TE_data, data_setup = data_setup)
  result
}


calculate_weights <- function(data, setup) {
  if (is.null(setup$treatment_switching) && is.null(setup$censoring)) {
    stop("No setup for treatment switching or informative censoring weight models defined.")
  }

  object <- weight_func(
    sw_data = data$data,
    switch_n_cov = setup$treatment_switching$switch_n_model,
    switch_d_cov = setup$treatment_switching$switch_d_model,
    eligible_wts_0 = setup$treatment_switching$eligible_wts_0,
    eligible_wts_1 = setup$treatment_switching$eligible_wts_1,
    cense = setup$censoring$censored,
    pool_cense = setup$censoring$pool_models,
    cense_d_cov = setup$censoring$cense_d_model,
    cense_n_cov = setup$censoring$cense_n_model,
    save_weight_models = FALSE,
    save_dir = NA,
    quiet = TRUE,
    glm_function = "glm"
  )

  keep_wt_cols <- intersect(
    c(
      "id", "period", "wt", "wtS", "wtC",
      "p0_d", "p0_n", "p1_d", "p1_n",
      "pC_d0", "pC_n0", "pC_d1", "pC_n1", "pC_n", "pC_d"
    ),
    colnames(object$data)
  )
  object$data <- object$data[, keep_wt_cols, with = FALSE]
  class(object) <- "TE_weights"
  object
}

#' @export
weights.TE_weights <- function(object, weight_type = c("combined", "switching", "censoring"), ...) {
  weight_type <- match.arg(weight_type)
  switch(weight_type,
    "combined" = object$data$wt,
    "switching" = object$data$wtS,
    "censoring" = object$data$wtC
  )
}

#' @export
hist.TE_weights <- function(x, weight_type = c("combined", "switching", "censoring"), ...) {
  do.call(hist, c(
    list(x = substitute(weights(te_weights, .WT), list(.WT = match.arg(weight_type)))),
    list(...)
  ))
}

expand_trial_sequence <- function(data, settings) {
  data_extension(data, settings)
}



expand_trial_sequence <- function(data, settings) {
  data_extension(data, settings)
}
