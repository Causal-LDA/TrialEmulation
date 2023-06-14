#' Prepare Data for Trial Emulation
#'
#' @param data A data frame
#' @param setup A `TE_setup` object from [trial_emulation_setup()]
#'
#' @return A `TE_data` object containing a data frame `data`
#' @export
prepare_data <- function(data,
                         setup) {
  check_setup(setup)
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


calculate_weights <- function(data,
                              setup) {
  weight_func(
    data,
    sw_data = data,
    switch_n_cov = setup$treatment_switching$switch_n_cov,
    switch_d_cov = setup$treatment_switching$switch_d_cov,
    eligible_wts_0 = setup$treatment_switching$eligible_wts_0,
    eligible_wts_1 = setup$treatment_switching$eligible_wts_1,
    cense = setup$censoring$censored,
    pool_cense = setup$censoring$pool_cense,
    cense_d_cov = setup$censoring$cense_d_cov,
    cense_n_cov = setup$censoring$cense_n_cov,
    save_weight_models = save_weight_models,
    save_dir = data_dir,
    quiet = quiet,
    glm_function = glm_function
  )
}

set_weights <- function(data, weights_object) {
  data$weights <- weights(weights_object)
}

expand_trial_sequence <- function(data, settings) {
  data_extension(data, settings)
}



calculate_weights <- function(data,
                              settings_object) {
  weight_func(data, settings)
}

set_weights <- function(data, weights_object) {
  data$weights <- weights(weights_object)
}

expand_trial_sequence <- function(data, settings) {
  data_extension(data, settings)
}
