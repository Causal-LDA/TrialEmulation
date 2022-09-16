#' Data Preparation Function
#'
#' This function prepare the data for modelling.
#' @inheritParams initiators
#' @param chunk_size Number of patients to process in one chunk when `separate_files = TRUE`
#' @param separate_files Save expanded data in separate CSV files for each trial.
#' @export
#'
#' @details
#' The arguments `chunk_size` and `separate_files` allow for processing of large datasets that
#' would not fit in memory once expanded. When `separate_files = TRUE`, the input data are processed
#' in chunks of patients and saved into separate files for each trial starting period. These separate
#' files can be sampled to create the dataset for the modelling.
data_preparation <- function(data,
                             id = "id",
                             period = "period",
                             treatment = "treatment",
                             outcome = "outcome",
                             eligible = "eligible",
                             outcome_cov = ~1,
                             model_var = NULL,
                             switch_n_cov = ~1,
                             switch_d_cov = ~1,
                             first_period = NA,
                             last_period = NA,
                             use_weight = 0,
                             use_censor = 0,
                             check_missing = 0,
                             cense = NA,
                             pool_cense = 0,
                             cense_d_cov = ~1,
                             cense_n_cov = ~1,
                             include_followup_time_case = ~ followup_time + I(followup_time^2),
                             include_expansion_time_case = ~ for_period + I(for_period^2),
                             include_regime_length = FALSE,
                             eligible_wts_0 = NA,
                             eligible_wts_1 = NA,
                             lag_p_nosw = 1,
                             where_var = NULL,
                             data_dir,
                             numCores = 1,
                             chunk_size = 500,
                             separate_files = FALSE,
                             quiet = FALSE) {
  assert_flag(quiet)
  assert_flag(separate_files)
  outcome_cov <- as_formula(outcome_cov)
  switch_n_cov <- as_formula(switch_n_cov)
  switch_d_cov <- as_formula(switch_d_cov)
  cense_d_cov <- as_formula(cense_d_cov)
  cense_n_cov <- as_formula(cense_n_cov)
  include_followup_time_case <- as_formula(include_followup_time_case)
  include_expansion_time_case <- as_formula(include_expansion_time_case)

  model_var <- if (!is.null(model_var)) {
    as_formula(model_var)
  } else if (use_censor == 0 && use_weight == 1) {
    ~dose
  } else {
    ~assigned_treatment
  }

  data <- select_data_cols(
    data,
    id = id,
    period = period,
    treatment = treatment,
    outcome = outcome,
    eligible = eligible,
    eligible_wts_0 = eligible_wts_0,
    eligible_wts_1 = eligible_wts_1,
    formula_vars = unlist(lapply(list(outcome_cov, switch_n_cov, switch_d_cov, cense_n_cov, cense_n_cov), all.vars)),
    cense = cense,
    where_var = where_var
  )

  h_quiet_print(quiet, "Start data manipulation")
  timing <- system.time({
    data <- data_manipulation(data, use_censor = use_censor)
  })
  h_quiet_print(quiet, "Finish data manipulation")
  h_quiet_print(quiet, "Processing time of data manipulation:")
  h_quiet_print(quiet, timing)
  h_quiet_print(quiet, "----------------------------")

  if (use_weight == 1) {
    data <- weight_func(
      sw_data = data,
      switch_n_cov = switch_n_cov,
      switch_d_cov = switch_d_cov,
      eligible_wts_0 = eligible_wts_0,
      eligible_wts_1 = eligible_wts_1,
      cense = cense,
      pool_cense = pool_cense,
      cense_d_cov = cense_d_cov,
      cense_n_cov = cense_n_cov,
      include_regime_length = include_regime_length,
      save_dir = data_dir,
      quiet = quiet
    )
  } else if (use_weight == 0) {
    set(data, j = "wt", value = 1)
  }

  keeplist <- c(
    "id", "for_period", "followup_time", "outcome", "weight", "treatment",
    where_var, all.vars(outcome_cov), all.vars(model_var)
  )

  h_quiet_print(quiet, "Start data extension")
  timing <- system.time({
    if (!separate_files) {
      extended_data <- data_extension(
        data = data,
        keeplist,
        outcomeCov_var = all.vars(outcome_cov),
        first_period,
        last_period,
        use_censor,
        lag_p_nosw,
        where_var = where_var
      )
    } else {
      extended_data <- data_extension_parallel(
        data = data,
        keeplist = keeplist,
        outcomeCov_var = all.vars(outcome_cov),
        first_period = first_period,
        last_period = last_period,
        use_censor = use_censor,
        lag_p_nosw = lag_p_nosw,
        where_var = where_var,
        data_dir = data_dir,
        chunk_size = chunk_size
      )
    }
  })
  h_quiet_print(quiet, "Finish data extension")
  h_quiet_print(quiet, "Processing time of data extension:")
  h_quiet_print(quiet, timing)
  h_quiet_print(quiet, "----------------------------")

  h_quiet_print(quiet, paste0("Number of observations in expanded data: ", extended_data$N))

  return(extended_data)
}
