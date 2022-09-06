#' Data Preparation Function
#'
#' This function prepare the data for modelling.
#' @inheritParams initiators
#' @export
#'
#' @details The class variables parameters (`outcomeClass`,`class_switchn`,`class_switchd`,`class_censen`,
#' `class_censed`) can be given as a character vector which will construct factors using `as.factor` or as a named list
#' with the arguments for factor e.g.
#' `list(risk_cat=list(levels = c(1,2,3,0), age_cat=list(levels=c(1,2,3),labels=c("50-60","60-70","70+")`
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

  data <- select_data_cols(
    data,
    id = id,
    period = period,
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
      data = data,
      model_switchn = switch_n_cov,
      model_switchd = switch_d_cov,
      eligible_wts_0 = eligible_wts_0,
      eligible_wts_1 = eligible_wts_1,
      cense = cense,
      pool_cense = pool_cense,
      model_censed = cense_d_cov,
      model_censen = cense_n_cov,
      include_regime_length = include_regime_length,
      numCores = numCores,
      data_dir = data_dir,
      quiet = quiet
    )
  } else if (use_weight == 0) {
    data[, wt := 1]
  }

  keeplist <- c(
    "id", "for_period", "followup_time", "outcome",
    "weight", "treatment", "assigned_treatment", "dose",
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
        keeplist,
        outcomeCov_var = all.vars(outcome_cov),
        first_period,
        last_period,
        use_censor,
        lag_p_nosw,
        where_var = where_var,
        data_dir,
        numCores,
        chunk_size
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
