#' Initiators Analysis
#'
#' An all-in-one analysis using a sequence of target trials. This provides a simplified
#' interface to the main working functions [`data_preparation()`] and [`data_modelling()`].
#'
#' @param data A `data.frame` containing all the required columns.
#' @param id Name of the data column for id feature Defaults to id
#' @param period Name of the data column for period feature Defaults to period
#' @param treatment Name of the data column for treatment feature Defaults to treatment
#' @param outcome Name of the data column for outcome feature Defaults to outcome
#' @param eligible Indicator of whether or not an observation is eligible to be expanded about Defaults to eligible
#' @param outcome_cov A RHS formula with baseline covariates to adjust in final model
#' @param model_var List of Variables of interest to be used in final model.
#'   Derived variables to use in outcome models. Typically `assigned_treatment` for ITT and per-protocol,
#'   and `dose + dose^2` for as-treated. `time_on_regime`? TODO check what else is derived
#'
#' @param first_period First period value to start expanding about
#' @param last_period Last period value to expand about
#' @param first_followup First follow-up period
#' @param last_followup Last follow-up period
#' @param use_weight Use weights in analysis. If 0 then no weights will be calculated
#' @param run_unweighted_analysis Run the final model with no weights when use_weights = 1
#' @param run_weighted_analysis Run the final model with original weights
#' @param run_p99_analysis Run the final model with truncating the weights at the 1st and 99th percentile
#' @param run_user_limits_analysis Run the final model with truncating the weights using user defined limits
#' @param lower_weight Use lower weight as minimum possible weight
#' @param upper_weight Use upper weight as maximum possible weight
#' @param use_censor Use censoring for per-protocol analysis - censor person-times once a person-trial stops taking the
#'  initial treatment value
#' @param check_missing Check for missing values in final model when use_censor=1 (Not added yet!)
#' @param cense Censoring variable
#' @param pool_cense Pool the numerator and denominator models (0: split models by previous treatment Am1 = 0 and
#' Am1 = 1 as in treatment models and 1: pool all observations together into a single numerator and denominator model)
#'  Defaults to 0
#' @param include_followup_time_case The model to include follow up time in outcome model.
#' This has 3 options c("linear","quadratic","spline")
#' @param include_expansion_time_case The model to include for_period in outcome model.
#'  This has 3 options c("linear","quadratic","spline")
#' @param include_regime_length If defined as 1 a new variable (time_on_regime) is added to dataset.
#'  This variable stores the duration of time that the patient has been on the current treatment value
#' @param eligible_wts_0 Eligibility criteria used in weights for model condition Am1 = 0
#' @param eligible_wts_1 Eligibility criteria used in weights for model condition Am1 = 1
#' @param lag_p_nosw When 1 this will set the first weight to be 1 and use p_nosw_d and p_nosw_n at followup-time (t-1)
#'  for calculating the weights at followup-time t - can be set to 0 which will increase the maximum and variance of
#'  weights (Defaults to 1)
#' @param where_var List of variables used in where conditions used in subsetting the data used in final analysis
#' (where_case), the variables not included in the final model
#' @param where_case List of where conditions used in subsetting the data used in final analysis
#' @param run_base_model Run the model with no conditions Defaults to 1
#' @param data_dir Direction to save data
#' @param numCores Number of cores for parallel programming (default value is maximum cores and parallel programming)
#' @param glm_function Which glm function to use for the final model from `stats` or `parglm` packages
#' @param quiet Don't print progress messages.
#' @param switch_n_cov A RHS formula for modelling probability of switching treatment. Used in the numerator of weight
#' calculation.
#' @param switch_d_cov A RHS formula for modelling probability of switching treatment. Used in the denominator of weight
#' calculation.
#' @param cense_d_cov A RHS formula for modelling probability of being censored. Used in the numerator of weight
#' calculation.
#' @param cense_n_cov A RHS formula for modelling probability of being censored. Used in the denominator of weight
#' calculation.
#'
#' @export
initiators <- function(data,
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
                       first_followup = NA,
                       last_followup = NA,
                       use_weight = 0,
                       run_unweighted_analysis = 0,
                       run_weighted_analysis = 1,
                       run_p99_analysis = 0,
                       run_user_limits_analysis = 0,
                       lower_weight = NA,
                       upper_weight = NA,
                       use_censor = 0,
                       check_missing = 0,
                       cense = NA,
                       pool_cense = 0,
                       cense_d_cov = ~1,
                       cense_n_cov = ~1,
                       include_followup_time_case = ~ followup_time + I(followup_time^2),
                       include_expansion_time_case = ~ for_period + I(for_period^2),
                       include_regime_length = 0,
                       eligible_wts_0 = NA,
                       eligible_wts_1 = NA,
                       lag_p_nosw = 1,
                       where_var = NULL,
                       where_case = NA,
                       run_base_model = 1,
                       data_dir,
                       numCores = NA,
                       glm_function = "glm",
                       quiet = FALSE) {
  # Check parameters
  if (!missing(data_dir)) {
    if (!dir.exists(data_dir)) stop(paste0("Specified data_dir does not exist: ", data_dir))
  }

  # Prepare variables, calculate weights and expand data
  prep_result <- data_preparation(
    data = data,
    id = id,
    period = period,
    treatment = treatment,
    outcome = outcome,
    eligible = eligible,
    outcome_cov = outcome_cov,
    model_var = model_var,
    switch_n_cov = switch_n_cov,
    switch_d_cov = switch_d_cov,
    first_period = first_period,
    last_period = last_period,
    use_weight = use_weight,
    use_censor = use_censor,
    check_missing = check_missing,
    cense = cense,
    pool_cense = pool_cense,
    cense_d_cov = cense_d_cov,
    cense_n_cov = cense_n_cov,
    include_followup_time_case = include_followup_time_case,
    include_expansion_time_case = include_expansion_time_case,
    include_regime_length = include_regime_length,
    eligible_wts_0 = eligible_wts_0,
    eligible_wts_1 = eligible_wts_1,
    lag_p_nosw = lag_p_nosw,
    where_var = where_var,
    data_dir = data_dir,
    numCores = numCores,
    separate_files = FALSE,
    quiet = quiet
  )

  # Fit final models and robust variance estimates
  model_full <- data_modelling(
    data = prep_result$data,
    outcome_cov = outcome_cov,
    model_var = model_var,
    first_followup = first_followup,
    last_followup = last_followup,
    use_weight = use_weight,
    run_unweighted_analysis = run_unweighted_analysis,
    run_weighted_analysis = run_weighted_analysis,
    run_p99_analysis = run_p99_analysis,
    run_user_limits_analysis = run_user_limits_analysis,
    lower_weight = lower_weight,
    upper_weight = upper_weight,
    use_censor = use_censor,
    check_missing = check_missing,
    include_followup_time_case = include_followup_time_case,
    include_expansion_time_case = include_expansion_time_case,
    where_case = where_case,
    run_base_model = run_base_model,
    numCores = numCores,
    glm_function = "glm",
    use_sample_weights = FALSE,
    quiet = quiet
  )

  model_full
}
