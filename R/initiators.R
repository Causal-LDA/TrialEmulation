#' Initiators Analysis
#'
#' An all-in-one analysis using a sequence of target trials. This provides a simplified
#' interface to the main working functions [`data_preparation()`] and [`pooled_trial_lr()`].
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
#'   and `dose + dose^2` for as-treated.
#'
#' @param first_period First time period to include as trial baseline in expanded data
#' @param last_period Last time period to include as trial baseline in expanded data
#' @param first_followup First follow-up period
#' @param last_followup Last follow-up period
#' @param use_weight Use weights in analysis. If `FALSE` then no weights will be calculated.
#' @param save_weight_models Save weight models objects in `data_dir`.
#' @param analysis_weights One of
#'  * `"asis"`: use the weights as calculated
#'  * `"p99"`: truncate weights at the 1st and 99th percentiles
#'  * `"weight_limits"`: truncate weights at the values specified in `weight_limits`
#'  * `"unweighted"`: set all analysis weights to 1, even with `use_weight = TRUE`
#' @param weight_limits Lower and upper limits to truncate weights, given as `c(lower, upper)`
#' @param use_censor Use censoring for per-protocol analysis - censor person-times once a person-trial stops taking the
#'  initial treatment value
#' @param cense Censoring variable
#' @param pool_cense Fit pooled or separate censoring models for those treated and
#' those untreated at the immediately previous visit.
#' (default is `FALSE`, separate numerator and denominator models for treatment groups)
#' @param include_followup_time The model to include the follow up time of the trial (`followup_time`) in outcome model,
#'  specified as a RHS formula.
#' @param include_trial_period The model to include the trial period (`trial_period`) in outcome model,
#'  specified as a RHS formula.
#' @param eligible_wts_0 Eligibility criteria used in weights for model condition Am1 = 0
#' @param eligible_wts_1 Eligibility criteria used in weights for model condition Am1 = 1
#' @param where_var List of variables used in where conditions used in subsetting the data used in final analysis
#' (where_case), the variables not included in the final model
#' @param where_case List of where conditions used in subsetting the data used in final analysis
#' @param data_dir Directory to save model objects in.
#' @param glm_function Which glm function to use for the final model from `stats` or `parglm` packages
#' @param quiet Don't print progress messages.
#' @param switch_n_cov A RHS formula for modelling probability of switching treatment. Used in the numerator of weight
#' calculation. May use `time_on_regime` to include treatment duration.
#' @param switch_d_cov A RHS formula for modelling probability of switching treatment. Used in the denominator of weight
#' calculation. May use `time_on_regime` to include treatment duration.
#' @param cense_d_cov A RHS formula for modelling probability of being censored. Used in the numerator of weight
#' calculation.
#' @param cense_n_cov A RHS formula for modelling probability of being censored. Used in the denominator of weight
#' calculation.
#' @param ... Additional arguments passed to `glm_function`. This may be used to specify initial parameter estimates
#' or arguments to `control`. See [stats::glm], [parglm::parglm] and [parglm::parglm.control()] for more information.
#'
#' @details
#' If `model_var = NULL` the package will add some terms to the outcome model:
#'
#'  * if `use_censor = FALSE` and `use_weight = FALSE`, an as-treated analysis will be done the outcome model will have
#'  `~ dose + I(dose^2)` terms added
#'  * if `use_censor = TRUE`, a per-protocol analysis will be done with an `~assigned_treatment` term added
#'  * if `use_censor = FALSE` and `use_weight = TRUE`, an intention to treat analysis will be done with an
#'   `~assigned_treatment` term added
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
                       use_weight = FALSE,
                       save_weight_models = FALSE,
                       analysis_weights = c("asis", "unweighted", "p99", "weight_limits"),
                       weight_limits = c(0, Inf),
                       use_censor = FALSE,
                       cense = NA,
                       pool_cense = FALSE,
                       cense_d_cov = ~1,
                       cense_n_cov = ~1,
                       include_followup_time = ~ followup_time + I(followup_time^2),
                       include_trial_period = ~ trial_period + I(trial_period^2),
                       eligible_wts_0 = NA,
                       eligible_wts_1 = NA,
                       where_var = NULL,
                       where_case = NA,
                       data_dir,
                       glm_function = "glm",
                       quiet = FALSE,
                       ...) {
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
    cense = cense,
    pool_cense = pool_cense,
    cense_d_cov = cense_d_cov,
    cense_n_cov = cense_n_cov,
    eligible_wts_0 = eligible_wts_0,
    eligible_wts_1 = eligible_wts_1,
    where_var = where_var,
    data_dir = data_dir,
    save_weight_models = save_weight_models,
    separate_files = FALSE,
    quiet = quiet,
    ...
  )

  # Fit final models and robust variance estimates
  model_full <- pooled_trial_lr(
    data = prep_result$data,
    outcome_cov = outcome_cov,
    model_var = model_var,
    first_followup = first_followup,
    last_followup = last_followup,
    use_weight = use_weight,
    analysis_weights = analysis_weights,
    weight_limits = weight_limits,
    use_censor = use_censor,
    include_followup_time = include_followup_time,
    include_trial_period = include_trial_period,
    where_case = where_case,
    glm_function = "glm",
    use_sample_weights = FALSE,
    quiet = quiet,
    ...
  )

  model_full
}
