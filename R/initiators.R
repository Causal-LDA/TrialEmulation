#' A wrapper function to perform data preparation and model fitting in a sequence of emulated target trials
#'
#' `r lifecycle::badge('stable')`
#'
#' An all-in-one analysis using a sequence of emulated target trials. This provides a simplified interface to the main
#' functions [`data_preparation()`] and [`trial_msm()`].
#'
#' @param data A `data.frame` containing all the required variables in the person-time format, i.e., the  `long' format.
#' @param id Name of the variable for identifiers of the individuals.  Default is `id'.
#' @param period Name of the variable for the visit/period.   Default is `period'.
#' @param treatment Name of the variable  for the treatment indicator at that visit/period. Default is `treatment'.
#' @param outcome Name of the variable for the indicator of the outcome event at that visit/period.  Default is
#'   `outcome'.
#' @param eligible Name of the variable for the indicator of eligibility for the target trial at that visit/period.
#'   Default is `eligible'.
#' @param outcome_cov A RHS formula with baseline covariates to be adjusted for in the marginal structural model for the
#'   emulated trials. Note that if a time-varying covariate is specified in `outcome_cov`, only its value at each of the
#'   trial baselines will be included in the expanded data.
#' @param model_var Treatment variables to be included in the marginal structural model for the emulated trials.
#'   `model_var = "assigned_treatment"` will create a variable `assigned_treatment` that is the assigned treatment at
#'   the trial baseline, typically used for ITT and per-protocol analyses. `model_var = "dose"` will create a variable
#'   `dose` that is the cumulative number of  treatments received since the trial baseline, typically used in as-treated
#'   analyses.
#'
#' @param first_period First time period to be set as trial baseline  to start expanding the data.
#' @param last_period Last time period to be set as trial baseline  to start expanding the data.
#' @param first_followup First follow-up time/visit in the trials to be included in the marginal structural model for
#'   the outcome event.
#' @param last_followup Last follow-up time/visit in the trials to be included in the marginal structural model for the
#'   outcome event.
#' @param save_weight_models Save model objects for estimating the weights in `data_dir`.
#' @param analysis_weights Choose which type of weights to be used for fitting the marginal structural model for the
#'   outcome event.
#'  * `"asis"`: use the weights as calculated.
#'  * `"p99"`: use weights truncated at the 1st and 99th percentiles (based on the distribution of weights
#'   in the entire sample).
#'  * `"weight_limits"`: use weights truncated at the values specified in `weight_limits`.
#'  * `"unweighted"`: set all analysis weights to 1, even if treatment weights or censoring weights were calculated.
#' @param weight_limits Lower and upper limits to truncate weights, given as `c(lower, upper)`
#' @param estimand_type Specify the estimand for the causal analyses in the sequence of emulated trials. `estimand_type
#'   = "ITT"` will perform intention-to-treat analyses, where treatment switching after trial baselines are ignored.
#'   `estimand_type = "PP"` will perform per-protocol analyses, where individuals' follow-ups are artificially censored
#'   and inverse probability of treatment weighting is applied. `estimand_type = "As-Treated"` will fit a standard
#'   marginal structural model for all possible treatment sequences, where individuals' follow-ups are not artificially
#'   censored  but treatment switching after trial baselines are accounted for by applying inverse probability of
#'   treatment weighting.
#' @param use_censor_weights Require the inverse probability of censoring weights. If `use_censor_weights = TRUE`, then
#'   the variable name of the censoring indicator needs to be provided in the argument `cense`.
#' @param cense Variable name for the censoring indicator. Required if `use_censor_weights = TRUE`.
#' @param pool_cense Fit pooled or separate censoring models for those treated and those untreated at the immediately
#'   previous visit. Pooling can be specified for the models for the numerator and denominator terms of the inverse
#'   probability of censoring weights. One of `"none"`, `"numerator"`, or `"both"` (default is `"none"` except when
#'   `estimand_type = "ITT"` then default is `"numerator"`).
#' @param include_followup_time The model to include the follow up time/visit of the trial (`followup_time`) in the
#'   marginal structural model, specified as a RHS formula.
#' @param include_trial_period The model to include the trial period (`trial_period`) in the marginal structural model,
#'   specified as a RHS formula.
#' @param eligible_wts_1 Exclude some observations when fitting the models for the inverse probability of treatment
#'   weights. For example, if it is assumed that an individual will stay on treatment for at least 2 visits, the first 2
#'   visits  after treatment initiation by definition have a probability of staying on the treatment of 1.0 and should
#'   thus be excluded from the weight models for those who are on treatment at the immediately previous visit. Users can
#'   define a variable that indicates that these 2 observations are ineligible for the weight model for those who are on
#'   treatment at the immediately previous visit and add the variable name in the argument `eligible_wts_1`. Similar
#'   definitions are applied to `eligible_wts_0` for excluding observations when fitting the models for the inverse
#'   probability of treatment weights for those who are not on treatment at the immediately previous visit.
#' @param eligible_wts_0 See definition for `eligible_wts_1`
#' @param where_var Specify the variable names that will be used to define subgroup conditions when fitting the marginal
#'   structural model for a subgroup of individuals. Need to specify jointly with the argument `where_case`.
#' @param where_case Define conditions using variables specified in `where_var` when fitting a marginal structural model
#'   for a subgroup of the individuals. For example, if `where_var= "age"`, `where_case = "age >= 30"` will only fit the
#'   marginal structural model to the subgroup of individuals. who are 30 years old or above.
#' @param data_dir Directory to save model objects in.
#' @param glm_function Specify which glm function to use for the marginal structural model from the `stats` or `parglm`
#'   packages. The default function is the `glm` function in the `stats` package. Users can also specify `glm_function =
#'   "parglm"` such that the `parglm` function in the `parglm` package can be used for fitting generalized linear models
#'   in parallel. The default control setting for  `parglm` is `nthreads = 4` and `method = "FAST"`, where four cores
#'   and Fisher information are used for faster computation. Users can change the default control setting by passing the
#'   arguments `nthreads` and `method` in the `parglm.control` function of the `parglm` package, or alternatively, by
#'   passing a `control` argument with a list produced by `parglm.control(nthreads = , method = )`.
#' @param quiet Suppress the printing of progress messages and summaries of the fitted models.
#' @param switch_n_cov A RHS formula to specify the logistic models for estimating the numerator terms of the inverse
#'   probability of treatment weights. A derived variable named `time_on_regime` containing the duration of time that
#'   the individual has been on the current treatment/non-treatment is available for use in these models.
#' @param switch_d_cov A RHS formula to specify the logistic models for estimating the denominator terms of the inverse
#'   probability of treatment weights.
#' @param cense_d_cov A RHS formula to specify the logistic models for estimating the denominator terms of the inverse
#'   probability of censoring weights.
#' @param cense_n_cov A RHS formula to specify the logistic models for estimating the numerator terms of the inverse
#'   probability of censoring weights.
#' @param ... Additional arguments passed to `glm_function`. This may be used to specify initial values of parameters or
#'   arguments to `control`. See [stats::glm], [parglm::parglm] and [parglm::parglm.control()] for more information.
#'
#'
#' @returns Returns the result of [trial_msm()] from the expanded data. An object of class `TE_msm` containing
#' \describe{
#'  \item{model}{a `glm` object}
#'  \item{robust}{a list containing a summary table of estimated regression coefficients and the robust covariance
#'  matrix}
#' }
#' @export
initiators <- function(data,
                       id = "id",
                       period = "period",
                       treatment = "treatment",
                       outcome = "outcome",
                       eligible = "eligible",
                       outcome_cov = ~1,
                       estimand_type = c("ITT", "PP", "As-Treated"),
                       model_var = NULL,
                       switch_n_cov = ~1,
                       switch_d_cov = ~1,
                       first_period = NA,
                       last_period = NA,
                       first_followup = NA,
                       last_followup = NA,
                       use_censor_weights = FALSE,
                       save_weight_models = FALSE,
                       analysis_weights = c("asis", "unweighted", "p99", "weight_limits"),
                       weight_limits = c(0, Inf),
                       cense = NA,
                       pool_cense = c("none", "both", "numerator"),
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
    estimand_type = estimand_type,
    model_var = model_var,
    switch_n_cov = switch_n_cov,
    switch_d_cov = switch_d_cov,
    first_period = first_period,
    last_period = last_period,
    use_censor_weights = use_censor_weights,
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
  model_full <- trial_msm(
    data = prep_result$data,
    outcome_cov = outcome_cov,
    estimand_type = estimand_type,
    model_var = model_var,
    first_followup = first_followup,
    last_followup = last_followup,
    analysis_weights = analysis_weights,
    weight_limits = weight_limits,
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
