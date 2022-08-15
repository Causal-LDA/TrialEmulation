#' INITIATORS Function
#'
#' This function analyses observational longitudinal data to estimate the effect of interventions sustained over time. This emulates the design of a hypothetical randomized trial.
#' @param data_path The path to csv file
#' @param id Name of the data column for id feature Defaults to id
#' @param period Name of the data column for period feature Defaults to period
#' @param treatment Name of the data column for treatment feature Defaults to treatment
#' @param outcome Name of the data column for outcome feature Defaults to outcome
#' @param eligible Indicator of whether or not an observation is eligible to be expanded about Defaults to eligible
#' @param outcomeCov_var List of individual baseline variables used in final model
#' @param outcomeCov List of functions of baseline covariates used in final model
#' @param outcomeClass Categorical variables used in the final model
#' @param model_var List of Variables of interest to be used in final model
#' @param cov_switchn List of covariates to be used in logistic model for switching probabilities for numerator model
#' @param model_switchn List of models (functions) to use the covariates from cov_switchn
#' @param class_switchn Class variables used in logistic model for nominator model
#' @param cov_switchd List of covariates to be used in logistic model for switching probabilities for denominator model
#' @param model_switchd List of models (functions) to use the covariates from cov_switchd
#' @param class_switchd Class variables used in logistic model for denominator model
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
#' @param use_censor Use censoring for per-protocol analysis - censor person-times once a person-trial stops taking the initial treatment value
#' @param check_missing Check for missing values in final model when use_censor=1 (Not added yet!)
#' @param cense Censoring variable
#' @param pool_cense Pool the numerator and denominator models (0: split models by previous treatment Am1 = 0 and Am1 = 1 as in treatment models and 1: pool all observations together into a single numerator and denominator model) Defaults to 0
#' @param cov_censed List of covariates to be used in logistic model for censoring weights in denominator model
#' @param model_censed List of models (functions) to use the covariates from cov_censed
#' @param class_censed Class variables used in censoring logistic regression in denominator model
#' @param cov_censen List of covariates to be used in logistic model for censoring weights in numerator model
#' @param model_censen List of models (functions) to use the covariates from cov_censen
#' @param class_censen Class variables used in censoring logistic regression in numerator model
#' @param include_followup_time_case The model to include follow up time in outcome model. This has 3 options c("linear","quadratic","spline")
#' @param include_expansion_time_case The model to include for_period in outcome model. This has 3 options c("linear","quadratic","spline")
#' @param followup_spline The parameters for spline model for followup time when choose "spline" in the include_followup_time_case (ex. list(df=2))
#' @param period_spline The parameters for spline model for for_period when choose "spline" in the include_expansion_time_case (ex. list(df=3))
#' @param include_regime_length If defined as 1 a new variable (time_on_regime) is added to dataset - This variable stores the duration of time that the patient has been on the current treatment value
#' @param eligible_wts_0 Eligibility criteria used in weights for model condition Am1 = 0
#' @param eligible_wts_1 Eligibility criteria used in weights for model condition Am1 = 1
#' @param lag_p_nosw When 1 this will set the first weight to be 1 and use p_nosw_d and p_nosw_n at followup-time (t-1) for calculating the weights at followup-time t - can be set to 0 which will increase the maximum and variance of weights (Defaults to 1)
#' @param where_var List of variables used in where conditions used in subsetting the data used in final analysis (where_case), the variables not included in the final model
#' @param where_case List of where conditions used in subsetting the data used in final analysis
#' @param run_base_model Run the model with no conditions Defaults to 1
#' @param case_control Run the case control sampling or not Defaults to 0
#' @param n_control Number of controls used in case control sampling. Not compatible with `p_control`. If `p_control` is specified, `n_control` has no effect.
#' @param p_control Proportion of control to sample. If `p_control` is specified, `n_control` has no effect.
#' @param sample_all_periods Sample a proportion of controls even if there are no cases in that period. Useful when estimating the baseline hazard.
#' @param data_dir Direction to save data
#' @param numCores Number of cores for parallel programming (default value is maximum cores and parallel programming)
#' @param chunk_expansion Do the expansion in chunks (and in parallel if numCores > 1). Turn this off if you have enough memory to expand the whole dataset at once. (default TRUE)
#' @param chunk_size Number of ids to process at once for the chunk expansion (default 500). Larger chunk_sizes may be faster but require more memory.
#' @param separate_files Write to one file or one per trial (default FALSE)
#' @param glm_function Which glm function to use for the final model from `stats` or `parglm` packages
#' @param quiet Don't print progress messages.
#'
#' @details The class variables paramers (`outcomeClass`,`class_switchn`,`class_switchd`,`class_censen`,`class_censed`)
#' can be given as a character vector which will construct factors using `as.factor` or as a named list with the arguments for factor
#' eg `list(risk_cat=list(levels = c(1,2,3,0), age_cat=list(levels=c(1,2,3),labels=c("50-60","60-70","70+")`
#'
#' initiators()
#' @export
#' @import foreach
#' @import doParallel
#' @import parallel
#' @import data.table
#' @importFrom Rcpp sourceCpp
#' @useDynLib RandomisedTrialsEmulation


initiators <- function(data_path,
                       id = "id",
                       period = "period",
                       treatment = "treatment",
                       outcome = "outcome",
                       eligible = "eligible",
                       outcomeCov_var = NA,
                       outcomeCov = NA,
                       outcomeClass = NA,
                       model_var = NA,
                       cov_switchn = NA,
                       model_switchn = NA,
                       class_switchn = NA,
                       cov_switchd = NA,
                       model_switchd = NA,
                       class_switchd = NA,
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
                       cov_censed = NA,
                       model_censed = NA,
                       class_censed = NA,
                       cov_censen = NA,
                       model_censen = NA,
                       class_censen = NA,
                       include_followup_time_case = "linear",
                       include_expansion_time_case = "linear",
                       followup_spline = NA,
                       period_spline = NA,
                       include_regime_length = 0,
                       eligible_wts_0 = NA,
                       eligible_wts_1 = NA,
                       lag_p_nosw = 1,
                       where_var = NA,
                       where_case = NA,
                       run_base_model = 1,
                       case_control = 0,
                       n_control,
                       p_control,
                       sample_all_periods = FALSE,
                       data_dir,
                       numCores = NA,
                       chunk_expansion = TRUE,
                       chunk_size = 500,
                       separate_files = FALSE,
                       glm_function = "parglm",
                       quiet = FALSE) {


  # Check parameters
  if (isTRUE(separate_files) & case_control != 1) stop("Separate trial files without case-control sampling is not possible with initiators()")
  if (!dir.exists(data_dir)) stop(paste0("Specified data_dir does not exist: ", data_dir))
  if (!file.exists(data_path)) stop(paste0("Specified data_path does not exist: ", data_path))

  assert_flag(quiet)

  # Prepare variables, calculate weights and expand data
  prep_result <- data_preparation(
    data_path = data_path,
    id = id,
    period = period,
    treatment = treatment,
    outcome = outcome,
    eligible = eligible,
    outcomeCov_var = outcomeCov_var,
    outcomeCov = outcomeCov,
    outcomeClass = outcomeClass,
    model_var = model_var,
    cov_switchn = cov_switchn,
    model_switchn = model_switchn,
    class_switchn = class_switchn,
    cov_switchd = cov_switchd,
    model_switchd = model_switchd,
    class_switchd = class_switchd,
    first_period = first_period,
    last_period = last_period,
    use_weight = use_weight,
    use_censor = use_censor,
    check_missing = check_missing,
    cense = cense,
    pool_cense = pool_cense,
    cov_censed = cov_censed,
    model_censed = model_censed,
    class_censed = class_censed,
    cov_censen = cov_censen,
    model_censen = model_censen,
    class_censen = class_censen,
    include_followup_time_case = include_followup_time_case,
    include_expansion_time_case = include_expansion_time_case,
    followup_spline = followup_spline,
    period_spline = period_spline,
    include_regime_length = include_regime_length,
    eligible_wts_0 = eligible_wts_0,
    eligible_wts_1 = eligible_wts_1,
    lag_p_nosw = lag_p_nosw,
    where_var = where_var,
    data_dir = data_dir,
    numCores = numCores,
    chunk_expansion = chunk_expansion,
    chunk_size = chunk_size,
    separate_files = separate_files,
    quiet = quiet
  )

  analysis_data_path <- prep_result$absolutePath

  # Sampling contols ----------
  if (prep_result$N >= 2^31 -1) {
    warning("Expanded data is too large for model fitting. Case-control sampling will be applied.")
    case_control <- 1
  }

  if (case_control == 1) {
    if (file.exists(file.path(data_dir, "temp_data.csv"))) file.remove(file.path(data_dir, "temp_data.csv"))

    if (isFALSE(separate_files)) {
      if (missing(p_control) & !missing(n_control)) {
        # sample using fixed n cases:controls ratio
        sample_data_path <- case_control_sampling(
          data_dir = data_dir,
          samples_file = "temp_data.csv",
          min_period = prep_result$min_period,
          max_period = prep_result$max_period,
          sample_all_periods = sample_all_periods,
          n_control = n_control,
          numCores=numCores)
      } else {
        # set p_control if missing
        if (missing(p_control)) p_control <- 0.01
        # sample a proportion of controls
        sample_data_path <- case_control_sampling(
          data_dir = data_dir,
          samples_file = "temp_data.csv",
          min_period = prep_result$min_period,
          max_period = prep_result$max_period,
          p_control = p_control,
          sample_all_periods = sample_all_periods,
          numCores=numCores)
      }
    } else if (isTRUE(separate_files)) {
      sample_data_path <- case_control_sampling_trials(
        data_dir = data_dir,
        n_control = n_control,
        numCores = numCores,
        samples_file = "temp_data.csv",
        infile_pattern = "trial_",
        sample_all_periods = sample_all_periods
      )
    }
    analysis_data_path <- normalizePath(sample_data_path)
    use_sample_weights <- TRUE
  } else {
    use_sample_weights <- FALSE
  }

  # Splines ------
  # Add splines to data if requested
  spline_object <- add_splines(
    data_path = analysis_data_path,
    period_spline = period_spline,
    followup_spline = followup_spline
  )

  # Fit final models and robust variance estimates
  model_full <- data_modelling(
    outcomeCov_var = outcomeCov_var,
    outcomeCov = outcomeCov,
    outcomeClass = outcomeClass,
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
    absolutePath = analysis_data_path,
    numCores = numCores,
    glm_function = glm_function,
    use_sample_weights = use_sample_weights,
    quiet = quiet
  )

  data_files <- list(
    raw = data_path,
    analysis = analysis_data_path
  )

  data_prep <- list(
    splines = spline_object,
    outcomeClass = outcomeClass,
    class_switchn = class_switchn,
    class_switchd = class_switchd,
    class_censen = class_censen,
    class_censed = class_censed
    )

  specification <- list(
    data_files = data_files,
    data_prep = data_prep
  )

  return(c(
    model_full,
    specification = specification
  ))
 }
