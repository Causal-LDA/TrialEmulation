

# check setup is valid
validate_TE_setup <- function(object, data) {
  assert_class(object, "TE_setup")
}

#' @export
print.TE_setup <- function(x, ...) {
  print.TE_censoring(x$censoring)
  cat("\n")
}


#' @export
print.TE_censoring <- function(x, ...) {
  if (is.na(x$censored)) {
    cat("No informative censoring defined.\n")
  } else {
    cat_underline("Informative censoring model", 1)
    cat("Censoring variable:", dQuote(x$censored, FALSE), "\n")
    cat("Numerator model:", as.character(x$censor_n_model), "\n")
    cat("Denominator model:", as.character(x$censor_d_model), "\n")
    if (isTRUE(x$pooled)) {
      cat("Models pooled across treatment groups\n")
    } else {
      cat("Separate models for treatment groups\n")
    }
  }
}


#' Set up Trial Emulation
#'
#' @param data A `data.frame`
#' @param variables set the columns in the data frame using [te_variables()]
#' @param expansion set how the data are expanded into trials using [te_expansion()]
#' @param treatment_switching set how treatment switching weighted and censored using [te_switching()]
#' @param censoring set how informative censoring is weighted using [te_censoring()]
#' @param sampling set how case-control sampling is done for large datasets [te_sampling()]
#' @param outcome_model specify the outcome model using [te_outcome_model()]
#' @param ... other parameters saved to the object
#'
#' @export
trial_emulation_setup <- function(data,
                                  variables = te_variables(),
                                  expansion = te_expansion(),
                                  treatment_switching = te_switching(),
                                  censoring,
                                  sampling = te_sampling(),
                                  outcome_model = te_outcome_model(),
                                  ...) {
  if (missing(censoring)) censoring <- te_censoring(censored = NA)

  setup <- c(
    list(
      variables = variables,
      expansion = expansion,
      treatment_switching = treatment_switching,
      censoring = censoring,
      sampling = sampling,
      outcome_model = outcome_model
    ),
    list(...)
  )

  class(setup) <- "TE_setup"
  validate_TE_setup(setup, data)
  setup
}

#' Set Data Variables
#'
#' @param id column name of patient IDs
#' @param period column name of time period
#' @param outcome column name of outcome events
#' @param eligible column name of eligibility flag
#' @param treatment column name of treatment received in period
#' @param where_variables column names of variables which should be retained for sub-setting the final dataset
#'  for modelling.
#'
#' @export
te_variables <- function(id = "id",
                         period = "period",
                         eligible = "eligible",
                         treatment = "treatment",
                         outcome = "outcome",
                         where_variables = NA) {
  list(
    id = id,
    period = period,
    eligible = eligible,
    outcome = outcome,
    treatment = treatment,
    where_variables = where_variables
  )
}

#' Set up Trial Expansion
#'
#' @param first_period Exclude trials starting before this time
#' @param last_period Exclude trials starting after this time
#' @param separate_files Expand data into separate files for each trial
#' @param chunk_size number of patients to process in each chunk
#'
#'
#' @export
te_expansion <- function(first_period = NA,
                         last_period = NA,
                         separate_files = FALSE,
                         chunk_size = 500) {

}

#' Set Treatment Switching
#'
#' @param switch_n_model RHS formula for numerator model for calculating treatment switching weights.
#' @param switch_d_model RHS formula for denominator model for calculating stabilized treatment switching weights.
#' @param censor_switchers logical. Censor patients who switch treatments.
#' @param eligible_wts_0 name of column containing `0`/`1` flag for observations which cannot switch treatment
#'  (from `treatment = 0` to `treatment = 1`) and should be excluded from fitting the treatment switch model.
#' @param eligible_wts_1 name of column for excluding patients from model for switching from
#'  `treatment = 1` to `treatment = 0`.
#'
#' @export
te_switching <- function(switch_n_model = ~1,
                         switch_d_model = ~1,
                         censor_switchers = TRUE,
                         eligible_wts_0 = NA,
                         eligible_wts_1 = NA) {
  list(
    switch_n_model = switch_n_model,
    switch_d_model = switch_d_model,
    censor_switchers = censor_switchers,
    eligible_wts_0 = eligible_wts_0,
    eligible_wts_1 = eligible_wts_1
  )
}

#' Set up Informative Censoring
#'
#' @param censored column name with censoring flag
#' @param censor_d_model RHS formula denominator model for informative censoring
#' @param censor_n_model RHS formula numerator model for informative censoring
#' @param pool_models Pool the numerator and denominator models
#' (`FALSE`: split models by previous treatment as in treatment models and
#'  `TRUE`: pool all observations together into a single numerator and denominator model)
#'
#' @export
te_censoring <- function(censored = "censored",
                         censor_d_model = ~1,
                         censor_n_model = ~1,
                         pool_models = FALSE) {
  object <- list(
    censored = censored,
    censor_d_model = censor_d_model,
    censor_n_model = censor_n_model,
    pool_models = pool_models
  )
  class(object) <- "TE_censoring"
  object
}

#' Set up Data Sampling
#'
#' @param sample_trials FALSE
#' @param probability sampling probability for a control observation
#'
#' @export
te_sampling <- function(sample_trials = FALSE,
                        probability = .1) {
  list(
    sample_trials = FALSE,
    probability = probability
  )
}

#' Set up Outcome Model Fitting
#' @param treatment_model RHS model for specifying treatment in outcome model, typically `~assigned_treatment`
#' @param covariate_adjustment RHS formula for specifying the adjustment of baseline trial covariates
#' @param followup_time_adjustment RHS formula for specifying how the trial's follow up time is adjusted
#' in the outcome model.
#' @param expansion_time_adjustment RHS formula for specifying how the trial's start time is adjusted in
#' the outcome model.
#' @param where_case where_case
#' @param use_weight logical. Fit the logistic regression model with the censoring and sampling weights.
#' @param glm_function name of function to use for fitting logistic regression model,
#'  `"glm"` for [stats::glm] or `"parglm"` for [parglm::parglm].
#'
#' @export
te_outcome_model <- function(treatment_model = ~assigned_treatment,
                             covariate_adjustment = ~1,
                             followup_time_adjustment = ~ followup_time + I(followup_time^2),
                             expansion_time_adjustment = ~ for_period + I(for_period^2),
                             where_case = NULL,
                             use_weight = TRUE,
                             glm_function = "glm") {
  list(
    treatment_model = treatment_model,
    covariate_adjustment = covariate_adjustment,
    followup_time_adjustment = followup_time_adjustment,
    expansion_time_adjustment = expansion_time_adjustment,
    where_case = where_case,
    use_weight = use_weight,
    glm_function = glm_function
  )
}


# get set up for data preparation
get_data_setup <- function(setup) {
  assert_class(setup, "TE_setup")

  data_setup <- with(
    setup,
    list(
      id = variables$id,
      period = variables$period,
      treatment = variables$treatment,
      outcome = variables$outcome,
      eligible = variables$eligible,
      eligible_wts_0 = treatment_switching$eligible_wts_0,
      eligible_wts_1 = treatment_switching$eligible_wts_1,
      formula_vars = unlist(lapply(
        list(
          outcome_model$covariate_adjustment,
          treatment_switching$switch_n_model,
          treatment_switching$switch_d_model,
          censoring$censor_n_model,
          censoring$censor_d_model
        ),
        all.vars
      )),
      use_censor = isTRUE(treatment_switching$censor_switchers),
      cense = censoring$censored,
      where_var = variables$where_var
    )
  )

  class(data_setup) <- "TE_data_setup"
  data_setup
}
