

check_setup <- function(setup) {
  # check setup is valid
}


get_data_setup <- function(setup) {
  setup$data
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
#' @param ...
#'
#' @export
trial_emulation_setup <- function(data,
                                  variables = te_variables(),
                                  expansion = te_expansion(),
                                  treatment_switching = te_switching(),
                                  censoring = te_censoring(),
                                  sampling = te_sampling(),
                                  outcome_model = te_outcome_model(),
                                  ...) {

}

#' Set Data Variables
#'
#' @param id column name of patient IDs
#' @param period column name of time period
#' @param outcome column name of outcome events
#' @param eligible column name of eligibility flag
#' @param treatment column name of treatment received in period
#' @param where_variables column names of variables which should be retained for subsetting the final dataset
#'  for modelling.
#'
#' @export
te_variables <- function(id = "id",
                         period = "period",
                         eligible = "eligible",
                         treatment = "treatment",
                         outcome = "outcome") {
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

}

#' Set up Data Sampling
#'
#' @param sample_trials FALSE
#'
#' @export
te_sampling <- function(sample_trials = FALSE,
                        proportion) {

}

#' Set up Outcome Model Fitting
#'
#' @param treatment_model RHS formula specifying how treatment is incorporated in the outcome model
#' @param covariate_adjustment RHS formula for specifying the adjustment of baseline trial covariates
#' @param followup_time_adjustment RHS formula for specifying how the trial's follow up time is adjusted
#' in the outcome model.
#' @param expansion_time_adjustment RHS formula for specifying how the trial's start time is adjusted in
#' the outcome model.
#' @param where_case where_case
#' @param use_weight logical. Fit the logistic regression model with the censoring and sampling weights.
#' @param glm_function name of function to use for fitting glm. `"glm"` for `stats::glm` or `"parglm"` for
#' `parglm::parglm`
#'
#' @export
te_outcome_model <- function(outcome = ~assigned_treatment,
                             covariate_adjustment = ~1,
                             followup_time_adjustment = ~ followup_time + I(followup_time^2),
                             expansion_time_adjustment = ~ for_period + I(for_period^2),
                             where_case,
                             use_weight = TRUE,
                             glm_function = "glm") {

}
