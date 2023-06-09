

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
#' @param treatment_switching set how treatment switching is handles
#' @param censoring
#' @param sampling
#' @param outcome_model
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
    treatment = treatment
  )
}

#' Set up Trial Expansion
#'
#' @param first_period
#' @param last_period
#'
#' @return
#' @export
#'
#' @examples
te_expansion <- function(first_period = NA,
                         last_period = NA) {

}

#' Set Treatment Switching
#'
#' @param switch_n_model
#' @param switch_d_model
#' @param censor_switchers
#' @param eligible_wts_0
#' @param eligible_wts_1
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
#' @param outcome
#' @param covariate_adjustment
#' @param followup_time_adjustment
#' @param expansion_time_adjustment
#' @param where_variables
#' @param use_weight
#' @param glm_function
#'
#' @return
#' @export
#'
#' @examples
te_outcome_model <- function(outcome = assigned_treatment ~ .,
                             covariate_adjustment = ~1,
                             followup_time_adjustment = ~ followup_time + I(followup_time^2),
                             expansion_time_adjustment = ~ for_period + I(for_period^2),
                             where_variables,
                             use_weight = TRUE,
                             glm_function = "glm") {

}
