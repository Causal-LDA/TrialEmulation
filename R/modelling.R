#' @include trial_sequence.R generics.R
NULL

#' Fit the marginal structural model for the sequence of emulated trials
#'
#' `r lifecycle::badge('stable')`
#'
#' Apply a weighted pooled logistic regression to fit the marginal structural model for the sequence of emulated trials
#' and calculates the robust covariance matrix  of parameter using the sandwich estimator.
#'
#' @param use_sample_weights Use case-control sampling weights in addition to inverse probability weights for treatment
#'   and censoring. `data` must contain a column `sample_weight`. The final weights used in the pooled logistic
#'   regression are calculated as `weight = weight * sample_weight`.
#' @inheritParams initiators
#'
#' @details The model formula is constructed by combining the arguments `outcome_cov`, `model_var`,
#' `include_followup_time`, and `include_trial_period`.
#'
#' @returns Object of class `TE_msm` containing
#' \describe{
#'  \item{model}{a `glm` object}
#'  \item{robust}{a list containing a summary table of estimated regression coefficients and the robust covariance
#'  matrix}
#'  \item{args}{a list contain the parameters used to prepare and fit the model}
#' }
#'
#' @export
#' @importFrom stats as.formula binomial pnorm quantile relevel
#' @importFrom utils write.csv

trial_msm <- function(data,
                      outcome_cov = ~1,
                      estimand_type = c("ITT", "PP", "As-Treated"),
                      model_var = NULL,
                      first_followup = NA,
                      last_followup = NA,
                      analysis_weights = c("asis", "unweighted", "p99", "weight_limits"),
                      weight_limits = c(0, Inf),
                      include_followup_time = ~ followup_time + I(followup_time^2),
                      include_trial_period = ~ trial_period + I(trial_period^2),
                      where_case = NA,
                      glm_function = c("glm", "parglm"),
                      use_sample_weights = TRUE,
                      quiet = FALSE,
                      ...) {
  if (inherits(data, "TE_data_prep_dt")) data <- data$data

  arg_checks <- makeAssertCollection()
  assert_data_frame(data, add = arg_checks)
  outcome_cov <- as_formula(outcome_cov, add = arg_checks)
  estimand_type <- assert_choice(estimand_type[1], choices = c("ITT", "PP", "As-Treated"), add = arg_checks)
  assert_multi_class(model_var, classes = c("formula", "character"), null.ok = TRUE, add = arg_checks)
  assert_integerish(first_followup, lower = 0, all.missing = TRUE, len = 1, add = arg_checks)
  assert_integerish(last_followup, lower = 0, all.missing = TRUE, len = 1, add = arg_checks)
  analysis_weights <-
    assert_choice(analysis_weights[1], choices = c("asis", "unweighted", "p99", "weight_limits"), add = arg_checks)
  assert_numeric(weight_limits, len = 2, lower = 0, upper = Inf, sorted = TRUE, add = arg_checks)
  include_followup_time <- as_formula(include_followup_time, add = arg_checks)
  include_trial_period <- as_formula(include_trial_period, add = arg_checks)
  assert_multi_class(include_trial_period, classes = c("formula", "character"), add = arg_checks)
  assert_character(where_case, add = arg_checks)
  glm_function <- assert_choice(glm_function[1], choices = c("glm", "parglm"), add = arg_checks)
  assert_flag(use_sample_weights, add = arg_checks)
  assert_flag(quiet, add = arg_checks)
  reportAssertions(arg_checks)

  if ("use_weight" %in% ...names()) {
    stop("Argument `use_weight` is no longer supported. Use `analysis_weights` to control weighting behaviour.")
  }

  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  weight <- sample_weight <- followup_time <- NULL

  data <- as.data.table(data)
  # if there are any limits on the follow up
  if (!is.na(first_followup) || !is.na(last_followup)) {
    data <- data[
      followup_time >= max(0, first_followup, na.rm = TRUE) & followup_time <= min(Inf, last_followup, na.rm = TRUE),
    ]
  }

  quiet_msg(quiet, "Preparing for model fitting")


  if (estimand_type == "ITT") {
    if (is.null(model_var)) {
      model_var <- ~assigned_treatment
    }
  } else if (estimand_type == "PP") {
    if (is.null(model_var)) {
      model_var <- ~assigned_treatment
    }
  } else if (estimand_type == "As-Treated") {
    if (is.null(model_var)) {
      model_var <- ~ dose + I(dose^2)
    }
  }
  model_var <- as_formula(model_var)


  model_formula <- outcome ~ 1
  model_formula <- Reduce(
    add_rhs,
    c(model_formula, model_var, include_trial_period, include_followup_time, outcome_cov)
  )

  if (any(!is.na(where_case))) {
    quiet_msg(quiet, paste("Subsetting data with", toString(where_case), sep = " "))
    for (i in seq_along(where_case)) {
      data <- data[eval(parse(text = where_case[i])), ]
    }
  }

  # adjust weights if necessary
  w <- if (!"weight" %in% colnames(data)) rep(1, nrow(data)) else data[["weight"]]

  if (use_sample_weights) {
    if (!"sample_weight" %in% colnames(data)) {
      warning("'sample_weight' column not found in data. Using sample weights = 1.")
    } else {
      w <- w * data[["sample_weight"]]
    }
  }

  if (analysis_weights == "asis") {
    # nothing to do
  } else if (analysis_weights == "p99") {
    w <- p99_weight(w)
  } else if (analysis_weights == "weight_limits") {
    w <- limit_weight(w, weight_limits[1], weight_limits[2])
  } else if (analysis_weights == "unweighted") {
    w <- rep(1, nrow(data))
  }

  if (!test_data_table(data, any.missing = FALSE)) {
    warning(
      "Data frame for outcome model contains missing data. Doing complete-case analysis.",
      "See ?glm for `na.action` options."
    )
  }

  quiet_line(quiet)
  quiet_msg(quiet, "Fitting outcome model")
  model.full <- fit_glm(
    glm_function = glm_function,
    formula = model_formula,
    data = data,
    weights = w,
    ...
  )

  quiet_print(quiet, summary(model.full))
  quiet_line(quiet)

  quiet_msg(quiet, "Calculating robust variance")
  robust_model <- robust_calculation(model.full, data[["id"]])
  quiet_msg(quiet, "Summary with robust standard error:")
  quiet_print(quiet, format.data.frame(robust_model$summary, digits = 3))
  quiet_line(quiet)

  args <- list(
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
    glm_function = glm_function,
    use_sample_weights = use_sample_weights,
    model_formula = model_formula
  )
  result <- list(model = model.full, robust = robust_model, args = args)

  class(result) <- c("TE_msm")
  result
}


#' @rdname fit_msm
setMethod(
  f = "fit_msm",
  signature = "trial_sequence",
  definition = function(object, weight_cols, modify_weights) {
    if (is(object@outcome_model, "te_outcome_unset")) stop("outcome_model not set, please run set_outcome_model")
    if (object@expansion@datastore@N == 0) stop("datastore is empty, please run expand_trials")
    if (!length(object@outcome_data@n_rows)) stop("outcome_data is empty, please run load_expanded_data")

    if (!is.null(weight_cols)) {
      assert_subset(weight_cols, choices = colnames(outcome_data(object)))
      object@outcome_data@data$w <- object@outcome_data@data[, Reduce(`*`, .SD), .SDcols = weight_cols]
    } else {
      object@outcome_data@data$w <- 1
    }

    if (!is.null(modify_weights)) {
      assert_function(modify_weights)
      temp_weights <- modify_weights(object@outcome_data@data$w)
      assert_numeric(temp_weights, lower = 0, finite = TRUE)
      object@outcome_data@data$w <- temp_weights
    }

    object@outcome_model@fitted <- fit_outcome_model(object@outcome_model@model_fitter,
      data = object@outcome_data@data,
      formula = object@outcome_model@formula,
      weights = object@outcome_data@data$w
    )

    object
  }
)
