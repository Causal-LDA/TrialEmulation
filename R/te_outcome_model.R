#' @include te_model_fitter.R
NULL

#' Fitted Outcome Model Object
#'
#' @slot model list containing fitted model objects.
#' @slot summary list of data.frames. Tidy model summaries a la `broom()` and `glance()`
setClass(
  "te_outcome_fitted",
  slots = c(
    model = "list",
    summary = "list"
  )
)

#' Fitted Outcome Model Object
#'
#' @slot formula `formula` object for the model fitting
#' @slot treatment_var character. The treatment variable
#' @slot adjustment_vars character. Adjustment variables
#' @slot model_fitter Model fitter object
#' @slot fitted list. Saves the model objects
setClass("te_outcome_model",
  slots = c(
    formula = "formula",
    treatment_var = "character",
    adjustment_vars = "character",
    model_fitter = "te_model_fitter",
    fitted = "te_outcome_fitted"
  )
)

# te_stats_glm_logit_outcome_fitted -----

setClass(
  "te_stats_glm_logit_outcome_fitted",
  contains = "te_outcome_fitted"
)
