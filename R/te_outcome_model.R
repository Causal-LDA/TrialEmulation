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

# te_outcome_model_unset -----
setClass("te_outcome_model_unset", contains = "te_outcome_model")

# te_stats_glm_logit_outcome_fitted -----

setClass(
  "te_stats_glm_logit_outcome_fitted",
  contains = "te_outcome_fitted"
)

# show method for te_outcome_model -----
setMethod(
  "show",
  c(object = "te_outcome_model"),
  function(object) {
    catn("TE Outcome Model Object")
    catn("Formula:", paste0(object@formula))
    catn("Treatment_var:", object@treatment_var)
    catn("Adjustment_vars:", object@adjustment_vars)
    catn("")
    # model_fitter?
    show(object@fitted)
    # outcome_data?
  }
)

# show method for te_outcome_model_unset -----
setMethod(
  "show",
  c(object = "te_outcome_model_unset"),
  function(object) cat("Outcome model not specified. See set_outcome_model()")
)

# show method for te_outcome_fitted -----
setMethod(
  "show",
  c(object = "te_outcome_fitted"),
  function(object) {
    if (length(object@summary)) {
      catn("TE Outcome Fitted Object")
      catn("Model Summary:")
      catn("")
      show(as.data.frame(object@summary$tidy))
      catn("")
      show(as.data.frame(object@summary$glance))
    }
  }
)
