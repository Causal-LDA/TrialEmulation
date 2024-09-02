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
#' @slot adjustment_vars character. Adjustment variables
#' @slot treatment_var Variable used for treatment
#' @slot stabilised_weights_terms formula. Adjustment terms from numerator models of stabilised weights. These must be
#' included in the outcome model.
#' @slot adjustment_terms formula. User specified terms to include in the outcome model
#' @slot treatment_terms formula. Estimand defined treatment term
#' @slot followup_time_terms formula. Terms to model follow up time within an emulated trial
#' @slot trial_period_terms formula. Terms to model start time ("trial_period") of an emulated trial
#' @slot model_fitter Model fitter object
#' @slot fitted list. Saves the model objects
setClass("te_outcome_model",
  slots = c(
    formula = "formula",
    adjustment_vars = "character",
    treatment_var = "character",
    adjustment_terms = "formula",
    treatment_terms = "formula",
    followup_time_terms = "formula",
    trial_period_terms = "formula",
    stabilised_weights_terms = "formula",
    model_fitter = "te_model_fitter",
    fitted = "te_outcome_fitted"
  )
)

# te_outcome_model_unset -----
setClass("te_outcome_model_unset", contains = "te_outcome_model")


# show method for te_outcome_model -----
setMethod(
  "show",
  c(object = "te_outcome_model"),
  function(object) {
    catn("- Formula:", paste0(object@formula))
    catn("- Treatment variable:", object@treatment_var)
    catn("- Adjustment variables:", object@adjustment_vars)
    catn("- Model fitter type:", class(object@model_fitter))
    catn("")

    show(object@fitted)
    # outcome_data?
  }
)

# show method for te_outcome_model_unset -----
setMethod(
  "show",
  c(object = "te_outcome_model_unset"),
  function(object) cat(" - Outcome model not specified. Use set_outcome_model()")
)

# show method for te_outcome_fitted -----
setMethod(
  "show",
  c(object = "te_outcome_fitted"),
  function(object) {
    if (length(object@summary)) {
      catn("Model Summary:")
      catn("")
      print.data.frame(as.data.frame(object@summary$tidy), row.names = FALSE, digits = 2, right = FALSE)
      catn("")
      print.data.frame(as.data.frame(object@summary$glance), row.names = FALSE, digits = 3, right = FALSE)
    } else {
      catn("Use fit_msm() to fit the outcome model")
    }
  }
)
