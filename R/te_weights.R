#' @include te_model_fitter.R

#' Fitted Weights Object
#'
#' @slot label string. A short description of the model
#' @slot summary list of data.frames. Tidy model summaries a la `broom()` and `glance()`
#' @slot fitted list. Saves the model objects or at least summaries if large
setClass("te_weights_fitted",
  slots = c(
    label = "character",
    summary = "list",
    fitted = "numeric"
  )
)



# te_weights_spec -------
setClass("te_weights_spec",
  slots = c(
    numerator = "formula",
    denominator = "formula",
    pool_numerator = "logical",
    pool_denominator = "logical",
    model_fitter = "te_model_fitter",
    fitted = "list"
  )
)

setValidity("te_weights_spec", function(object) {
  msg <- character()

  if (!is(object, "te_weights_unset") && !hasMethod("fit_weights_model", class(object@model_fitter))) {
    msg <- c(
      msg,
      paste(
        "No fit_weights_model method found for object with model_fitter class",
        sQuote(class(object@model_fitter))
      )
    )
  }

  if (length(msg)) msg else TRUE
})


# te_weights_unstet -------
setClass("te_weights_unset", contains = "te_weights_spec")



setMethod(
  f = "show",
  signature = "te_weights_unset",
  function(object) {
    catn(" - No weight model specified")
  }
)

setMethod(
  f = "show",
  signature = "te_weights_spec",
  function(object) {
    catn(" - Numerator formula:", as.character(object@numerator))
    catn(" - Denominator formula:", as.character(object@denominator))
    # TODO catn("Pooled numerator:", censor_weights@pool_numerator)censor_weights@pool_denominator)
    catn("Model fitter type:", class(object@model_fitter))
    if (length(object@fitted)) {
      catn("View weight model summaries with show_weight_models()")
    } else {
      catn("Weight models not fitted")
    }
  }
)

#' Show Weight Model Summaries
#'
#' @param object A [trial_sequence] object after fitting weight models with [calculate_weights()]
#'
#' @return Prints summaries of the censoring models
#' @export
show_weight_models <- function(object) {
  assert_class(object, "trial_sequence")
  if (.hasSlot(object, "censor_weights")) {
    if (test_list(object@censor_weights@fitted, types = "te_weights_fitted")) {
      lapply(object@censor_weights@fitted, show)
    }
  }

  if (.hasSlot(object, "switch_weights")) {
    if (test_list(object@switch_weights@fitted, types = "te_weights_fitted")) {
      lapply(object@switch_weights@fitted, show)
    }
  }
  invisible()
}


setMethod(
  f = "show",
  signature = "te_weights_fitted",
  function(object) {
    catn("Model:", object@label, "\n")
    for (df in object@summary) {
      print.data.frame(df, row.names = FALSE, right = FALSE)
      catn("")
    }
  }
)
