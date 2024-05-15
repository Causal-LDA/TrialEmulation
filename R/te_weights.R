#' Fitted Weights Object
#'
#' @slot specification list. The parameters specifying how the model should be fit
#' @slot summary list of data.frames. Tidy model summaries a la `broom()` and `glance()`
#' @slot fitted list. Saves the model objects or at least summaries if large
setClass("te_weights_fitted",
  slots = c(
    label = "character",
    summary = "list",
    fitted = "numeric"
  )
)


#' Weight Model Fitter Class
#'
#' This is a virtual class which other weight fitter classes should inherit from. Objects of these
#' class exist to define how the censoring and switching weight models are fit.
#' They are used for the dispatch of the method [fit_weights_model]
#'
#' @slot save_path character. A string specifying a directory to save the weight model objects.
#'
#' @export
#' @family weight_fitter
setClass(
  "te_weights_fitter",
  contains = "VIRTUAL",
  slots = c(
    save_path = "character"
  )
)


setClass("te_weights_spec",
  slots = c(
    numerator = "formula",
    denominator = "formula",
    pool_numerator = "logical",
    pool_denominator = "logical",
    model_fitter = "te_weights_fitter",
    fitted = "list"
  )
)

setClass("te_weights_unset", contains = "te_weights_spec")

#' @rdname te_weights_fitter-class
setClass(
  "te_stats_glm_logit",
  contains = "te_weights_fitter"
)


#' Fit weight models using `stats::glm`
#'
#' Specify that the pool logistic regression models should be fit using [stats::glm] with `family = binomial(link =
#' "logit")`.
#'
#' @param save_path A string specifying a directory to save the weight model objects.
#'
#' @return An object of class `te_stats_glm_logit` inheriting from [te_weights_fitter-class] which is used for
#'   dispatching methods for the fitting models.
#' @export
#' @family weight_fitter
#' @examples
#' stats_glm_logit(tempdir())
stats_glm_logit <- function(save_path) {
  assert_path_for_output(save_path, overwrite = TRUE)
  new("te_stats_glm_logit", save_path = save_path)
}


#' Method for fitting weight models
#'
#' @param object The object determining which method should be used, containing any slots containing user defined
#'   parameters.
#' @param data `data.frame` containing outcomes and covariates as defined in `formula`.
#' @param formula `formula` describing the model.
#' @param label A short string describing the model.
#'
#' @return An object of class `te_weights_fitted`
#' @export
#'
#' @examples
#' fitter <- stats_glm_logit(tempdir())
#' data(data_censored)
#' # Not usually called directly by a user
#' fitted <- fit_weights_model(
#'   object = fitter,
#'   data = data_censored,
#'   formula = 1 - censored ~ x1 + age_s + treatment,
#'   label = "Example model for censoring"
#' )
#' fitted
#' unlink(fitted@summary$save_path$path)
setGeneric("fit_weights_model", function(object, data, formula, label) standardGeneric("fit_weights_model"))

#' @rdname fit_weights_model
setMethod(
  f = "fit_weights_model",
  signature = "te_weights_fitter",
  function(object, data, formula, label) {
    catn("No specific fit_weights_model() method found for object with fitter class '", class(object), "'.", sep = "")
  }
)

#' @rdname fit_weights_model
setMethod(
  f = "fit_weights_model",
  signature = "te_stats_glm_logit",
  function(object, data, formula, label) {
    model <- stats::glm(formula, data, family = binomial("logit"))
    if (length(object@save_path)) {
      if (!dir.exists(object@save_path)) dir.create(object@save_path, recursive = TRUE)
      file <- tempfile(pattern = "model_", tmpdir = object@save_path, fileext = ".rds")
      saveRDS(model, file = file)
    }
    new(
      "te_weights_fitted",
      label = label,
      summary = list(tidy = broom::tidy(model), glance = broom::glance(model), save_path = data.frame(path = file)),
      fitted = model$fitted
    )
  }
)


setMethod(
  f = "show",
  signature = "te_weights_unset",
  function(object) {
    catn("No weight model specified")
  }
)

setMethod(
  f = "show",
  signature = "te_weights_spec",
  function(object) {
    catn("Numerator formula:", as.character(object@numerator))
    catn("Denominator formula:", as.character(object@denominator))
    # TODO catn("Pooled numerator:", censor_weights@pool_numerator)censor_weights@pool_denominator)
    catn("Model fitter type:", class(object@model_fitter))
    if (length(object@fitted)) {
      call <- match.call()
      show_string <- paste0("show(", call$object, "@fitted)")
      catn("View weight model summaries with", show_string)
    } else {
      catn("Weight models not fitted")
    }
  }
)


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
