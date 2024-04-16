#' Weighting Method
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


setClass("te_weights_fitter",
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

stats_glm_logit <- setClass(
  "te_stats_glm_logit",
  contains = "te_weights_fitter"
)

# returns a `te_weights_fitted` object
setGeneric("fit_weights_model", function(object, data, formula, label) standardGeneric("fit_weights_model"))

setMethod(
  f = "fit_weights_model",
  signature = "te_weights_fitter",
  function(object, data, formula, label) {
    catn("No specific fit_weights_model() method found for object with fitter class '", class(object), "'.", sep = "")
  }
)


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

setClass(
  "dummy",
  contains = "te_weights_fitter"
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
