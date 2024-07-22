#' Outcome Model Fitter Class
#'
#' This is a virtual class which other outcome model fitter classes should inherit from. Objects of these class exist to
#' define how the outcome models are fit. They are used for the dispatch of the internal methods [fit_outcome_model],
#' [fit_weights_model] and [predict].
#'
#' @export
#' @family model_fitter
setClass(
  "te_model_fitter",
  contains = "VIRTUAL",
  slots = c(
    save_path = "character"
  )
)
