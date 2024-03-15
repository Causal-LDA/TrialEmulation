#' Weighting Method
#'
#' @slot specification list. The parameters specifying how the model should be fit
#' @slot summary list of data.frames. Tidy model summaries a la `broom()` and `glance()`
#' @slot fitted list. Saves the model objects or at least summaries if large
setClass("te_weight",
  slots = c(
    specification = "list",
    summary = "list",
    fitted = "list",
  )
)


weights
