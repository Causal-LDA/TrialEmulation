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
    fitted = "list",
    data_subset_expr = "list"
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

setClass("te_weights_switch", contains = "te_weights_spec")
setClass("te_weights_censoring", contains = "te_weights_spec")

# te_weights_unset -------
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
    if (isTRUE(object@pool_numerator)) {
      if (isTRUE(object@pool_denominator)) {
        catn(" - Numerator and denominotor models are pooled across treatment arms.")
      } else {
        catn(" - Numerator model is pooled across treatment arms. Denominator model is not pooled.")
      }
    }
    catn(" - Model fitter type:", class(object@model_fitter))
    if (length(object@fitted)) {
      catn(" - View weight model summaries with show_weight_models()")
    } else {
      catn(" - Weight models not fitted. Use calculate_weights()")
    }
  }
)
#' Show Weight Model Summaries
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param object A [trial_sequence] object after fitting weight models with [calculate_weights()]
#'
#' @return Prints summaries of the censoring models
#' @export
show_weight_models <- function(object) {
  assert_class(object, "trial_sequence")
  if (.hasSlot(object, "censor_weights")) {
    if (test_list(object@censor_weights@fitted, types = "te_weights_fitted")) {
      cat_underline("Weight Models for Informative Censoring")
      for (i in names(object@censor_weights@fitted)) {
        catn("[[", i, "]]", sep = "")
        show(object@censor_weights@fitted[[i]])
      }
    }
  }

  if (.hasSlot(object, "switch_weights")) {
    if (test_list(object@switch_weights@fitted, types = "te_weights_fitted")) {
      cat_underline("Weight Models for Treatment Switching")
      for (i in names(object@switch_weights@fitted)) {
        catn("[[", i, "]]", sep = "")
        show(object@switch_weights@fitted[[i]])
      }
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


#' Data used in weight model fitting
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param object A [trial_sequence] object
#' @param type Select a censoring or switching model
#' @param model The model name
#' @param set_col A character string to specifying a new column to contain indicators for observations used in
#' fitting this model.
#'
#' @return If `set_col` is not specified a logical `data.table` column is returned. Otherwise
#' @export
#' @examples
#' trial_pp <- trial_sequence("PP") |>
#'   set_data(data_censored) |>
#'   set_switch_weight_model(
#'     numerator = ~age,
#'     denominator = ~ age + x1 + x3,
#'     model_fitter = stats_glm_logit(tempdir())
#'   ) |>
#'   calculate_weights()
#' ipw_data(trial_pp)
#' show_weight_models(trial_pp)
#'
#' # get logical column for own processing
#' i <- weight_model_data_indices(trial_pp, "switch", "d0")
#'
#' # set column in data
#' weight_model_data_indices(trial_pp, "switch", "d0", set_col = "sw_d0")
#' weight_model_data_indices(trial_pp, "switch", "d1", set_col = "sw_d1")
#' ipw_data(trial_pp)
weight_model_data_indices <- function(object, type = c("switch", "censor"), model, set_col = NULL) {
  assert_choice(type, c("switch", "censor"))
  model_names <- switch(type,
    "switch" = names(object@switch_weights@data_subset_expr),
    "censor" = names(object@censor_weights@data_subset_expr)
  )
  model <- assert_choice(model, model_names)
  expr <- object@switch_weights@data_subset_expr[[model]]

  if (!is.null(set_col)) {
    assert_names(set_col)
    object@data@data[, (set_col) := eval(expr)]
  } else {
    object@data@data[, list(i = eval(expr))]
  }
}
