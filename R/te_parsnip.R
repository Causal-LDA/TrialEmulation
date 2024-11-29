#' @include generics.R te_model_fitter.R
NULL


#' Fit Models using parsnip
#'
#' The classes and (internal) methods defined for using parsnip to fit the weight models.
#' @slot model_spec A model specification defined with the `parsnip` package.
#' @family model_fitter_classes
#' @keywords internal
setClass(
  "te_parsnip_model",
  contains = "te_model_fitter",
  slots = c(
    model_spec = "ANY"
  )
)
setValidity(
  "te_parsnip_model",
  method = function(object) {
    msg <- character()
    if (!inherits(object@model_spec, "model_spec")) {
      msg <- c(msg, "model_spec must be a parsnip model_spec object")
    }
    if (!object@model_spec$mode == "classification") {
      msg <- c(msg, "model_spec must have mode classification")
    }
    if (length(msg) == 0) {
      TRUE
    } else {
      msg
    }
  }
)



#' Fit outcome models using `parsnip` models
#'
#' `r lifecycle::badge('experimental')`
#'
#' Specify that the models should be fit using a classification model specified with the `parsnip` package.
#'
#' @param model_spec A `parsnip` model definition with `mode = "classification"`.
#' @param save_path Directory to save models. Set to `NA` if models should not be saved.
#' @return An object of class `te_parsnip_model` inheriting from [te_model_fitter-class] which is used for
#'   dispatching methods for the fitting models.
#' @export
#' @details
#' Warning: This functionality is experimental and not recommended for use in analyses.
#' \eqn{sqrt{n}}-consistency estimation and valid inference of the parameters in marginal structural models for
#' emulated trials generally require that the weights for treatment switching and censoring be estimated at parametric
#' rates, which is generally not possible when using data-adaptive estimation of high-dimensional regressions.
#' Therefore, we only recommend using [stats_glm_logit()].
#'
#'
#' @family model_fitter
#' @examples
#' \dontrun{
#' if (
#'   requireNamespace("parsnip", quietly = TRUE) &&
#'     requireNamespace("rpart", quietly = TRUE)
#' ) {
#'   # Use a decision tree model fitted with the rpart package
#'   parsnip_model(
#'     model_spec = parsnip::decision_tree(tree_depth = 30) |>
#'       set_mode("classification") |>
#'       set_engine("rpart"),
#'     save_path = tempdir()
#'   )
#' }
#' }
#'
parsnip_model <- function(model_spec, save_path) {
  if (!is.na(save_path)) {
    assert_path_for_output(save_path, overwrite = TRUE)
  } else {
    save_path <- NA_character_
  }
  assert_class(model_spec, "model_spec")
  if (model_spec[["mode"]] != "classification") stop("model_spec must have mode \"classification\".")
  new("te_parsnip_model", model_spec = model_spec, save_path = save_path)
}

#' @describeIn te_parsnip_model-class Fit the weight models object via [calculate_weights] on `trial_sequence`
#' @inheritParams fit_weights_model
setMethod(
  f = "fit_weights_model",
  signature = "te_parsnip_model",
  function(object, data, formula, label) {
    if (!requireNamespace("parsnip")) stop("Package 'parsnip' must be installed to use parsnip models.")
    data$treatment <- factor(data$treatment, levels = c(0, 1))
    parsnip_fit <- parsnip::fit(
      object@model_spec,
      formula,
      data = data
    )

    if (!is.na(object@save_path)) {
      if (!dir.exists(object@save_path)) dir.create(object@save_path, recursive = TRUE)
      file <- tempfile(pattern = "model_", tmpdir = object@save_path, fileext = ".rds")
      saveRDS(parsnip_fit, file = file)
    }

    fitted <- predict(parsnip_fit, data, type = "prob")[[".pred_1"]]
    # TODO how can we show the nice name of the type of model, not just "parsnip" when we print.
    tidy <- tryCatch(broom::tidy(parsnip_fit), error = function(e) data.frame(error = as.character(e)))
    glance <- tryCatch(broom::glance(parsnip_fit), error = function(e) data.frame(error = as.character(e)))
    new(
      "te_weights_fitted",
      label = label,
      summary = list(tidy = tidy, glance = glance, save_path = data.frame(path = file)),
      fitted = fitted
    )
  }
)
