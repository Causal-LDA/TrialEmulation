#' @include generics.R
NULL

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

# stats::glm ---------

#' @rdname te_model_fitter-class
setClass(
  "te_stats_glm_logit",
  contains = "te_model_fitter"
)

#' Fit outcome models using `stats::glm`
#'
#' Specify that the pooled logistic regression outcome models should be fit using [stats::glm] with `family =
#' binomial(link = "logit")`.
#'
#' Outcome models additional calculate robust variance estimates using `sandwich::vcovCL`.
#'
#' @param save_path Directory to save models. Set to `NA` if models should not be saved.
#' @return An object of class `te_stats_glm_logit` inheriting from [te_model_fitter-class] which is used for
#'   dispatching methods for the fitting models.
#' @export
#' @family model_fitter
#' @examples
#' stats_glm_logit(save_path = tempdir())
stats_glm_logit <- function(save_path) {
  if (!is.na(save_path)) {
    assert_path_for_output(save_path, overwrite = TRUE)
  } else {
    save_path <- NA_character_
  }
  new("te_stats_glm_logit", save_path = save_path)
}


#' @rdname fit_weights_model
setMethod(
  f = "fit_weights_model",
  signature = "te_stats_glm_logit",
  function(object, data, formula, label) {
    model <- stats::glm(formula, data, family = binomial("logit"))
    if (!is.na(object@save_path)) {
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

#' @rdname fit_outcome_model
setMethod(
  f = "fit_outcome_model",
  signature = "te_stats_glm_logit",
  function(object, data, formula, weights) {
    data$.weights <- if (is.null(weights)) rep(1, nrow(data)) else weights
    model <- glm(
      formula = formula,
      data = data,
      family = binomial("logit"),
      x = FALSE,
      y = FALSE,
      weights = .weights
    )
    if (!is.na(object@save_path)) {
      if (!dir.exists(object@save_path)) dir.create(object@save_path, recursive = TRUE)
      file <- tempfile(pattern = "model_", tmpdir = object@save_path, fileext = ".rds")
      saveRDS(model, file = file)
    }

    vcov <- sandwich::vcovCL(
      model,
      cluster = data[["id"]],
      type = NULL,
      sandwich = TRUE,
      fix = FALSE
    )

    model_list <- list(
      model = model,
      vcov = vcov
    )

    coef_obj <- lmtest::coeftest(model, vcov. = vcov, save = TRUE)
    summary_list <- list(
      tidy = broom::tidy(coef_obj, conf.int = TRUE),
      glance = broom::glance(coef_obj),
      save_path = data.frame(save = object@save_path)
    )

    new(
      "te_stats_glm_logit_outcome_fitted",
      model = model_list,
      summary = summary_list
    )
  }
)
