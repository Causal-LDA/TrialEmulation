#' @include generics.R te_outcome_model.R te_model_fitter.R
NULL

# stats::glm ---------


#' Fit Models using logistic stats::glm
#'
#' The classes and (internal) methods defined for using [stats::glm] to fit the logistic regression models.
#'
#' @rdname te_stats_glm_logit-class
#' @family model_fitter_classes
#' @keywords internal
setClass(
  "te_stats_glm_logit",
  contains = "te_model_fitter"
)

#' @rdname te_stats_glm_logit-class
setClass(
  "te_stats_glm_logit_outcome_fitted",
  contains = "te_outcome_fitted"
)

#' Fit outcome models using `stats::glm`
#'
#' `r lifecycle::badge('experimental')`
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

#' @describeIn te_stats_glm_logit-class Fit the weight models object via [calculate_weights] on `trial_sequence`
#' @inheritParams fit_weights_model
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

#' @describeIn te_stats_glm_logit-class Fit the outcome model object via [fit_msm] on `trial_sequence`
#' @inheritParams fit_outcome_model
setMethod(
  f = "fit_outcome_model",
  signature = "te_stats_glm_logit",
  function(object, data, formula, weights) {
    data$weights <- if (is.null(weights)) rep(1, nrow(data)) else weights
    model <- glm(
      formula = formula,
      data = data,
      family = binomial("logit"),
      x = FALSE,
      y = FALSE,
      weights = weights
    )
    if (!is.na(object@save_path)) {
      if (!dir.exists(object@save_path)) dir.create(object@save_path, recursive = TRUE)
      file <- tempfile(pattern = "model_", tmpdir = object@save_path, fileext = ".rds")
      saveRDS(model, file = file)
      save_path <- data.frame(save = file)
    }

    vcov <- sandwich::vcovCL(
      model,
      cluster = data[["id"]],
      type = NULL,
      sandwich = TRUE,
      fix = TRUE
    )

    model_list <- list(
      model = model,
      vcov = vcov
    )

    coef_obj <- lmtest::coeftest(model, vcov. = vcov, save = TRUE)
    summary_list <- list()
    summary_list[["tidy"]] <- broom::tidy(coef_obj, conf.int = TRUE)
    summary_list[["glance"]] <- broom::glance(coef_obj)
    if (!is.na(object@save_path)) summary_list[["save_path"]] <- save_path

    new(
      "te_stats_glm_logit_outcome_fitted",
      model = model_list,
      summary = summary_list
    )
  }
)

#' @describeIn te_stats_glm_logit-class Predict from the fitted model object via [predict] on `trial_sequence`
#' @inheritParams predict_marginal
#' @param object Object to dispatch method on
setMethod(
  f = "predict",
  signature = "te_stats_glm_logit_outcome_fitted",
  function(object,
           newdata,
           predict_times,
           conf_int = TRUE,
           samples = 100,
           type = c("cum_inc", "survival")) {
    # Derived from predict.TE_msm
    assert_class(object@model$model, "glm")
    model <- object@model$model
    type <- match.arg(type)
    assert_integerish(predict_times, lower = 0, min.len = 1)
    assert_flag(conf_int)
    assert_int(samples, lower = 1)

    coefs_mat <- matrix(coef(model), nrow = 1)
    if (conf_int) {
      assert_matrix(object@model$vcov, nrows = ncol(coefs_mat), ncols = ncol(coefs_mat))
      coefs_mat <- rbind(
        coefs_mat,
        mvtnorm::rmvnorm(n = samples, mean = coef(model), sigma = object@model$vcov, checkSymmetry = FALSE)
      )
    }

    newdata <- check_newdata(newdata, model, predict_times)

    pred_fun <- if (type == "survival") {
      calculate_survival
    } else if (type == "cum_inc") {
      calculate_cum_inc
    }

    pred_list <- calculate_predictions(
      newdata = newdata,
      model = model,
      treatment_values = c(assigned_treatment_0 = 0, assigned_treatment_1 = 1),
      pred_fun = pred_fun,
      coefs_mat = coefs_mat,
      matrix_n_col = length(predict_times)
    )

    pred_list$difference <- pred_list$assigned_treatment_1 - pred_list$assigned_treatment_0
    mapply(
      pred_matrix = pred_list,
      col_names = paste0(type, c("", "", "_diff")),
      SIMPLIFY = FALSE,
      FUN = function(pred_matrix, col_names) {
        if (conf_int) {
          quantiles <- apply(pred_matrix, 1, quantile, probs = c(0.025, 0.975))
          setNames(
            data.frame(predict_times, pred_matrix[, 1], quantiles[1, ], quantiles[2, ]),
            c("followup_time", col_names, "2.5%", "97.5%")
          )
        } else {
          setNames(data.frame(predict_times, pred_matrix[, 1]), nm = c("followup_time", col_names))
        }
      }
    )
  }
)
