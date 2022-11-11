

#' Predict Cumulative Incidence with Confidence Intervals
#'
#' @param object Object from [data_modelling()] or [initiators()].
#' @param newdata Baseline trial data to predict cumulative incidence or survival for.
#' @param predict_times Follow-up times to predict. Any times given in newdata will be ignored.
#' @param conf_int Calculate a confidence interval using coefficient samples from a multivariate normal distribution
#' based on the robust covariance matrix.
#' @param samples The number of samples of the coefficients for prediction models.
#'
#' @return A list of two data frames containing the cumulative incidences for each of the assigned treatment options.
#' @export
#' @importFrom stats .checkMFClasses coef delete.response model.frame model.matrix terms
#' @examples
#' model <- initiators(
#'   data = trial_example,
#'   outcome_cov = c("catvarA", "catvarB", "catvarC", "nvarA", "nvarB", "nvarC"),
#' )
#'
#' predicted_ci <- predict(model, predict_times = 1:30, samples = 20)
#'
#' plot_data <- data.table::rbindlist(predicted_ci, idcol = "Treatment")
#' ggplot2::ggplot(plot_data, aes(x = followup_time, y = mean, colour = Treatment)) +
#'   geom_point() +
#'   geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`))
predict.RTE_model <- function(object,
                              newdata,
                              predict_times,
                              conf_int = TRUE,
                              samples = 100) {
  assert_class(object$model, "glm")
  model <- object$model
  required_vars <- setdiff(all.vars(model$formula), "outcome")

  assert_integerish(predict_times, lower = 0, min.len = 1)
  assert_flag(conf_int)
  assert_int(samples, lower = 1)

  if (missing(newdata)) {
    newdata <- model$data[, required_vars, with = FALSE]
    newdata <- newdata[newdata$followup_time == 0, ]
  } else {
    assert_data_frame(newdata, min.rows = 1)
    assert_names(colnames(newdata), must.include = required_vars)
    newdata <- data.table(newdata)[, required_vars, with = FALSE]
    newdata <- newdata[newdata$followup_time == 0, ]
    col_attr_model <- lapply(model$data[, required_vars, with = FALSE], attributes)
    col_attr_newdata <- lapply(newdata, attributes)
    if (!isTRUE(all_eq <- all.equal(col_attr_model, col_attr_newdata))) {
      warning("Attributes of newdata are do not match data used for fitting: ", all_eq)
      message("Attempting to fix.")
      newdata <- rbind(model$data[0, required_vars, with = FALSE], newdata)
    }
  }

  n_baseline <- nrow(newdata)
  newdata <- newdata[rep(seq_len(n_baseline), each = length(predict_times)), ]
  newdata$followup_time <- rep(predict_times, times = n_baseline)

  coefs_mat <- matrix(coef(model), nrow = 1)

  if (conf_int) {
    if (!test_matrix(object$robust$matrix, nrow = ncol(coefs_mat), ncol = ncol(coefs_mat))) {
      stop("Valid covariance matrix not found in object$robust$matrix.")
    }
    coefs_mat <- rbind(
      coefs_mat,
      MASS::mvrnorm(n = samples, mu = coef(model), Sigma = object$robust$matrix)
    )
  }

  model_terms <- delete.response(terms(model))
  model_frame <- model.frame(model_terms, newdata, xlev = model$xlevels)
  if (!is.null(data_classes <- attr(model_terms, "dataClasses"))) .checkMFClasses(data_classes, model_frame)
  model_matrix <- model.matrix(model_terms, model_frame, contrasts.arg = model$contrasts)
  treatment_col <- which(colnames(model_matrix) == "assigned_treatment")

  results <- lapply(
    c(assigned_treatment_0 = 0, assigned_treatment_1 = 1),
    function(treat) {
      model_matrix[, treatment_col] <- treat
      pred_list <- lapply(seq_len(nrow(coefs_mat)), function(coef_i) {
        sum_up_cum_inc(
          list(matrix(
            model$family$linkinv(model_matrix %*% t(coefs_mat[coef_i, , drop = FALSE])),
            ncol = length(predict_times)
          ))
        )
      })
    }
  )

  lapply(results, function(trt_list) {
    cum_inc_matrix <- matrix(unlist(trt_list), ncol = length(trt_list))
    quantiles <- apply(cum_inc_matrix, 1, quantile, probs = c(0.025, 0.5, 0.975))
    data.frame(
      followup_time = predict_times,
      mean = rowMeans(cum_inc_matrix),
      median = quantiles[2, ],
      `2.5%` = quantiles[1, ],
      `97.5%` = quantiles[3, ],
      check.names = FALSE
    )
  })
}

#' Predict Cumulative Incidence
#'
#' @param object The result from `initiators`. Either object or model must be specified.
#' @param model A `glm` object.
#' @param predict_times a vector of follow up times to predict values for.
#' @param newdata `data.frame` to predict survival for. Extracted from `model`/`object` if not provided.
#'
#' @return a list with estimated cumulative incidence curves for `assigned_treatment`
#' equal to 0 and 1.
#' @export
#' @importFrom stats predict
#'
#' @examples
#' data("trial_example")
#' i <- initiators(
#'   data = trial_example,
#'   id = "id",
#'   period = "period",
#'   eligible = "eligible",
#'   treatment = "treatment",
#'   outcome = "outcome",
#'   outcome_cov = ~ nvarA + nvarB,
#'   include_followup_time_case = ~followup_time,
#'   include_expansion_time_case = ~for_period,
#'   last_followup = 30,
#'   last_period = 30,
#'   use_censor = 0,
#'   use_weight = 0
#' )
#'
#' predict_cum_inc(i, predict_times = seq(1, 20, by = 2))
#'
predict_cum_inc <- function(object, model, predict_times, newdata) {
  if (missing(model) && missing(object)) stop("Either model or object must be specified.")
  if (missing(model)) model <- object$model
  assert_class(model, "glm")
  assert_numeric(predict_times, lower = 0, min.len = 1, any.missing = FALSE)

  # Dummy assignments for data.table
  assigned_treatment <- NULL

  if (missing(newdata)) newdata <- as.data.table(model$data)

  baseline <- newdata[newdata$followup_time == 0, ]
  expanded <- baseline[rep(seq_len(nrow(baseline)), times = length(predict_times)), ]
  expanded$followup_time <- rep(predict_times, each = nrow(baseline))

  pred_0 <- predict(model, newdata = expanded[, assigned_treatment := 0], type = "response")
  pred_1 <- predict(model, newdata = expanded[, assigned_treatment := 1], type = "response")

  preds <- expanded[, c("id", "followup_time", "for_period")]
  preds$pred_0 <- pred_0
  preds$pred_1 <- pred_1
  setorderv(preds, cols = c("for_period", "followup_time", "id"))

  pred_trt <- list(
    trt_0 = tapply(preds$pred_0, preds$for_period, matrix, ncol = length(predict_times)),
    trt_1 = tapply(preds$pred_1, preds$for_period, matrix, ncol = length(predict_times))
  )

  lapply(pred_trt, sum_up_cum_inc)
}


#' Helper to Calculate Cumulative Incidence
#'
#' @param p_mat_list A list of probability matrices with rows for each subject and followup time as the columns.
#' All matrices must have the followup times (i.e. same number of columns).
#'
#' @return A vector of cumulative incidences.
#' @export
#'
#' @examples
#' surv_prob_list <- list(
#'   trial_1 = matrix(
#'     c(
#'       0.1, 0.1, 0.1,
#'       0.5, 0.2, 0.1
#'     ),
#'     nrow = 2, byrow = TRUE
#'   ),
#'   trial_2 = matrix(
#'     c(
#'       0.15, 0.15, 0.15,
#'       0.45, 0.25, 0.1
#'     ),
#'     nrow = 2, byrow = TRUE
#'   )
#' )
#' sum_up_cum_inc(surv_prob_list)
sum_up_cum_inc <- function(p_mat_list) {
  cols <- unique(vapply(p_mat_list, ncol, integer(1L)))
  assert_integer(cols, len = 1, lower = 1)

  cum_inc_mat <- vapply(p_mat_list, cum_inc_up_to, numeric(cols))
  total_n <- sum(vapply(p_mat_list, nrow, integer(1L)))
  result <- rowSums(cum_inc_mat) / total_n
  assert_monotonic(result)
  result
}


#' Calculate Cumulative Incidence and Survival
#'
#' @param p_mat Probability matrix with rows for each subject and follow-up time as the columns.
#'
#' @return A vector containing the cumulative incidence or survival values.
#' @export
#'
#' @examples
#' surv_prob <- matrix(
#'   c(
#'     0.1, 0.1, 0.1,
#'     0.5, 0.2, 0.1
#'   ),
#'   nrow = 2,
#'   byrow = TRUE
#' )
#' cum_inc_up_to(surv_prob)
cum_inc_up_to <- function(p_mat) {
  assert_matrix(p_mat, mode = "numeric")

  prod_term <- apply(1 - cbind(0, p_mat)[, -ncol(p_mat), drop = FALSE], 1, cumprod)
  sum_term <- prod_term * t(p_mat)
  cumsum_term <- apply(sum_term, 2, cumsum)
  result <- rowSums(cumsum_term)
  assert_monotonic(result)
  result
}

#' @rdname cum_inc_up_to
#' @export
survival_up_to <- function(p_mat) {
  assert_matrix(p_mat, mode = "numeric")
  result <- c(1, rowMeans(apply(1 - p_mat, 1, cumprod)))
  assert_monotonic(result, increasing = FALSE)
  result
}
