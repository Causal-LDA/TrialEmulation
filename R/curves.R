

#' Predict Cumulative Incidence with Confidence Intervals
#'
#' @param object Object from [data_modelling()] or [initiators()].
#' @param newdata Baseline trial data to predict cumulative incidence or survival for. If `newdata` contains
#' rows with `followup_time > 0` these will be removed.
#' @param type Type of values to calculate. Either cumulative incidence (`"cum_inc"`) or survival (`"survival"`).
#' @param predict_times Follow-up times to predict. Any times given in newdata will be ignored.
#' @param conf_int Calculate a confidence interval using coefficient samples from a multivariate normal distribution
#' based on the robust covariance matrix.
#' @param samples The number of samples of the coefficients for prediction models.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A list of three data frames containing the cumulative incidences for each of the assigned treatment options
#'  and the difference between them.
#' @export
#' @importFrom stats .checkMFClasses coef delete.response model.frame model.matrix terms setNames
#' @examples
#' model <- initiators(
#'   data = trial_example,
#'   outcome_cov = c("catvarA", "catvarB", "catvarC", "nvarA", "nvarB", "nvarC"),
#' )
#'
#' predicted_ci <- predict(model, predict_times = 0:30, samples = 20)
#'
#' # Plot the cumulative incidence curves for each treatment
#' plot(predicted_ci[[1]]$followup_time, predicted_ci[[1]]$estimate,
#'   type = "l",
#'   xlab = "Follow-up Time", ylab = "Cumulative Incidence"
#' )
#' lines(predicted_ci[[1]]$followup_time, predicted_ci[[1]]$`2.5%`, lty = 2)
#' lines(predicted_ci[[1]]$followup_time, predicted_ci[[1]]$`97.5%`, lty = 2)
#'
#' lines(predicted_ci[[2]]$followup_time, predicted_ci[[2]]$estimate, type = "l", col = 2)
#' lines(predicted_ci[[2]]$followup_time, predicted_ci[[2]]$`2.5%`, lty = 2, col = 2)
#' lines(predicted_ci[[2]]$followup_time, predicted_ci[[2]]$`97.5%`, lty = 2, col = 2)
#' legend("topleft", title = "Assigned Treatment", legend = c("0", "1"), col = 1:2, lty = 1)
#'
#' # Plot the difference in cumulative incidence over follow up
#' plot(predicted_ci[[3]]$followup_time, predicted_ci[[3]]$estimate,
#'   type = "l",
#'   xlab = "Follow-up Time", ylab = "Difference in Cumulative Incidence",
#'   ylim = c(-0.1, 0.1)
#' )
#' lines(predicted_ci[[3]]$followup_time, predicted_ci[[3]]$`2.5%`, lty = 2)
#' lines(predicted_ci[[3]]$followup_time, predicted_ci[[3]]$`97.5%`, lty = 2)
#'
predict.RTE_model <- function(object,
                              newdata,
                              predict_times,
                              conf_int = TRUE,
                              samples = 100,
                              type = c("cum_inc", "survival"),
                              ...) {
  assert_class(object$model, "glm")
  model <- object$model
  type <- match.arg(type)
  assert_integerish(predict_times, lower = 0, min.len = 1)
  assert_flag(conf_int)
  assert_int(samples, lower = 1)

  coefs_mat <- matrix(coef(model), nrow = 1)
  if (conf_int) {
    assert_matrix(object$robust$matrix, nrows = ncol(coefs_mat), ncols = ncol(coefs_mat))
    coefs_mat <- rbind(coefs_mat, mvtnorm::rmvnorm(n = samples, mean = coef(model), sigma = object$robust$matrix))
  }

  newdata <- check_newdata(newdata, model, predict_times)
  model_terms <- delete.response(terms(model))
  model_frame <- model.frame(model_terms, newdata, xlev = model$xlevels)
  if (!is.null(data_classes <- attr(model_terms, "dataClasses"))) .checkMFClasses(data_classes, model_frame)
  model_matrix <- model.matrix(model_terms, model_frame, contrasts.arg = model$contrasts)

  pred_fun <- if (type == "survival") {
    calculate_survival
  } else if (type == "cum_inc") {
    calculate_cum_inc
  }

  pred_list <- lapply(
    c(assigned_treatment_0 = 0, assigned_treatment_1 = 1),
    calculate_predictions,
    model_matrix = model_matrix,
    pred_fun = pred_fun,
    coefs_mat = coefs_mat,
    linkinv = model$family$linkinv,
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
        setNames(data.frame(predict_times, pred_matrix[, 1]), c("followup_time", col_names))
      }
    }
  )
}



#' Check Data used for Prediction
#'
#' @param newdata new data to predict, or missing.
#' @param model glm model object.
#' @param predict_times times to predict to add to resulting newdata.
#' @noRd
#' @return A `newdata` data.frame
check_newdata <- function(newdata, model, predict_times) {
  required_vars <- setdiff(all.vars(model$formula), "outcome")
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
      warning("Attributes of newdata do not match data used for fitting. Attempting to fix.")
      newdata <- rbind(model$data[0, required_vars, with = FALSE], newdata)
      fixed <- all.equal(
        lapply(model$data[, required_vars, with = FALSE], attributes),
        lapply(newdata, attributes)
      )
      if (!fixed) {
        print(fixed)
        stop("Attributes do not match.")
      }
    }
  }

  n_baseline <- nrow(newdata)
  newdata <- newdata[rep(seq_len(n_baseline), times = length(predict_times)), ]
  newdata$followup_time <- rep(predict_times, each = n_baseline)

  newdata
}


#' Calculate Cumulative Incidence and Survival
#'
#' @param p_mat Probability matrix with rows for each subject and follow-up time as the columns.
#'
#' @return A vector containing the cumulative incidence or survival values.
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
#' RandomisedTrialsEmulation:::calculate_cum_inc(surv_prob)
calculate_cum_inc <- function(p_mat) {
  assert_matrix(p_mat, mode = "numeric")
  result <- 1 - calculate_survival(p_mat)
  assert_monotonic(result)
  result
}

#' @rdname calculate_cum_inc
#' @examples
#' RandomisedTrialsEmulation:::calculate_survival(surv_prob)
calculate_survival <- function(p_mat) {
  assert_matrix(p_mat, mode = "numeric")
  result <- rowMeans(apply(1 - p_mat, 1, cumprod))
  assert_monotonic(result, increasing = FALSE)
  result
}



#' Calculate and transform predictions
#'
#' @param model_matrix Model matrix to multiple with coefficients
#' @param treatment_value Value to insert into `assigned_treatment` column
#' @param pred_fun Function to transform prediction matrix
#' @param coefs_mat Matrix of coefficients corresponding to `model_matrix`.
#' @param linkinv Inverse link function for transforming linear predictor
#' @param matrix_n_col Expected number of column after prediction.
#'
#' @return A matrix with transformed predicted values. Number of columns corresponds
#'  to the number of rows of `coefs_mat`
calculate_predictions <- function(model_matrix, treatment_value, pred_fun, coefs_mat, linkinv, matrix_n_col) {
  treatment_col <- which(colnames(model_matrix) == "assigned_treatment")
  model_matrix[, treatment_col] <- treatment_value
  pred_list <- lapply(seq_len(nrow(coefs_mat)), function(coef_i) {
    pred_fun(matrix(linkinv(model_matrix %*% t(coefs_mat[coef_i, , drop = FALSE])), ncol = matrix_n_col))
  })
  matrix(unlist(pred_list), ncol = length(pred_list))
}
