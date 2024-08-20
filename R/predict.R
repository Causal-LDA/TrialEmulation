#' @rdname predict_marginal
#' @importFrom stats .checkMFClasses coef delete.response model.frame model.matrix terms setNames
#' @exportS3Method stats::predict
#' @examples
#' # Prediction for initiators() or trial_msm() objects -----
#'
#' # If necessary set the number of `data.table` threads
#' data.table::setDTthreads(2)
#'
#' data("te_model_ex")
#' predicted_ci <- predict(te_model_ex, predict_times = 0:30, samples = 10)
#'
#' # Plot the cumulative incidence curves under treatment and non-treatment
#' plot(predicted_ci[[1]]$followup_time, predicted_ci[[1]]$cum_inc,
#'   type = "l",
#'   xlab = "Follow-up Time", ylab = "Cumulative Incidence",
#'   ylim = c(0, 0.7)
#' )
#' lines(predicted_ci[[1]]$followup_time, predicted_ci[[1]]$`2.5%`, lty = 2)
#' lines(predicted_ci[[1]]$followup_time, predicted_ci[[1]]$`97.5%`, lty = 2)
#'
#' lines(predicted_ci[[2]]$followup_time, predicted_ci[[2]]$cum_inc, type = "l", col = 2)
#' lines(predicted_ci[[2]]$followup_time, predicted_ci[[2]]$`2.5%`, lty = 2, col = 2)
#' lines(predicted_ci[[2]]$followup_time, predicted_ci[[2]]$`97.5%`, lty = 2, col = 2)
#' legend("topleft", title = "Assigned Treatment", legend = c("0", "1"), col = 1:2, lty = 1)
#'
#' # Plot the difference in cumulative incidence over follow up
#' plot(predicted_ci[[3]]$followup_time, predicted_ci[[3]]$cum_inc_diff,
#'   type = "l",
#'   xlab = "Follow-up Time", ylab = "Difference in Cumulative Incidence",
#'   ylim = c(0.0, 0.5)
#' )
#' lines(predicted_ci[[3]]$followup_time, predicted_ci[[3]]$`2.5%`, lty = 2)
#' lines(predicted_ci[[3]]$followup_time, predicted_ci[[3]]$`97.5%`, lty = 2)
#'
predict.TE_msm <- function(object,
                           newdata,
                           predict_times,
                           conf_int = TRUE,
                           samples = 100,
                           type = c("cum_inc", "survival"),
                           ...) {
  if (object$args$estimand_type == "As-Treated") {
    warning("As-Treated estimands are not currently supported by this predict method. Results may be unexpected.")
  }

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



#' Check Data used for Prediction
#'
#' @param newdata new data to predict, or missing.
#' @param model glm model object.
#' @param predict_times times to predict to add to resulting newdata.
#' @keywords internal
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
#' @noRd
#' @examples
#' surv_prob <- matrix(
#'   c(
#'     0.1, 0.1, 0.1,
#'     0.5, 0.2, 0.1
#'   ),
#'   nrow = 2,
#'   byrow = TRUE
#' )
#' TrialEmulation:::calculate_cum_inc(surv_prob)
calculate_cum_inc <- function(p_mat) {
  assert_matrix(p_mat, mode = "numeric")
  result <- 1 - calculate_survival(p_mat)
  assert_monotonic(result)
  result
}

#' @rdname calculate_cum_inc
#' @noRd
#' @keywords internal
#' TrialEmulation:::calculate_survival(surv_prob)
calculate_survival <- function(p_mat) {
  assert_matrix(p_mat, mode = "numeric")
  result <- .colMeans(cumprod_matrix(1 - p_mat, "rows"), nrow(p_mat), ncol(p_mat))
  assert_monotonic(result, increasing = FALSE)
  result
}

cumprod_matrix <- function(x, by = c("rows", "cols")) {
  by <- match.arg(by, choices = c("rows", "cols"), several.ok = FALSE)
  y <- matrix(1, nrow = nrow(x), ncol = ncol(x))

  if (by == "cols") {
    y[1, ] <- x[1, ]
    for (i in 2:dim(x)[1]) y[i, ] <- y[i - 1, ] * x[i, ]
  } else if (by == "rows") {
    y[, 1] <- x[, 1]
    for (i in 2:dim(x)[2]) y[, i] <- y[, i - 1] * x[, i]
  }
  y
}

#' Calculate and transform predictions
#'
#' @param model GLM object
#' @param newdata New data to predict outcome
#' @param treatment_values Named vector of value to insert into `assigned_treatment` column
#' @param pred_fun Function to transform prediction matrix
#' @param coefs_mat Matrix of coefficients corresponding to `model_matrix`.
#' @param matrix_n_col Expected number of column after prediction.
#'
#' @return A matrix with transformed predicted values. Number of columns corresponds
#'  to the number of rows of `coefs_mat`
#' @keywords internal
calculate_predictions <- function(newdata, model, treatment_values, pred_fun, coefs_mat, matrix_n_col) {
  model_terms <- delete.response(terms(model))
  model_frame <- model.frame(model_terms, newdata, xlev = model$xlevels)
  if (!is.null(data_classes <- attr(model_terms, "dataClasses"))) .checkMFClasses(data_classes, model_frame)

  linkinv <- model$family$linkinv

  lapply(treatment_values, function(treatment_value) {
    model_frame$assigned_treatment <- treatment_value
    model_matrix <- model.matrix(model_terms, model_frame, contrasts.arg = model$contrasts)
    pred_list <- lapply(seq_len(nrow(coefs_mat)), function(coef_i) {
      pred_fun(matrix(linkinv(model_matrix %*% t(coefs_mat[coef_i, , drop = FALSE])), ncol = matrix_n_col))
    })
    matrix(unlist(pred_list), ncol = length(pred_list))
  })
}
