
#' Robust Variance Calculation
#'
#' This function performs the calculation of robust standard errors based on
#' variances estimated using [`sandwich::vcovCL`].
#'
#' @param model The logistic regression model object.
#' @param data_id Values of id column of the data (ie `data[, id]`) to identify clusters.
#'
#' @returns A list with elements `summary`, a table with the model summary using the
#' robust variance estimates, and `matrix`, the `sandwich` covariance matrix.
#'
robust_calculation <- function(model, data_id) {
  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  lb <- estimate <- robust_se <- ub <- z <- p_value <- NULL

  var_matrix <- sandwich::vcovCL(
    model,
    cluster = data_id,
    type = NULL,
    sandwich = TRUE,
    fix = FALSE
  )
  se <- sqrt(diag(var_matrix))

  est_temp <- model$coefficients
  output <- data.table(
    names = names(est_temp),
    estimate = est_temp,
    robust_se = se[names(est_temp)]
  )
  output[, lb := estimate - (1.96 * robust_se)]
  output[, ub := estimate + (1.96 * robust_se)]
  output[, z := estimate / robust_se]
  output[, p_value := format.pval(2 * (1 - pnorm(abs(z))), eps = 0.001)]

  list(summary = output, matrix = var_matrix)
}
