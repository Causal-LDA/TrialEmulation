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
#' @keywords internal
robust_calculation <- function(model, data_id) {
  var_matrix <- sandwich::vcovCL(
    model,
    cluster = data_id,
    type = NULL,
    sandwich = TRUE,
    fix = FALSE
  )
  se <- sqrt(diag(var_matrix))
  output <- data.frame(names = names(model$coefficients))
  output$estimate <- model$coefficients
  output$robust_se <- se[names(model$coefficients)]
  output$`2.5%` <- output$estimate - (1.96 * output$robust_se)
  output$`97.5%` <- output$estimate + (1.96 * output$robust_se)
  output$z <- output$estimate / output$robust_se
  output$p_value <- 2 * (1 - pnorm(abs(output$z)))

  result <- list(summary = output, matrix = var_matrix)
  class(result) <- "TE_robust"
  result
}
