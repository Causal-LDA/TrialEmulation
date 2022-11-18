#' P99 Weights Function
#'
#' This function truncate the weights of data at the 1st and 99th percentile
#' @param switch_data The data.table with weight column

p99_weight <- function(switch_data) {
  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  weight <- NULL

  p99 <- quantile(switch_data[, weight],
    prob = c(0.99, 0.1),
    type = 1
  )

  len <- nrow(switch_data)
  switch_data[weight > p99[1], weight := p99[1]]
  switch_data[weight < p99[2], weight := p99[2]]
  return(switch_data)
}

#' Limit Weights Function
#'
#' This function truncate the weights using user defined limits
#' @param switch_data The data.table contains weight column
#' @param lower_limit The user defined minimum possible weight
#' @param upper_limit The user defined maximum possible weight

limit_weight <- function(switch_data, lower_limit, upper_limit) {
  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  weight <- NULL

  len <- nrow(switch_data)
  switch_data[weight > upper_limit, weight := upper_limit]
  switch_data[weight < lower_limit, weight := lower_limit]
  return(switch_data)
}


#' Fit a GLM
#'
#' @param formula the model formula.
#' @param data a data frame or similar.
#' @param ...
#' @param glm_function The glm function to call given as a string, such as `"glm"` or `"parglm"`.
#' @noRd
#' @import parglm
#'
#' If no family is specified `binomial("logit")` will be used. If `glm_function = "parglm"` is specified
#' but no `nthreads`, `control`, or `method`, then the default is used
#' `control = parglm.control(nthreads = 4, method = \"FAST\")`.
#'
fit_glm <- function(formula, data, weights, ..., glm_function = "glm") {
  this_call <- match.call(expand.dots = FALSE)
  dots <- list(...)
  this_call$`...` <- NULL
  this_call$glm_function <- NULL
  if (is.null(this_call$family)) this_call$family <- quote(binomial(link = "logit"))
  this_call$formula <- formula
  this_call$data <- quote(data)


  if (glm_function == "parglm") {
    if (!any(c("nthreads", "control", "method") %in% names(dots))) {
      warning(
        "Argument glm_function = \"parglm\" but no `nthreads`, `method` or `control` specified.\n",
        "Using `control = parglm.control(nthreads = 4, method = \"FAST\")`"
      )
      this_call$control <- parglm::parglm.control(nthreads = 4, method = "FAST")
    }
  }
  this_call[[1]] <- call(glm_function)[[1]]
  for (i in names(dots)) {
    this_call[[i]] <- dots[[i]]
  }
  eval(this_call)
}
