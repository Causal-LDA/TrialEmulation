#' P99 Weights Function
#'
#' This function truncate the weights of data at the 1st and 99th percentile
#' @param weight The data.table weight column
#' @noRd
p99_weight <- function(weight) {
  p99 <- quantile(weight, prob = c(0.01, 0.99), type = 1)
  limit_weight(weight, p99[1], p99[2])
}

#' Limit Weights Function
#'
#' This function truncate the weights using user defined limits
#' @param weight The data.table weight column
#' @param lower_limit The user defined minimum possible weight
#' @param upper_limit The user defined maximum possible weight
#' @noRd
limit_weight <- function(weight, lower_limit, upper_limit) {
  weight[weight > upper_limit] <- upper_limit
  weight[weight < lower_limit] <- lower_limit
  weight
}


#' Fit a GLM
#'
#' @param formula the model formula.
#' @param data a data frame or similar.
#' @param ... Other arguments for glm
#' @param glm_function The glm function to call given as a string, such as `"glm"` or `"parglm"`.
#' @noRd
#' @import parglm
#'
#' @details
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
