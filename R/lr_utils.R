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


#' Title
#'
#' @param formula
#' @param data
#' @param family
#' @param ...
#' @param glm_function
#' @noRd
#' @import parglm
fit_glm <- function(formula, data, family = binomial(link = "logit"), ..., glm_function = "glm") {
  this_call <- match.call(expand.dots = TRUE)

  if (glm_function == "parglm") {
    # nthreads_arg <- ...elt(which(...names() == "nthreads"))
    # control_arg <- ...elt(which(...names() == "control"))
    # if (is.null(this_call$nthreads) && is.null(this_call$control$nthreads)) {
    #   warning(
    #   "Argument glm_function = \"parglm\" but no `nthreads` specified.\n",
    #   "Using default `control = parglm.control(nthreads = 4, method = \"FAST\")`"
    #   )
    this_call$control <- parglm::parglm.control(nthreads = 4, method = "FAST")
    # }
  }
  # browser()
  this_call$glm_function <- NULL
  this_call$formula <- formula
  this_call$data <- quote(data)
  this_call[[1]] <- call(glm_function)[[1]]

  eval(this_call)
}

#'
#' #' Weight Logistic Regression Function
#' #'
#' #' This function get the information needed for performing Logistic Regression in the weight calculation process
#' #' using parglm
#' #'
#' #' @param data data
#' #' @param formula model formula for `parglm`
#'
#' weight_lr <- function(data, formula) {
#'   model <- parglm::parglm(as.formula(formula),
#'     data = data,
#'     family = binomial(link = "logit"),
#'     control = parglm::parglm.control(nthreads = 4, method = "FAST")
#'   )
#'   return(model)
#' }
#'
#'
#'
#' #' Logistic Regression Function
#' #'
#' #' This function get the information needed for performing Logistic Regression for final model
#' #' @param l A list contains the data and logistic regression formula
#' #'
#' lr <- function(l) {
#'   # Dummy variables used in data.table calls declared to prevent package check NOTES:
#'   weight <- id <- NULL
#'
#'   d <- l[[1]]
#'   regf <- l[[2]]
#'
#'   model <- parglm::parglm(as.formula(regf),
#'     data = d,
#'     weights = d[["weight"]],
#'     family = binomial(link = "logit"),
#'     control = parglm::parglm.control(nthreads = 4, method = "FAST")
#'   )
#'
#'   out <- robust_calculation(model, d[, id])
#'
#'   return(list(
#'     model = model,
#'     output = out
#'   ))
#' }
