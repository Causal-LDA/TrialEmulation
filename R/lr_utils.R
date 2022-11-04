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

#' Weight Logistic Regression Function
#'
#' This function get the information needed for performing Logistic Regression in the weight calculation process
#' using parglm
#'
#' @param data data
#' @param formula model formula for `parglm`
#' @param class_var categorical variables to be converted to factors

weight_lr <- function(data, formula, class_var) {
  if (!missing(class_var) && any(!is.na(class_var))) {
    class_var <- class_var[!is.na(class_var)]

    if (!(is.list(class_var) || is.character(class_var))) stop("outcomeClass is not a list or character vector")
    # class_var given as a character vector, convert to list
    if (is.character(class_var)) class_Var <- as.list(class_var)

    # process the list
    for (i in seq_along(class_var)) {
      # variable name provided only
      if (is.character(class_var[[i]])) {
        this_var <- class_var[[i]]
        set(data, j = this_var, value = as.factor(data[[this_var]]))
      }

      # named list provided
      if (is.list(class_var[[i]])) {
        this_var <- names(class_var[i])
        set(data,
          j = this_var,
          value = do.call("factor", c(x = list(data[[this_var]]), class_var[[this_var]]))
        )
      }
    }
  }

  model <- parglm::parglm(as.formula(formula),
    data = data,
    family = binomial(link = "logit"),
    control = parglm::parglm.control(nthreads = 4, method = "FAST")
  )
  return(model)
}



#' Logistic Regression Function
#'
#' This function get the information needed for performing Logistic Regression for final model
#' @param l A list contains the data with categorical feature determind if needed and logistic regression formula

lr <- function(l) {
  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  weight <- id <- NULL

  d <- l[[1]]
  regf <- l[[2]]

  model <- parglm::parglm(as.formula(regf),
    data = d,
    weights = d[["weight"]],
    family = binomial(link = "logit"),
    control = parglm::parglm.control(nthreads = 4, method = "FAST")
  )

  out <- robust_calculation(model, d[, id])

  return(list(
    model = model,
    output = out
  ))
}
