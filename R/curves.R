#' Extract Baseline Observations
#'
#' @param trial_file Path to an expanded trial csv file
#' @param baseline_file Path to csv to save baseline observations
#' @param quiet Don't print progress messages.
#'
#' @return
#' @export
#'
#' @examples
#'
h_extract_baseline <- function(trial_file, baseline_file, quiet = TRUE){
  # Dummy assignments for data.table
  followup_time <- NULL

  if (file.exists(trial_file)){
    h_quiet_print("Extracting baseline observations from ", trial_file)
    fwrite(fread(trial_file)[followup_time == 0,], file = baseline_file)
  }
}

#' Predict Cumulative Incidence
#'
#' @param object The result from `initiators`. Either object or model must be specified.
#' @param model A `glm` object.
#' @param predict_followup a vector of follow up times to predict values for.
#' @param newdata `data.frame` to predict survival for. Extracted from `model`/`object` if not provided.
#'
#' @return a list with estimated cumulative incidence curves for `assigned_treatment`
#' equal to 0 and 1.
#' @export
#' @importFrom stats predict
#'
#' @examples
predict_survival <- function(object, model, predict_followup, newdata) {
  if (missing(model) & missing(object)) stop("Either model or object must be specified.")
  if(missing(model)) model <- object$model
  assert_class(model, "glm")
  assert_numeric(predict_followup, lower = 0, min.len = 1, any.missing = FALSE)

  # Dummy assignments for data.table
  assigned_treatment <- NULL

  if (missing(newdata)) newdata <- as.data.table(model$data)

  baseline <- newdata[newdata$followup_time == 0, ]
  expanded <- baseline[rep(1:nrow(baseline), times = length(predict_followup)), ]
  expanded$followup_time <- rep(predict_followup, each = nrow(baseline))

  pred_0 <- predict(model, newdata = expanded[, assigned_treatment := 0], type = "response")
  pred_1 <- predict(model, newdata = expanded[, assigned_treatment := 1], type = "response")

  preds <- expanded[, c("id", "followup_time", "for_period")]
  preds$pred_0 <- pred_0
  preds$pred_1 <- pred_1
  setorderv(preds, cols = c("for_period", "followup_time", "id"))

  surv <- list(
    pred_0 = lapply(split(preds$pred_0, preds$for_period), matrix, ncol = length(predict_followup)),
    pred_1 = lapply(split(preds$pred_1, preds$for_period), matrix, ncol = length(predict_followup))
  )

  list(
    ci_0 = sum_up_CI(surv$pred_0),
    ci_1 = sum_up_CI(surv$pred_1)
  )
}


#' Helper to Calculate CI
#'
#' @param p_mat_list A list of probability matrices with rows for each subject and followup time as the columns.
#'
#' @return A vector of cumulative incidences.
#' @export
#'
#' @examples
#' surv_prob_list <- list(
#'  trial_1 = matrix(
#'   c(0.1, 0.1, 0.1,
#'    0.5, 0.2, 0.1),
#'   nrow = 2, byrow = TRUE),
#'  trial_2 = matrix(
#'  c(0.15, 0.15, 0.15,
#'   0.45, 0.25, 0.1),
#'   nrow = 2, byrow = TRUE)
#' )
#' sum_up_CI(surv_prob_list)
sum_up_CI <- function(p_mat_list){
  cols <- unqiue(vapply(p_mat_list, ncol, integer(1L)))
  assert_integer(cols, len = 1, lower = 1)

  ci_mat <- vapply(p_mat_list, CI_up_to, numeric(cols))
  total_n <- sum(vapply(p_mat_list, nrow, integer(1L)))
  result <- rowSums(ci_mat) / total_n
  assert_monotonic(result)
  result
}


#' Calculate Cumulative Incidence
#'
#' @param p_mat Probabilty matrix with rows for each subject and followup time as the columns.
#'
#' @return A vector containing the cumulative incidence values.
#' @export
#'
#' @examples
#' surv_prob <- matrix(
#' c(0.1, 0.1, 0.1,
#'   0.5, 0.2, 0.1),
#'   nrow = 2, byrow = TRUE)
#' CI_up_to(surv_prob)
CI_up_to <- function(p_mat){
  assert_matrix(p_mat, mode = "numeric")

  prod_term <- apply(1 - cbind(0, p_mat), 1, cumprod)
  sum_term <- prod_term * t(p_mat)
  cumsum_term <- apply(sum_term, 2, cumsum)
  result <- rowSums(cumsum_term)
  assert_monotonic(result)
  result
}
