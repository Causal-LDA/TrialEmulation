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

  baseline <- newdata[newdata$followup_time == 0,]
  expanded <- baseline[rep(1:nrow(baseline), times = length(predict_followup)), ]
  expanded$followup_time <- rep(predict_followup, each = nrow(baseline))

  pred_0 <- predict(model, newdata = expanded[, assigned_treatment := 0], type = "response")
  pred_1 <- predict(model, newdata = expanded[, assigned_treatment := 1], type = "response")
  expanded$pred_0 <- pred_0
  expanded$pred_1 <- pred_1

  preds <- expanded[, c("id", "followup_time", "for_period", "pred_0", "pred_1")]
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

#' Calculate Cumulative Incidence
#'
#' @param p_mat Probabilty matrix with rows for each subject and followup time as the columns.
#' @param t Number of time periods to use. Must not be more than `ncol(p_mat)`
#'
#' @return
#' @export
#'
#' @examples
CI_up_to <- function(p_mat, t = ncol(p_mat)){
  assert_integerish(t, lower = 2, upper = ncol(p_mat))
  assert_matrix(p_mat, mode = "numeric")

  seq <- seq_len(t)
  p_term <- apply(1 - rbind(0, p_mat)[, seq], 1, cumprod)

  p_term <- apply(1-p_mat[, seq_len(t-1)], 1, cumprod)
  prod_term <- rbind(1, p_term)
  sum_term <- prod_term * t(p_mat[, seq_len(t)])
  cumsum_term <- apply(sum_term, 2, cumsum)
  result <- rowSums(cumsum_term)
  assert_monotonic(result)
  result
}


#' Helper to Calculate CI
#'
#' @param p_mat_list A list of probability matrices with rows for each subject and followup time as the columns.
#'
#' @return
#' @export
#'
#' @examples
sum_up_CI <- function(p_mat_list){
  mat <- vapply(p_mat_list, CI_up_to, numeric(ncol(p_mat_list[[1]])))
  total_n <- vapply(p_mat_list, nrow, integer(1L))
  result <- rowSums(mat) / sum(total_n)
  assert_monotonic(result)
  result
}
