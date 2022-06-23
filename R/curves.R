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
  if (file.exists(trial_file)){
    h_quiet_print("Extracting baseline observations from ", trial_file)
    fwrite(fread(trial_file)[followup_time == 0,], file = baseline_file)
  }
}

#' Predict Cumulative Incidence
#'
#' @param object the result from `initiators` or a `glm` object.
#' @param predict_followup a vector of follow up times to predict values for.
#'
#' @return a list with estimated cumulative incidence curves for `assigned_treatment`
#' equal to 0 and 1.
#' @export
#' @importFrom stats predict
#'
#' @examples
predict_survival <- function(model, predict_followup) {
  assert_class(model, "glm")
  assert_numeric(predict_followup, lower = 0, min.len = 1, any.missing = FALSE)

  baseline <- model$data[model$data$followup_time == 0,]
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

#
# ps <- predict_survival(result$model, c(1:5))
# ps[, cumprod := cumprod(1-predicted), by = c("id", "for_period")]
# library(dplyr)
# ps %>% arrange(id, for_period, followup_time)
#
# library(ggplot2)
# ggplot(ps, aes(x = followup_time, y = cumprod, group = interaction(id, for_period))) + geom_line()


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
#' @param p_mat
#'
#' @return
#' @export
#'
#' @examples
sum_up_CI <- function(p_mat){
  mat <- vapply(p_mat, CI_up_to, numeric(ncol(p_mat[[1]])))
  total_n <- vapply(p_mat, nrow, integer(1L))
  result <- rowSums(mat) / sum(total_n)
  assert_monotonic(result)
  result
}



#' Calculate Cumulative Incidence for Initiators Results
#'
#' @param object result of `initiators()`
#'
#' @return Cumulative incidence curves for the two treatment arms.
#' @export
#'
#' @examples
initiators_ci <- function(object, follow_up) {
  if (missing(follow_up)) {
    follow_up <- seq_len(max(object$model$data$followup_time))
  }
  surv <- predict_survival(object$model, follow_up)
  list(
    ci_0 = sum_up_CI(surv$pred_0),
    ci_1 = sum_up_CI(surv$pred_1)
  )
}

