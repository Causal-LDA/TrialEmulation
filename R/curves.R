#' Extract Baseline Observations
#'
#' @param trial_file Path to an expanded trial csv file
#' @param baseline_file Path to csv to save baseline observations
#' @param quiet Don't print progress messages.
#'
#' @details
#' Reads `trial_file` and saves the observations with `followup_time == 0` to `baseline_file` csv.
#'
#' @export
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
#' @param predict_times a vector of follow up times to predict values for.
#' @param newdata `data.frame` to predict survival for. Extracted from `model`/`object` if not provided.
#'
#' @return a list with estimated cumulative incidence curves for `assigned_treatment`
#' equal to 0 and 1.
#' @export
#' @importFrom stats predict
#'
#' @examples
#' \donttest{
#' data("trial_example")
#' working_dir <- file.path(tempdir(),"trial_emu")
#' dir.create(working_dir)
#' data_path <- file.path(working_dir, "trial_example.csv")
#' write.csv(trial_example, file=data_path)
#'
#' i <- initiators(
#'   data_path = data_path,
#'   id="id",
#'   period = "period",
#'   eligible = "eligible",
#'   treatment = "treatment",
#'   outcome = "outcome",
#'   model_var = "assigned_treatment",
#'   outcomeCov_var = c("catvarA", "catvarB", "catvarC","nvarA","nvarB","nvarC"),
#'   outcomeClass = c("catvarA", "catvarB", "catvarC"),
#'   numCores = 1,
#'   data_dir = working_dir,
#'   use_censor = 0,
#'   use_weight = 0
#' )
#'
#' predict_survival(i, predict_times = c(1,2,3,4,5))
#' }
#'
predict_survival <- function(object, model, predict_times, newdata) {
  if (missing(model) & missing(object)) stop("Either model or object must be specified.")
  if(missing(model)) model <- object$model
  assert_class(model, "glm")
  assert_numeric(predict_times, lower = 0, min.len = 1, any.missing = FALSE)

  # Dummy assignments for data.table
  assigned_treatment <- NULL

  if (missing(newdata)) newdata <- as.data.table(model$data)

  baseline <- newdata[newdata$followup_time == 0, ]
  expanded <- baseline[rep(1:nrow(baseline), times = length(predict_times)), ]
  expanded$followup_time <- rep(predict_times, each = nrow(baseline))

  pred_0 <- predict(model, newdata = expanded[, assigned_treatment := 0], type = "response")
  pred_1 <- predict(model, newdata = expanded[, assigned_treatment := 1], type = "response")

  preds <- expanded[, c("id", "followup_time", "for_period")]
  preds$pred_0 <- pred_0
  preds$pred_1 <- pred_1
  setorderv(preds, cols = c("for_period", "followup_time", "id"))

  pred_trt <- list(
    trt_0 = tapply(preds$pred_0, preds$for_period, matrix, ncol = length(predict_times)),
    trt_1 = tapply(preds$pred_1, preds$for_period, matrix, ncol = length(predict_times))
  )

  lapply(pred_trt, sum_up_ci)
}


#' Helper to Calculate CI
#'
#' @param p_mat_list A list of probability matrices with rows for each subject and followup time as the columns.
#' All matrices must have the followup times (i.e. same number of columns).
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
#' sum_up_ci(surv_prob_list)
sum_up_ci <- function(p_mat_list){
  cols <- unique(vapply(p_mat_list, ncol, integer(1L)))
  assert_integer(cols, len = 1, lower = 1)

  ci_mat <- vapply(p_mat_list, ci_up_to, numeric(cols))
  total_n <- sum(vapply(p_mat_list, nrow, integer(1L)))
  result <- rowSums(ci_mat) / total_n
  assert_monotonic(result)
  result
}


#' Calculate Cumulative Incidence and Survival
#'
#' @param p_mat Probability matrix with rows for each subject and followup time as the columns.
#'
#' @return A vector containing the cumulative incidence or survival values.
#' @export
#'
#' @examples
#' surv_prob <- matrix(
#'  c(0.1, 0.1, 0.1,
#'    0.5, 0.2, 0.1),
#'  nrow = 2,
#'  byrow = TRUE)
#' ci_up_to(surv_prob)
ci_up_to <- function(p_mat){
  assert_matrix(p_mat, mode = "numeric")

  prod_term <- apply(1 - cbind(0, p_mat)[, -ncol(p_mat)], 1, cumprod)
  sum_term <- prod_term * t(p_mat)
  cumsum_term <- apply(sum_term, 2, cumsum)
  result <- rowSums(cumsum_term)
  assert_monotonic(result)
  result
}

#' @rdname ci_up_to
#' @export
survival_up_to <- function(p_mat){
  assert_matrix(p_mat, mode = "numeric")
  result <- c(1, rowMeans(apply(1 - p_mat, 1, cumprod)))
  assert_monotonic(result, increasing = FALSE)
  result
}
