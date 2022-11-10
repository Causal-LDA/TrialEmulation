#' Data modelling Function
#'
#' This function do the modelling.
#' @param use_sample_weights Use sample weights in addition to IP weights. `data` must contain a column `sample_weight`.
#' The weights used in the model are calculated as `weight = weight * sample_weight`.
#' @inheritParams initiators
#'
#' @details The class variables parameters (`outcomeClass`,`class_switchn`,
#'  `class_switchd`,`class_censen`,`class_censed`) can be given as a character
#'  vector which will construct factors using `as.factor` or as a named list
#'  with the arguments for factor e.g.
#'  `list(risk_cat=list(levels = c(1,2,3,0), age_cat=list(levels=c(1,2,3),labels=c("50-60","60-70","70+")`
#'
#' @export
#' @importFrom stats as.formula binomial pnorm quantile relevel
#' @importFrom utils write.csv

data_modelling <- function(data,
                           outcome_cov = ~1,
                           model_var = NA,
                           first_followup = NA,
                           last_followup = NA,
                           use_weight = 0,
                           analysis_weights = c("asis", "unweighted", "p99", "weight_limits"),
                           weight_limits = c(0, Inf),
                           use_censor = 0,
                           check_missing = 0,
                           include_followup_time_case = ~ followup_time + I(followup_time^2),
                           include_expansion_time_case = ~ for_period + I(for_period^2),
                           where_case = NA,
                           glm_function = c("parglm", "glm"),
                           use_sample_weights = TRUE,
                           quiet = FALSE) {
  assert_flag(quiet)
  outcome_cov <- as_formula(outcome_cov)
  include_followup_time_case <- as_formula(include_followup_time_case)
  include_expansion_time_case <- as_formula(include_expansion_time_case)
  glm_function <- match.arg(glm_function)
  analysis_weights <- match.arg(analysis_weights)
  assert_numeric(weight_limits, len = 2, lower = 0, upper = Inf)

  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  weight <- sample_weight <- followup_time <- NULL

  # if there are any limits on the follow up
  if (!is.na(first_followup) || !is.na(last_followup)) {
    data <- data[followup_time >= max(0, first_followup, na.rm = TRUE) &
      followup_time <= min(Inf, last_followup, na.rm = TRUE), ]
  }

  quiet_msg(quiet, "Preparing for model fitting")

  # adjust weights if necessary
  if (use_sample_weights) {
    if (!"sample_weight" %in% colnames(data)) {
      warning("'sample_weight' column not found in data. Using sample weights = 1.")
      data[, weight := weight]
    } else {
      data[, weight := weight * sample_weight]
    }
  }

  model_formula <- outcome ~ 1

  if (!is.null(model_var)) {
    # if the model_var is not empty, we use the information provided by user
    model_formula <- add_rhs(model_formula, as_formula(model_var))
  } else {
    # if the model_var is empty, we provide the needed variables based on analysis type
    if (use_censor == 0) {
      if (use_weight == 0) {
        # for ITT analysis
        model_formula <- add_rhs(model_formula, ~assigned_treatment)
      } else {
        # for as treated analysis
        model_formula <- add_rhs(model_formula, ~ dose + I(dose^2))
      }
    } else {
      # for per-protocol analysis
      model_formula <- add_rhs(model_formula, ~assigned_treatment)
    }
  }

  model_formula <- Reduce(
    add_rhs,
    c(model_formula, include_expansion_time_case, include_followup_time_case, outcome_cov)
  )

  if (any(!is.na(where_case))) {
    quiet_msg(quiet, paste("Subsetting data with", toString(where_case), sep = " "))
    for (i in seq_along(where_case)) {
      data <- data[eval(parse(text = where_case[i])), ]
    }
  }

  if (analysis_weights == "asis") {
    # no change
  } else if (analysis_weights == "p99") {
    data <- p99_weight(data)
  } else if (analysis_weights == "weight_limits") {
    data <- limit_weight(data, weight_limits[1], weight_limits[2])
  } else if (analysis_weights == "unweighted") {
    data[["weight"]] <- 1
  }
  if (use_weight == 0) data[["weight"]] <- 1

  quiet_line(quiet)
  quiet_msg(quiet, "Fitting outcome model")
  timing <- system.time({
    if (glm_function == "parglm") {
      model.full <- parglm::parglm(model_formula,
        data = data,
        weights = data[["weight"]],
        family = binomial(link = "logit"),
        control = parglm::parglm.control(nthreads = 4, method = "FAST")
      )
    } else if (glm_function == "glm") {
      model.full <- stats::glm(model_formula,
        data = data,
        weights = data[["weight"]],
        family = binomial(link = "logit")
      )
    }
  })

  quiet_msg_time(quiet, "Processing time of fitting outcome model: ", timing)
  quiet_msg(quiet, "summary(model)")
  quiet_print(quiet, summary(model.full))
  quiet_line(quiet)

  quiet_msg(quiet, "Calculating robust variance")
  timing <- system.time({
    robust_model <- robust_calculation(model.full, data[["id"]])
  })
  quiet_msg_time(quiet, "Processing time of calculating robust variance: ", timing)
  quiet_msg(quiet, "Summary with robust standard error:")
  quiet_print(quiet, format.data.frame(robust_model$summary, digits = 3))
  quiet_line(quiet)

  result <- list(model = model.full, robust = robust_model)
  class(result) <- "RTE_model"
  result
}
