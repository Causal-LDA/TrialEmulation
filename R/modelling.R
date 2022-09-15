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
                           run_unweighted_analysis = 0,
                           run_weighted_analysis = 1,
                           run_p99_analysis = 0,
                           run_user_limits_analysis = 0,
                           lower_weight = NA,
                           upper_weight = NA,
                           use_censor = 0,
                           check_missing = 0,
                           include_followup_time_case = ~ followup_time + I(followup_time^2),
                           include_expansion_time_case = ~ for_period + I(for_period^2),
                           where_case = NA,
                           run_base_model = 1,
                           numCores = NA,
                           glm_function = c("parglm", "glm"),
                           use_sample_weights = TRUE,
                           quiet = FALSE) {
  assert_flag(quiet)
  outcome_cov <- as_formula(outcome_cov)
  include_followup_time_case <- as_formula(include_followup_time_case)
  include_expansion_time_case <- as_formula(include_expansion_time_case)
  glm_function <- match.arg(glm_function)

  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  weight <- sample_weight <- followup_time <- NULL

  # if there are any limits on the follow up
  if (!is.na(first_followup) || !is.na(last_followup)) {
    data <- data[followup_time >= max(0, first_followup, na.rm = TRUE) &
      followup_time <= min(Inf, last_followup, na.rm = TRUE), ]
  }

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
    timing <- system.time({
      d <- list()
      for (i in seq_along(where_case)) {
        d[[i]] <- list(data[eval(parse(text = where_case[i]))], model_formula)
      }
      if (numCores == 1) {
        m <- lapply(d, lr)
      } else {
        m <- mclapply(d, lr, mc.cores = numCores)
      }

      for (i in seq_along(where_case)) {
        h_quiet_print(quiet, paste("Analysis with", where_case[i], sep = " "))
        h_quiet_print(quiet, summary(m[i]$model))
        h_quiet_print(quiet, paste("Analysis with", where_case[i], "using robust variance", sep = " "))
        h_quiet_print(quiet, m[i]$output)
      }
    })
    h_quiet_print(quiet, "------------------------------------")
    h_quiet_print(quiet, "Processing time of modeling for where case analysis in total parallel:")
    h_quiet_print(quiet, timing)
  }

  if (run_base_model == 1) {
    if (use_weight == 1) {
      if (run_p99_analysis == 1) {
        data <- p99_weight(data)
      } else if (run_user_limits_analysis == 1) {
        data <- limit_weight(data, lower_weight, upper_weight)
      } else if (run_unweighted_analysis == 1) {
        data[, weight] <- 1
      }
    }

    timing <- system.time({
      if (glm_function == "parglm") {
        model.full <- parglm::parglm(model_formula,
          data = data,
          weights = data[, "weight"],
          family = binomial(link = "logit"),
          control = parglm::parglm.control(nthreads = 4, method = "FAST")
        )
      } else if (glm_function == "glm") {
        model.full <- stats::glm(model_formula,
          data = data,
          weights = data[, weight],
          family = binomial(link = "logit")
        )
      }
    })
    h_quiet_print(quiet, "Base Analysis")
    h_quiet_print(quiet, summary(model.full))
    h_quiet_print(quiet, "-------------------------------------------------")
    h_quiet_print(quiet, "Processing time of modeling for base analysis:")
    h_quiet_print(quiet, timing)

    h_quiet_print(quiet, "Base Analysis with robust variance")
    timing <- system.time({
      h_quiet_print(quiet, "-------------------------------------------------------")
      h_quiet_print(quiet, "Robust standard error:")
      robust_model <- robust_calculation(model.full, data[["id"]])
    })
    h_quiet_print(quiet, robust_model)
    h_quiet_print(quiet, "----------------------------------------------")
    h_quiet_print(quiet, "Processing time of getting the output and sandwich with reduced switch data:")
    h_quiet_print(quiet, timing)
  }

  return(list(model = model.full, robust = robust_model))
}
