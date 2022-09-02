#' Data Manipulation Function
#'
#' This function takes the data and all the variables and does the extension preprocessing and weight calculation.
#'
#' @param numCores Number of cores to be used for fitting weights (passed to `weight_func`)
#' @inheritParams initiators

data_manipulation <- function(data,
                              treatment = "treatment", id = "id",
                              period = "period", outcome = "outcome",
                              eligible = "eligible", outcomeCov_var = NA,
                              cov_switchn = NA, model_switchn = NA,
                              class_switchn = NA, cov_switchd = NA,
                              model_switchd = NA, class_switchd = NA,
                              first_period = NA, last_period = NA,
                              use_weight = 0, use_censor = 0, check_missing = 0,
                              cense = NA, pool_cense = 0, cov_censed = NA,
                              model_censed = NA, class_censed = NA,
                              cov_censen = NA, model_censen = NA, class_censen = NA,
                              include_regime_length = 0,
                              eligible_wts_0 = NA, eligible_wts_1 = NA,
                              lag_p_nosw = 1, where_var = NA,
                              data_dir = "~/rds/hpc-work/",
                              numCores = NA,
                              quiet = FALSE) {
  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  time_of_event <- am_1 <- cumA <- regime_start <- time_on_regime <- time_on_regime2 <-
    regime_start_shift <- started0 <- started1 <- stop0 <- stop1 <- eligible0_sw <-
    eligible1_sw <- delete <- eligible0 <- eligible1 <- wt <- after_eligibility <-
    after_event <- NULL


  datatable <- select_data_cols(
    data, id,
    period, treatment, outcome, eligible,
    eligible_wts_0, eligible_wts_1,
    outcomeCov_var,
    cov_switchn, cov_switchd,
    cov_censed, cov_censen, cense, where_var
  )
  len <- nrow(datatable)
  len_id <- length(unique(datatable[, id]))

  datatable[, after_eligibility := period >= .SD[eligible == 1, min(period, Inf)], by = id]
  if (any(datatable[, "after_eligibility"] == FALSE)) {
    warning("Observations before trial eligibility were removed")
    datatable <- datatable[after_eligibility == TRUE]
  }

  datatable[, after_event := period > .SD[outcome == 1, min(period, Inf)], by = id]
  if (any(datatable[, "after_event"] == TRUE)) {
    warning("Observations after the outcome occured were removed")
    datatable <- datatable[after_event == FALSE] # keep all which are _not_ after the outcome event
  }

  # Calculate event time
  event_data <- datatable[, .SD[.N, list(period, outcome)], by = id]
  event_data[, time_of_event := 9999]
  event_data[(!is.na(outcome) & outcome == 1), time_of_event := as.double(period)]
  event_data <- event_data[, list(id, time_of_event)]

  sw_data <- datatable[event_data, on = "id"]

  sw_data <- sw_data[, first := !duplicated(datatable[, id])]
  sw_data <- sw_data[, am_1 := shift(treatment, type = "lag"), by = "id"]
  sw_data[first == TRUE, cumA := 0]
  sw_data[first == TRUE, am_1 := 0]
  sw_data[first == TRUE, switch := 0]
  sw_data[first == TRUE, regime_start := period]
  sw_data[first == TRUE, time_on_regime := 0]
  sw_data[first == TRUE, time_on_regime2 := 0]

  sw_data[(first == FALSE & am_1 != treatment), switch := 1]
  sw_data[(first == FALSE & am_1 == treatment), switch := 0]

  sw_data[(first == FALSE & switch == 1), regime_start := period]
  sw_data[, regime_start := nafill(regime_start, type = "locf"), by = "id"]

  sw_data[, regime_start_shift := shift(regime_start)]
  sw_data[first == FALSE, time_on_regime := period -
    as.double(regime_start_shift)]
  sw_data[first == FALSE, time_on_regime2 := time_on_regime**2]

  sw_data[first == TRUE, cumA := cumA + treatment]
  sw_data[first == FALSE, cumA := treatment]
  sw_data[, cumA := cumsum(cumA), by = id]
  sw_data[, regime_start_shift := NULL]

  if (use_censor == 1) {
    sw_data[, started0 := NA_real_]
    sw_data[, started1 := NA_real_]
    sw_data[, stop0 := NA_real_]
    sw_data[, stop1 := NA_real_]
    sw_data[, eligible0_sw := NA_real_]
    sw_data[, eligible1_sw := NA_real_]
    sw_data[, delete := NA]
    sw_data <- censor_func(sw_data)
    sw_data <- sw_data[delete == FALSE]
  }

  sw_data[, eligible0 := 0]
  sw_data[, eligible1 := 0]
  sw_data[am_1 == 0, eligible0 := 1]
  sw_data[am_1 == 1, eligible1 := 1]

  # for weight = 1
  if (use_weight == 1) {
    sw_data <- weight_func(sw_data, cov_switchn, model_switchn, class_switchn,
      cov_switchd, model_switchd, class_switchd,
      eligible_wts_0, eligible_wts_1,
      cense, pool_cense, cov_censed,
      model_censed, class_censed, cov_censen,
      model_censen, class_censen, include_regime_length,
      numCores, data_dir,
      quiet = quiet
    )
  } else if (use_weight == 0) {
    sw_data[, wt := 1]
  }

  setkeyv(sw_data, cols = "id")
  sw_data
}
