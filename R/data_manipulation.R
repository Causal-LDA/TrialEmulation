#' Data Manipulation Function
#'
#' This function takes the data and all the variables and does the extension preprocessing
#'
#' @param data `data.table` to pre-process for weight calculation and extension.
#' @param use_censor apply censoring due to treatment switch?
#' @keywords internal

data_manipulation <- function(data, use_censor = TRUE) {
  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  time_of_event <- am_1 <- cumA <- regime_start <- time_on_regime <- time_on_regime2 <-
    regime_start_shift <- started0 <- started1 <- stop0 <- stop1 <- eligible0_sw <-
    eligible1_sw <- delete <- eligible0 <- eligible1 <- wt <- after_eligibility <-
    after_event <- id <- period <- eligible <- outcome <- treatment <- NULL

  assert_flag(use_censor)
  len <- nrow(data)
  len_id <- length(unique(data[, id]))

  data[, after_eligibility := period >= .SD[eligible == 1, min(period, Inf)], by = id]
  if (any(data[["after_eligibility"]] == FALSE)) {
    warning("Observations before trial eligibility were removed")
    data <- data[after_eligibility == TRUE]
  }
  data[, after_eligibility := NULL]

  data[, after_event := period > .SD[outcome == 1, min(period, Inf)], by = id]
  if (any(data[["after_event"]] == TRUE)) {
    warning("Observations after the outcome occured were removed")
    data <- data[after_event == FALSE] # keep all which are _not_ after the outcome event
  }
  data[, after_event := NULL]

  # Calculate event time
  event_data <- data[, .SD[.N, list(period, outcome)], by = id]
  event_data[, time_of_event := 9999]
  event_data[(!is.na(outcome) & outcome == 1), time_of_event := as.double(period)]
  event_data <- event_data[, list(id, time_of_event)]

  sw_data <- data[event_data, on = "id"]
  sw_data <- sw_data[, first := !duplicated(id)]
  sw_data <- sw_data[, am_1 := shift(treatment, type = "lag"), by = "id"]
  sw_data[first == TRUE, cumA := 0]
  sw_data[first == TRUE, am_1 := 0]
  sw_data[first == TRUE, switch := 0]
  sw_data[first == TRUE, regime_start := period]
  sw_data[first == TRUE, time_on_regime := 0]

  sw_data[(first == FALSE & am_1 != treatment), switch := 1]
  sw_data[(first == FALSE & am_1 == treatment), switch := 0]

  sw_data[(first == FALSE & switch == 1), regime_start := period]
  sw_data[, regime_start := nafill(regime_start, type = "locf"), by = "id"]

  sw_data[, regime_start_shift := shift(regime_start)]
  sw_data[first == FALSE, time_on_regime := period - as.double(regime_start_shift)]

  sw_data[first == TRUE, cumA := cumA + treatment]
  sw_data[first == FALSE, cumA := treatment]
  sw_data[, cumA := cumsum(cumA), by = id]
  sw_data[, regime_start_shift := NULL]

  if (isTRUE(use_censor)) {
    sw_data[, started0 := NA_real_]
    sw_data[, started1 := NA_real_]
    sw_data[, stop0 := NA_real_]
    sw_data[, stop1 := NA_real_]
    sw_data[, eligible0_sw := NA_real_]
    sw_data[, eligible1_sw := NA_real_]
    sw_data[, delete := NA]
    sw_data <- censor_func(sw_data)
    sw_data <- sw_data[delete == FALSE]
    sw_data[, c("delete", "eligible0_sw", "eligible1_sw", "started0", "started1", "stop0", "stop1") := NULL]
  }

  sw_data[, eligible0 := 0]
  sw_data[, eligible1 := 0]
  sw_data[am_1 == 0, eligible0 := 1]
  sw_data[am_1 == 1, eligible1 := 1]

  setkeyv(sw_data, cols = "id")
  sw_data
}
