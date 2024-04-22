#' Data Extension
#'
#' Expands the longitudinal data into a sequence of trials.
#'
#' @param data A `data.table` to be expanded
#' @param keeplist A list contains names of variables used in final model
#' @param outcome_cov A formula for covariate adjustment of the outcome model
#' @param first_period First period value to start expanding about
#' @param last_period Last period value to expand about
#' @param censor_at_switch Use censoring for per-protocol analysis - censor person-times once a person-trial
#' stops taking the initial treatment value
#' @param where_var Variables used in where conditions used in subsetting the data used in final analysis (where_case),
#'  the variables not included in the final model
#' @param data_dir Directory to save data
#' @param separate_files Save expanded data in separate CSV files for each trial.
#' @param chunk_size Number of ids to expand in each chunk
#' @noRd

data_extension <- function(data,
                           keeplist,
                           outcome_cov = NA,
                           first_period = NA,
                           last_period = NA,
                           censor_at_switch = FALSE,
                           where_var = NA,
                           data_dir,
                           separate_files = FALSE,
                           chunk_size = 200) {
  # data.table notes:
  trial_period <- NULL

  if (isTRUE(separate_files)) assert_directory_exists(data_dir)
  if (is.na(first_period)) first_period <- min(data[["period"]])
  if (is.na(last_period)) last_period <- max(data[["period"]])
  outcomeCov_var <- all.vars(outcome_cov)

  if (isTRUE(separate_files)) {
    all_ids <- unique(data$id)
    ids_split <- split(all_ids, ceiling(seq_along(all_ids) / chunk_size))
    N <- 0
    for (ids in ids_split) {
      switch_data <- expand(
        sw_data = data[list(ids), ],
        outcomeCov_var = outcomeCov_var,
        where_var = where_var,
        use_censor = censor_at_switch,
        minperiod = first_period,
        maxperiod = last_period,
        keeplist = keeplist
      )
      N <- N + nrow(switch_data)
      for (p in unique(switch_data[["trial_period"]])) {
        file_p <- file.path(data_dir, paste0("trial_", p, ".csv"))
        fwrite(switch_data[trial_period == p, ], file = file_p, append = TRUE)
      }
    }
    files <- file.path(data_dir, paste0("trial_", first_period:last_period, ".csv"))
    list(
      data = files[file.exists(files)],
      min_period = first_period,
      max_period = last_period,
      N = N,
      data_template = as.data.frame(switch_data[0, ])
    )
  } else {
    switch_data <- expand(
      sw_data = data,
      outcomeCov_var = outcomeCov_var,
      where_var = where_var,
      use_censor = censor_at_switch,
      minperiod = first_period,
      maxperiod = last_period,
      keeplist = keeplist
    )
    list(
      data = switch_data,
      min_period = first_period,
      max_period = last_period,
      N = nrow(switch_data),
      data_template = as.data.frame(switch_data[0, ])
    )
  }
}


#' Expand Function
#'
#' This function performs the data expansion for a given dataset
#'
#' @param sw_data datatable to expand
#' @param outcomeCov_var A list of individual baseline variables used in final model
#' @param where_var Variables used in where conditions used in subsetting the data used in final analysis (where_case),
#' the variables not included in the final model
#' @param use_censor Use censoring for per-protocol analysis - censor person-times once a person-trial stops taking the
#' initial treatment value
#' @param maxperiod Maximum period
#' @param minperiod Minimum period
#' @param keeplist A list contains names of variables used in final model
#'
#' @import data.table
#' @keywords internal

expand <- function(sw_data,
                   outcomeCov_var,
                   where_var,
                   use_censor,
                   maxperiod,
                   minperiod,
                   keeplist) {
  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  id <- period <- wtprod <- elgcount <- treat <- dosesum <- eligible <- treatment <- treatment_new <-
    weight0 <- wt <- cumA <- init <- init_shift <- period_new <- cumA_new <- switch_new <-
    outcome_new <- outcome <- time_of_event <- trial_period <- index <-
    followup_time <- dose <- weight <- case <- NULL

  temp_data <- data.table(
    id = sw_data[, id],
    period = sw_data[, period],
    switch = sw_data[, switch]
  )
  temp_data[, wtprod := 1.0, by = id][, elgcount := 0.0, by = id][, expand := 0.0, by = id]
  temp_data[, treat := 0.0, by = id][, dosesum := 0.0, by = id]
  temp_data[
    (sw_data[, eligible] == 1 & !is.na(sw_data[, treatment])) &
      minperiod <= period & period <= maxperiod,
    expand := 1,
    by = id
  ]
  sw_data[first == TRUE, weight0 := 1.0]
  sw_data[, weight0 := cumprod(wt), by = id]
  temp_data[, wtprod := sw_data[, weight0]]
  temp_data[, treat := sw_data[, treatment]]
  temp_data[, dosesum := sw_data[, cumA]]
  temp_data[, elgcount := sw_data[, eligible]]
  temp_data[sw_data[, eligible] == 1, init := sw_data[eligible == 1, treatment]]
  temp_data[, init_shift := shift(sw_data[, treatment])]
  temp_data[sw_data[, eligible] == 0, init := init_shift, by = id]
  temp_data[, init_shift := NULL]

  if (any(!is.na(outcomeCov_var))) {
    tryCatch({
      suppressWarnings(temp_data[, eval(outcomeCov_var) := sw_data[, outcomeCov_var, with = FALSE]])
    })
  }
  if (any(!is.na(where_var))) {
    temp_data[, eval(where_var) := sw_data[, where_var, with = FALSE]]
  }

  expand_index <- rep(seq_len(nrow(sw_data)), sw_data[, period] + 1)

  switch_data <- data.table(id = sw_data[expand_index, id])
  switch_data[, period_new := sw_data[expand_index, period]]
  switch_data[, cumA_new := sw_data[expand_index, cumA]]
  switch_data[, treatment_new := sw_data[expand_index, treatment]]
  if (isTRUE(use_censor)) {
    switch_data[, switch_new := sw_data[expand_index, switch]]
  } else {
    switch_data[, switch_new := 0]
  }
  switch_data[, outcome_new := sw_data[expand_index, outcome]]
  switch_data[, time_of_event := sw_data[expand_index, time_of_event]]
  switch_data[, weight0 := sw_data[expand_index, weight0]]
  switch_data[, trial_period := sequence(sw_data[["period"]] + 1, from = 0)]
  switch_data[, index := seq_len(.N)]

  switch_data <- switch_data[temp_data, on = list(id = id, trial_period = period)]
  setorder(switch_data, index)

  switch_data[, followup_time := period_new - trial_period]

  if ("dose" %in% keeplist) {
    switch_data[, dose := cumA_new - dosesum + treat]
  }

  switch_data[followup_time == 0, switch_new := 0]
  switch_data[expand == 1,
    expand := expand_until_switch(switch_new, .N),
    by = c("id", "trial_period")
  ]

  switch_data[, weight := (weight0 / wtprod)]

  switch_data[, case := 0]
  if (isFALSE(use_censor)) {
    switch_data[(time_of_event == period_new & outcome_new == 1), case := 1]
  } else {
    switch_data[switch_new == 1, case := as.numeric(NA)]
    switch_data[(switch_new == 0 & time_of_event == period_new & outcome_new == 1), case := 1]
  }

  setnames(switch_data, old = c("case"), new = c("outcome"))
  setnames(switch_data, old = c("init"), new = c("assigned_treatment"))
  setnames(switch_data, old = c("treatment_new"), new = c("treatment"))
  switch_data <- switch_data[expand == 1]
  switch_data <- switch_data[, keeplist, with = FALSE]

  switch_data
}

#' Check Expand Flag After Treatment Switch
#'
#' Check if patients have switched treatment in eligible trials
#' and set `expand = 0`.
#' @param s numeric vector where `1` indicates a treatment switch in that period
#' @param n length of s
#' @return Vector of indicator values up until first switch.
#' @keywords internal
expand_until_switch <- function(s, n) {
  first_switch <- match(1, s)
  if (!is.na(first_switch)) {
    rep(c(1, 0), times = c(first_switch - 1, n - first_switch + 1))
  } else {
    rep(1, n)
  }
}
