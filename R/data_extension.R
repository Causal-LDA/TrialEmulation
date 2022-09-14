#' Data Extension in Parallel Function
#'
#' This function takes the data and all the variables and expand it using parallel computing
#' @param data A `data.table` to be expanded
#' @param keeplist A list contains names of variables used in final model
#' @param outcomeCov_var A list of individual baseline variables used in final model
#' @param first_period First period value to start expanding about
#' @param last_period Last period value to expand about
#' @param use_censor Use censoring for per-protocol analysis - censor person-times once a person-trial stops taking the
#' initial treatment value
#' @param lag_p_nosw when 1 this will set the first weight to be 1 and use `p_nosw_d` and `p_nosw_n`
#'  at follow-up time (t-1) for calculating the weights at follow-up time t - can be set to 0 which will increase
#'  the maximum and variance of weights (Defaults to 1).
#' @param where_var Variables used in where conditions used in subsetting the data used in final analysis (where_case),
#'  the variables not included in the final model
#' @param data_dir Direction to save data
#' @param chunk_size Number of ids to expand in each chunk

data_extension_parallel <- function(data,
                                    keeplist,
                                    outcomeCov_var = NA,
                                    first_period = NA,
                                    last_period = NA,
                                    use_censor = 0,
                                    lag_p_nosw = 1,
                                    where_var = NA,
                                    data_dir = "~/rds/hpc-work/",
                                    chunk_size = 200) {
  maxperiod <- max(data[, "period"])
  minperiod <- min(data[, "period"])

  if (is.na(first_period)) {
    first_period <- minperiod
  }
  if (is.na(last_period)) {
    last_period <- maxperiod
  }
  range <- (maxperiod - minperiod) + 1
  all_ids <- unique(data$id)
  ids_split <- split(all_ids, ceiling(seq_along(all_ids) / chunk_size))
  N <- 0

  for (ids in ids_split) {
    switch_data <- expand(
      sw_data = data[list(ids), ],
      outcomeCov_var = outcomeCov_var,
      where_var = where_var,
      use_censor = use_censor,
      maxperiod = maxperiod,
      minperiod = minperiod,
      lag_p_nosw = lag_p_nosw,
      keeplist = keeplist
    )
    N <- N + nrow(switch_data)
    for (p in unique(switch_data[, "for_period"])[[1]]) {
      fwrite(
        switch_data[for_period == p, ],
        file.path(data_dir, paste0("trial_", p, ".csv")),
        append = TRUE,
        row.names = FALSE
      )
    }
  }

  return(list(
    data = file.path(data_dir, paste0("trial_", first_period:last_period, ".csv")),
    min_period = minperiod,
    max_period = maxperiod,
    range = range,
    N = N,
    data_template = as.data.frame(switch_data[0, ])
  ))
}

#' Data Extension Function
#'
#' Expands the longitudinal data into a sequence of trials.
#'
#' @param data A `data.frame` or similar
#' @param keeplist A list contains names of variables used in final model
#' @param outcomeCov_var A list of individual baseline variables used in final model
#' @param first_period First period value to start expanding about
#' @param last_period Last period value to expand about
#' @param use_censor Use censoring for per-protocol analysis - censor person-times once a person-trial stops
#'  taking the initial treatment value
#' @param lag_p_nosw when 1 this will set the first weight to be 1 and use `p_nosw_d` and `p_nosw_n` at
#' follow-up time (t-1) for calculating the weights at follow-up time t - can be set to 0 which will increase
#'  the maximum and variance of weights (Defaults to 1)
#' @param where_var Variables used in where conditions used in subsetting the data used in final
#'  analysis (`where_case`), the variables are not included in the final model.
data_extension <- function(data, keeplist, outcomeCov_var = NA,
                           first_period = NA, last_period = NA,
                           use_censor = 0,
                           lag_p_nosw = 1, where_var = NA) {
  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  id <- period <- NULL

  assert_data_frame(data)

  max_id <- max(data[, id])
  maxperiod <- max(data[, period])
  minperiod <- min(data[, period])

  if (!is.na(first_period)) {
    minperiod <- first_period
  }
  if (!is.na(last_period)) {
    maxperiod <- last_period
  }
  range <- (maxperiod - minperiod) + 1

  switch_data <- expand(
    data, outcomeCov_var, where_var, use_censor,
    maxperiod, minperiod, lag_p_nosw,
    keeplist
  )

  return(
    list(
      data = switch_data,
      min_period = minperiod,
      max_period = maxperiod,
      range = range,
      N = nrow(switch_data)
    )
  )
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
#' @param lag_p_nosw when 1 this will set the first weight to be 1 and use p_nosw_d and p_nosw_n at followup-time (t-1)
#' for calculating the weights at followup-time t - can be set to 0 which will increase the maximum and variance of
#' weights (Defaults to 1)
#' @param keeplist A list contains names of variables used in final model
#'
#' @import data.table

expand <- function(sw_data,
                   outcomeCov_var,
                   where_var,
                   use_censor,
                   maxperiod,
                   minperiod,
                   lag_p_nosw,
                   keeplist) {
  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  id <- period <- wtprod <- elgcount <- treat <- dosesum <- eligible <- treatment <- treatment_new <-
    weight0 <- wt <- cumA <- init <- init_shift <- period_new <- cumA_new <- switch_new <-
    outcome_new <- outcome <- time_of_event <- for_period <- index <-
    followup_time <- dose <- weight <- case <- NULL

  temp_data <- data.table(
    id = sw_data[, id],
    period = sw_data[, period],
    switch = sw_data[, switch]
  )
  temp_data[, wtprod := 1.0, by = id][, elgcount := 0.0, by = id][, expand := 0.0, by = id]
  temp_data[, treat := 0.0, by = id][, dosesum := 0.0, by = id]
  temp_data[(sw_data[, eligible] == 1 & !is.na(sw_data[, treatment])),
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

  switch_data <- data.table(id = sw_data[, id])
  switch_data <- switch_data[rep(1:.N, sw_data[, period] + 1)]
  switch_data[, period_new := sw_data[rep(1:.N, period + 1), period]]
  switch_data[, cumA_new := sw_data[rep(1:.N, period + 1), cumA]]
  switch_data[, treatment_new := shift(sw_data[rep(1:.N, period + 1), treatment])]
  switch_data[1, "treatment_new"] <- sw_data[1, treatment]
  if (use_censor == 1) {
    switch_data[, switch_new := sw_data[rep(1:.N, period + 1), switch]]
  } else {
    switch_data[, switch_new := 0]
  }
  switch_data[, outcome_new := sw_data[rep(1:.N, period + 1), outcome]]
  switch_data[, time_of_event := sw_data[rep(1:.N, period + 1), time_of_event]]
  switch_data[, weight0 := sw_data[rep(1:.N, period + 1), weight0]]
  switch_data[, for_period := for_period_func(sw_data)]
  switch_data[, index := seq_len(.N)]
  switch_data <- switch_data[temp_data, on = list(id = id, for_period = period)]
  setorder(switch_data, index)

  switch_data[, followup_time := period_new - for_period]

  if (use_censor == 0) {
    switch_data[, dose := cumA_new - dosesum + treat]
  } else {
    switch_data[, treatment := init]
  }

  switch_data[expand == 1, expand := expand_func(.SD, maxperiod, minperiod), by = id]

  if (lag_p_nosw == 1) {
    switch_data[, weight := (weight0 / wtprod)]
  } else {
    switch_data[for_period == minperiod, weight := weight0]
    wtprod_shift <- shift(switch_data[, wtprod])
    switch_data[for_period != 0, weight := (weight0 / wtprod_shift)]
  }
  switch_data[, case := 0]
  if (use_censor == 0) {
    switch_data[(time_of_event == period_new & outcome_new == 1), case := 1]
  } else {
    switch_data[switch_new == 1, case := as.numeric(NA)]
    switch_data[(switch_new == 0 &
      time_of_event == period_new & outcome_new == 1), case := 1]
  }

  setnames(switch_data, c("case"), c("outcome"))
  setnames(switch_data, c("init"), c("assigned_treatment"))
  setnames(switch_data, c("treatment_new"), c("treatment"))
  switch_data <- switch_data[expand == 1]
  switch_data <- switch_data[, keeplist, with = FALSE]

  switch_data
}
