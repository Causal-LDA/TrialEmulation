#' Data Extension in Parallel Function
#'
#' This function takes the data and all the variables and expand it using parallel computing
#' @param data_address Address for data read with `bigmemory`
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
#' @param followup_spline The spline model for `followup_time` when `include_followup_time_case = "spline"`
#' @param period_spline The spline model for `for_period` when `include_expansion_time_case = "spline"`
#' @param data_dir Direction to save data
#' @param numCores Number of cores for parallel programming
#' @param chunk_size Number of ids to expand in each chunk
#' @param separate_files Write to one file or one per trial (default FALSE)

data_extension_parallel <- function(data_address, keeplist, outcomeCov_var = NA,
                                    first_period = NA, last_period = NA,
                                    use_censor = 0,
                                    lag_p_nosw = 1, where_var = NA,
                                    followup_spline = NA, period_spline = NA,
                                    data_dir = "~/rds/hpc-work/",
                                    numCores = NA,
                                    chunk_size = 200,
                                    separate_files = FALSE) {
  maxperiod <- max(data_address[, "period"])
  minperiod <- min(data_address[, "period"])

  if (is.na(first_period)) {
    first_period <- minperiod
  }
  if (is.na(last_period)) {
    last_period <- maxperiod
  }
  range <- (maxperiod - minperiod) + 1

  if (bigmemory::is.big.matrix(data_address)) {
    all_ids <- unique(data_address[, "id"])
  } else if (is.data.frame(data_address)) {
    all_ids <- unique(data_address$id)
  } else {
    stop("Unknown data_address object!")
  }

  j <- split(all_ids, ceiling(seq_along(all_ids) / chunk_size))

  N <- mclapply(j, expand_switch,
    data_address = data_address,
    outcomeCov_var = outcomeCov_var, where_var = where_var,
    use_censor = use_censor, followup_spline = followup_spline,
    period_spline = period_spline,
    maxperiod = maxperiod, minperiod = minperiod,
    lag_p_nosw = lag_p_nosw, keeplist = keeplist, data_dir = data_dir,
    mc.cores = numCores, separate_files = separate_files
  )
  gc()

  return(list(
    min_period = minperiod,
    max_period = maxperiod,
    range = range,
    N = sum(unlist(as.numeric(N))),
    path = ifelse(separate_files,
      file.path(data_dir, paste0("trial_", first_period:last_period, ".csv")),
      file.path(data_dir, "switch_data.csv")
    )
  ))
}

#' Data Extension Function
#'
#' Expands the longitudinal data into a sequence of trials.
#'
#' @param sw_data A `data.frame` or similar
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
#' @param followup_spline The spline model for follow-up time when `include_followup_time_case = "spline"`
#' @param period_spline The spline model for `for_period` when choose `include_expansion_time_case = "spline"`
#' @param data_dir Direction to save data
#' @param separate_files Write to one file or one per trial (default `FALSE`)
#' data_extension()

data_extension <- function(sw_data, keeplist, outcomeCov_var = NA,
                           first_period = NA, last_period = NA,
                           use_censor = 0,
                           lag_p_nosw = 1, where_var = NA,
                           followup_spline = NA, period_spline = NA,
                           data_dir = "~/rds/hpc-work/", separate_files = FALSE) {
  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  id <- period <- NULL

  assert_data_frame(sw_data)

  max_id <- max(sw_data[, id])
  maxperiod <- max(sw_data[, period])
  minperiod <- min(sw_data[, period])

  if (!is.na(first_period)) {
    minperiod <- first_period
  }
  if (!is.na(last_period)) {
    maxperiod <- last_period
  }
  range <- (maxperiod - minperiod) + 1

  n_expanded <- expand(
    sw_data, outcomeCov_var, where_var, use_censor,
    followup_spline, period_spline,
    maxperiod, minperiod, lag_p_nosw,
    keeplist, data_dir, separate_files
  )

  return(
    list(
      min_period = minperiod,
      max_period = maxperiod,
      range = range,
      N = n_expanded,
      path = file.path(data_dir, "switch_data.csv")
    )
  )
}
