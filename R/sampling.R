#' Case-control sampling from extended data in separate trial CSVs
#'
#' @param data_prep Result from [data_preparation()]
#' @param n_control Number of controls to sample per case
#' @param p_control Proportion of controls to select
#' @param subset_condition Expression used to `subset` the trial data before sampling
#' @param sample_all_periods Sample controls in all periods (TRUE) or only when there is a case in the period (FALSE)?
#'
#' @return A `data.frame` or a [split()] `data.frame` if  `length(n_control) > 1` or `length(p_control) > 1`.
#' @export
case_control_sampling_trials <- function(data_prep,
                                         n_control = NULL,
                                         p_control = NULL,
                                         sample_all_periods,
                                         subset_condition) {
  # data.table columns
  .id <- NULL

  trial_files <- data_prep$data
  exists <- vapply(trial_files, test_file_exists, logical(1L))
  if (any(!exists)) {
    warning("Trials files don't exist:", paste(trial_files[!exists], "\n"))
    trial_files <- trial_files[exists]
  }

  if (!is.null(p_control)) {
    n_sample_sets <- length(p_control)
    case_util_fun <- case_util_p
    sampling_value <- p_control
  } else if (!is.null(n_control)) {
    n_sample_sets <- length(n_control)
    case_util_fun <- case_util_n
    sampling_value <- n_control
  }

  if (!missing(subset_condition)) subset_cond <- substitute(subset_condition)
  template <- as.data.table(data_prep$data_template)
  # function used in lapply to read in a trial file and call sampling function
  trial_fun <- function(trial_file) {
    d_period <- rbind(template, fread(input = trial_file))

    if (exists("subset_cond")) d_period <- subset(d_period, eval(subset_cond))

    d <- split(d_period, d_period$followup_time)

    all_samples <- lapply(sampling_value, function(nc) {
      m <- lapply(d, case_util_fun, n_control = nc, p_control = nc, sample_all_periods = sample_all_periods)
      rbindlist(m)
    })

    rbindlist(all_samples, idcol = TRUE)
  }

  trial_samples <- rbindlist(lapply(trial_files, trial_fun))

  if (n_sample_sets > 1) {
    split(trial_samples, by = ".id", keep.by = FALSE)
  } else {
    trial_samples[, .id := NULL]
  }
}


#' Sample cases utility function
#'
#' @param data Data to sample from
#' @param n_control Number of controls for each case
#' @param ... ignored
#'
#' @return A data frame with the cases and sampled controls

case_util_n <- function(data, n_control = 5, ...) {
  ### cases occurred at each period and follow-up visit
  casedatajk <- data[data$outcome == 1, ]
  ### controls (still under follow-up and events haven't occurred) at each period and follow-up visit
  controldatajk <- data[data$outcome == 0, ]
  dataall <- NULL
  ncase <- dim(casedatajk)[1] ## number of cases
  ncontrol <- dim(controldatajk)[1] ## number of potential controls
  if (ncase > 0) {
    if (ncontrol >= n_control * ncase) {
      controlselect <- controldatajk[sample(1:ncontrol, n_control * ncase), ]
    } else {
      warning(paste0(
        ncontrol, ifelse(ncontrol == 1, "control", "controls"),
        " available in this period but ", n_control * ncase, " were requested. All available controls will be used."
      ))
      controlselect <- controldatajk
    }

    # sampling weights
    casedatajk$sample_weight <- rep(1, ncase)
    controlselect$sample_weight <- rep(ncontrol / (ncase * n_control), ncase * n_control)

    dataall <- rbind(casedatajk, controlselect) ## append sampled data
  }
  return(dataall)
}

#' Sample cases utility function
#'
#' @param data Data to sample from
#' @param p_control Proportion of controls to select
#' @param sample_all_periods Sample controls in all periods (TRUE) or only when there is a case in the period (FALSE)?
#' @param ... ignored
#'
#' @return A data frame with the cases and sampled controls
case_util_p <- function(data, p_control = 0.01, sample_all_periods = FALSE, ...) {
  sample_weight <- NULL

  ### cases occurred at each period and follow-up visit
  cases <- which(data$outcome == 1)
  ncase <- length(cases)
  dataall <- NULL

  if (ncase > 0 || sample_all_periods) {
    ### controls (still under follow-up and events haven't occurred) at each period and follow-up visit
    controls <- which(data$outcome == 0)
    n_sample <- ceiling(length(controls) * p_control)
    controlselect <- sample(controls, size = n_sample)
    dataall <- data[c(cases, controlselect), ]
    dataall <- dataall[, sample_weight := c(rep(1, length(cases)), rep(1 / p_control, n_sample))]
  }
  return(dataall)
}
