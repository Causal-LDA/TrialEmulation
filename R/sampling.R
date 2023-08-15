#' Case-control sampling from extended data
#'
#' @param data_prep Result from [data_preparation()].
#' @param p_control Proportion of controls to select at each follow-up time of each trial.
#' @param subset_condition Expression used to [subset()] the trial data before sampling.
#' @param sort Sort data before sampling. This ensures results are identical between data prepared with
#' `separate_files` TRUE and FALSE.
#'
#' @return A `data.frame` or a [split()] `data.frame` if  `length(p_control) > 1`. An additional column
#' containing sample weights will be added to the result. These can be included in the models fit with
#' [trial_msm()].
#' @export
#' @examples
#' dat <- trial_example[trial_example$id < 200, ]
#' expanded_data <- data_preparation(
#'   data = dat,
#'   outcome_cov = c("nvarA", "nvarB", "nvarC"),
#'   first_period = 260,
#'   last_period = 280
#' )
#' samples <- case_control_sampling_trials(expanded_data, p_control = 0.01)
case_control_sampling_trials <- function(data_prep,
                                         p_control = NULL,
                                         subset_condition,
                                         sort = FALSE) {
  assert_numeric(p_control, lower = 0, upper = 1, min.len = 1)

  if (use_subset <- !missing(subset_condition)) {
    subset_expr <- substitute(subset_condition)
  }


  args <- alist(data_prep, p_control, use_subset, subset_expr, sort)
  if (inherits(data_prep, "TE_data_prep_sep")) {
    trial_samples <- do.call(sample_data_prep_sep, args)
  } else if (inherits(data_prep, "TE_data_prep_dt")) {
    trial_samples <- do.call(sample_data_prep_dt, args)
  } else {
    stop("Unknown data_prep object.")
  }

  if (length(p_control) > 1) {
    split(rbindlist(trial_samples), by = "sample_id", keep.by = FALSE)
  } else {
    set(rbindlist(trial_samples), j = "sample_id", value = NULL)
  }
}

# Sample utility for data_preparation result with separate_files = TRUE
sample_data_prep_sep <- function(data_prep,
                                 p_control,
                                 use_subset,
                                 subset_expr,
                                 sort) {
  trial_files <- data_prep$data
  exists <- vapply(trial_files, test_file_exists, logical(1L))
  if (any(!exists)) {
    if (all(!exists)) stop("No trial files could be found.")
    warning("Trials files don't exist:", paste(trial_files[!exists], "\n"))
    trial_files <- trial_files[exists]
  }
  template <- as.data.table(data_prep$data_template)

  lapply(trial_files, function(trial_file) {
    period_data <- rbind(template, fread(input = trial_file))
    sample_from_period(period_data, p_control, use_subset, subset_expr, sort)
  })
}

# Sample utility for data_preparation result with separate_files = TRUE
sample_data_prep_dt <- function(data_prep,
                                p_control,
                                use_subset,
                                subset_expr,
                                sort) {
  periods <- sort(unique(data_prep$data$trial_period))
  lapply(periods, function(t) {
    sample_from_period(
      data_prep$data[data_prep$data$trial_period == t, ],
      p_control,
      use_subset,
      subset_expr,
      sort
    )
  })
}

#' Sample from Period
#'
#' Break period data into follow up periods and sample.
#'
#' @param period_data A data.frame
#' @param p_control Vector of proportions of controls to sample
#' @param use_subset TRUE/FALSE
#' @param subset_expr if `use_subset` is `TRUE` an expression
#' @param sort Sort data before subsetting for reproducibility.
#'
#' @return A data.frame containing sampled data for each follow up time. Contains column "sample_id"
#' @noRd
sample_from_period <- function(period_data,
                               p_control,
                               use_subset,
                               subset_expr,
                               sort = FALSE) {
  if (use_subset) period_data <- subset(period_data, eval(subset_expr))
  if (isTRUE(sort)) setorderv(period_data, c("followup_time", "id"))
  followup_split <- split(period_data, by = "followup_time")

  all_samples <- lapply(p_control, function(p) {
    rbindlist(lapply(
      followup_split,
      do_sampling,
      p_control = p
    ))
  })
  rbindlist(all_samples, idcol = "sample_id")
}

#' Sample controls and cases from data subset by trial and follow-up time
#'
#' For the given data, find cases and sample a number of controls.
#'
#' @param data Data to sample from
#' @param p_control Proportion of controls to select
#'
#' @return A data frame with the cases and the sampled controls
#' @noRd
do_sampling <- function(data, p_control = 0.01) {
  ### cases occurred at each period and follow-up visit
  cases <- which(data$outcome == 1)
  ncase <- length(cases)

  ### controls (still under follow-up and events haven't occurred) at each period and follow-up visit
  controls <- which(data$outcome == 0)
  n_controls <- length(controls)
  n_sample <- rbinom(1, n_controls, p_control)
  controlselect <- sample(controls, size = n_sample)
  dataall <- data[c(cases, controlselect), ]
  set(dataall, j = "sample_weight", value = c(rep(1, ncase), rep(1 / p_control, n_sample)))

  dataall
}
