#' Case-control sampling of expanded data for the sequence of emulated trials
#'
#' Perform case-control sampling of expanded data to create a data set of reduced size and calculate sampling weights
#' to be used in `trial_msm()`.
#'
#' @param data_prep Result from [data_preparation()].
#' @param p_control Control sampling probability for selecting potential controls at each follow-up time of each trial.
#' @param subset_condition Expression used to [subset()] the trial data before case-control sampling.
#' @param sort Sort data before applying case-control sampling to make sure that the resulting data are identical when
#'   sampling from the expanded data created with `separate_files = TRUE` or `separate_files = FALSE`.
#'
#' @return A `data.frame` or a [split()] `data.frame` if  `length(p_control) > 1`. An additional column `sample_weight`
#'   containing the sample weights will be added to the result. These can be included in the models fit with
#'   [trial_msm()].
#' @export
#' @examples
#' # If necessary reduce the number of threads for data.table
#' data.table::setDTthreads(2)
#'
#' data("te_data_ex")
#' samples <- case_control_sampling_trials(te_data_ex, p_control = 0.01)
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
  if (isTRUE(sort)) setorderv(period_data, cols = c("followup_time", "id"))
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


#' @rdname sample_expanded_data
setMethod(
  f = "sample_expanded_data",
  signature = "te_datastore",
  definition = function(object, period, subset_condition, p_control) {
    data <- read_expanded_data(object, period = period, subset_condition = subset_condition)
    data <- lapply(
      split(data, list(data$trial_period, data$followup_time), drop = TRUE),
      do_sampling, p_control = p_control
    )
    data_table <- data.table::rbindlist(data)
    data_table
  }
)


#' @rdname sample_expanded_data
setMethod(
  f = "sample_expanded_data",
  signature = "te_datastore_duckdb",
  definition = function(object, period, subset_condition, p_control) {
    if (use_subset <- !missing(subset_condition)) {
      subset_expr <- translate_to_sql(subset_condition)
    }
    q_p1 <- "SELECT * FROM (SELECT * FROM trial_data WHERE outcome = 0 "
    q_p2 <- "UNION SELECT * FROM trial_data WHERE outcome = 1 "
    q_sample <- paste0("USING SAMPLE ", p_control * 100, " PERCENT (bernoulli) ")
    q_period <- ""
    if (!is.null(period)) q_period <- paste0("AND trial_period IN (", paste0(period, collapse = ", "), ") ")
    q_subset <- ""
    if (use_subset) q_subset <- paste0("AND (", subset_expr, ")")

    query <- paste0(q_p1, q_period, q_subset, ") ", q_sample, q_p2, q_period, q_subset)

    data <- DBI::dbGetQuery(conn = object@con, statement = query)
    data["sample_weight"] <- ifelse(data$outcome == 1, 1, 1 / p_control)
    data_table <- data.table::as.data.table(data)
    data_table
  }
)


#' Translate subset_condition to SQL syntax
#'
#' @param string subset_condition as a string
#'
#' @return a string
#'
#' @noRd
translate_to_sql <- function(string) {
  replacement <- c("\\|" = "OR", "&" = "AND", "==" = "=", "%in%" = "IN", "^c\\(" = "\\(")
  rt <- data.table::as.data.table(replacement, keep.rownames = "pattern")

  vec <- strsplit(string, " ")[[1]]

  if (length(grep(":", vec)) != 0) {
    vec <- translate_num_vec(vec)
  }

  for (i in 1:nrow(rt)) {
    vec <- gsub(pattern = rt$pattern[i], replacement = rt$replacement[i], vec)
  }
  string <- paste0(vec, collapse = " ")
  string
}


#' Translate numerical vectors to SQL syntax
#'
#' @param vec a vector obtained by splitting subset_condition string from sample_controls
#'
#' @return a vector
#'
#' @noRd
translate_num_vec <- function(vec) {
  for (i in grep(":", vec)) {
    vec <- replace(vec, i, paste0("(", paste0(seq(strsplit(vec[i], ":")[[1]][1],
                                                  strsplit(vec[i], ":")[[1]][2]),
                                              collapse = ", "), ")"))
  }
  vec
}


#' @rdname sample_controls
setMethod(
  f = "sample_controls",
  signature = "trial_sequence",
  definition = function(object, period, subset_condition, p_control) {
    checkmate::assert_integerish(period, null.ok = TRUE, any.missing = FALSE, lower = 0)
    checkmate::assert_count(object@expansion@datastore@N, positive = TRUE)
    checkmate::assert_number(p_control, lower = 0, upper = 1)
    if (!missing(subset_condition)) {
      checkmate::assert(is.character(subset_condition), length(subset_condition) == 1, combine = "and")
    }
    data_table <- sample_expanded_data(
      object@expansion@datastore, period = period, subset_condition = subset_condition, p_control = p_control
    )
    data_table
  }
)
