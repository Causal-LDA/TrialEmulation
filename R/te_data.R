#' TrialEmulation Data Class
#' @slot data A `data.table` object with columns "id", "period",
#'   "treatment", "outcome", "eligible"
setClass("te_data",
  slots = c(
    data = "data.table",
    nobs = "numeric",
    n = "numeric"
  )
)

setValidity(
  "te_data",
  function(object) {
    checks <- list()
    checks["cols_check"] <- check_names(
      colnames(object@data),
      must.include = c("id", "period", "treatment", "outcome", "eligible")
    )

    msg <- unlist(lapply(checks, function(x) if (is.character(x)) x else NULL))
    if (length(msg)) msg else TRUE
  }
)

# For empty slot
setClass(
  "te_data_unset",
  contains = "te_data",
  prototype = list(
    data = data.frame(),
    nobs = 0,
    n = 0
  )
)

# Show
setMethod(
  "show",
  c(object = "te_data_unset"),
  function(object) {
    catn(" - No data has been set. Use set_data()")
  }
)

setMethod(
  "show",
  c(object = "te_data"),
  function(object) {
    catn(" - N:", object@nobs, "observations from", object@n, "patients")
    # hide the derived columns except for "time_on_regime" which may be added to weight models
    show_cols <- setdiff(
      colnames(object@data),
      c(
        "time_of_event", "first", "am_1", "cumA", "switch", "regime_start", "eligible0", "eligible1",
        "p_n", "p_d", "pC_n", "pC_d"
      )
    )
    print(
      data.table(object@data)[, show_cols, with = FALSE],
      nrows = 4, topn = 2, show.indices = FALSE, print.keys = FALSE
    )
  }
)


#' TrialEmulation Outcome Data Class
#'
#' @slot data A `data.table` object with columns "id", "period",
#' @slot n_rows Number of rows
#' @slot n_ids Number of IDs
#' @slot periods Vector of periods
#'   "treatment", "outcome", "eligible"
setClass(
  "te_outcome_data",
  slots = c(
    data = "data.table",
    n_rows = "numeric",
    n_ids = "numeric",
    periods = "numeric",
    p_control = "numeric",
    subset_condition = "character"
  )
)

setValidity(
  "te_outcome_data",
  function(object) {
    checks <- list()
    checks["cols_check"] <- check_names(
      colnames(object@data),
      must.include = c("id", "trial_period", "followup_time", "outcome", "weight")
    )

    msg <- unlist(lapply(checks, function(x) if (is.character(x)) x else NULL))
    if (length(msg)) msg else TRUE
  }
)

# Show
setMethod(
  "show",
  c(object = "te_outcome_data"),
  function(object) {
    if (!length(object@data)) {
      catn("No outcome data, use load_expanded_data()")
    } else {
      catn("Outcome data")
      catn(
        "N:", object@n_rows, "observations from", object@n_ids, "patients in", length(object@periods), "trial periods"
      )
      catn("Periods:", object@periods)
      if (length(object@subset_condition)) catn("Subset condition:", object@subset_condition)
      if (length(object@p_control)) catn("Sampling control observations with probability:", object@p_control)
      print(object@data, nrows = 4, topn = 2, show.indices = FALSE, print.keys = FALSE)
    }
  }
)


#' Create te_outcome_data
#'
#' @param data A data.table derived from expanded data, containing columns
#'  `c("id", "trial_period", "followup_time", "outcome", "weight")`
#' @param p_control p_control used for sampling
#' @param subset_condition subset_condition parameters used for loading/sampling
#' @return A `te_outcome_data` object
#' @noRd
te_outcome_data <- function(data, p_control = NULL, subset_condition = NULL) {
  checkmate::assert_data_table(data)
  checkmate::assert_names(colnames(data), must.include = c("id", "trial_period", "followup_time", "outcome", "weight"))
  n_rows <- nrow(data)
  if (n_rows == 0) warning("Outcome data has 0 rows")
  n_ids <- data.table::uniqueN(data[, "id"])
  periods <- sort(unique(data$trial_period))
  if (is.null(subset_condition)) subset_condition <- character()
  if (is.null(p_control)) p_control <- numeric()
  new(
    "te_outcome_data",
    data = data,
    n_rows = n_rows,
    n_ids = n_ids,
    periods = periods,
    p_control = p_control,
    subset_condition = subset_condition
  )
}
