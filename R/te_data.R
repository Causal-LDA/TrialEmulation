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
  c(object = "te_data"),
  function(object) {
    catn("Data")
    catn("N:", object@nobs, "observations from", object@n, "patients")
    print(object@data, nrows = 4, topn = 2)
  }
)


#' TrialEmulation Outcome Data Class
#'
#' @slot data A `data.table` object with columns "id", "period",
#' @slot n_rows Number of rows
#' @slot n_ids Number of IDs
#' @slot periods Vector of periods
#'   "treatment", "outcome", "eligible"
setClass("te_outcome_data",
         slots = c(
           data = "data.table",
           n_rows = "numeric",
           n_ids = "numeric",
           periods = "numeric"
         )
)

setValidity(
  "te_outcome_data",
  function(object) {
    checks <- list()
    checks["cols_check"] <- check_names(
      colnames(object@data),
      must.include = c("id", "trial_period", "followup_time", "outcome", "weights")
    )

    msg <- unlist(lapply(checks, function(x) if (is.character(x)) x else NULL))
    if (length(msg)) msg else TRUE
  }
)
