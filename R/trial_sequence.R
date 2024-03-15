#' @include classes.R te_data.R
NULL

#' Trial Sequence class
#'
#' @slot data te_data.
#' @slot estimand character. Descriptive name of estimand.
#' @slot expansion te_expanded_datastore.
#' @slot outcome_model te_outcome_model.
#'
#' @export
setClass("trial_sequence",
  slots = c(
    data = "te_data",
    estimand = "character",
    expansion = "te_expanded_datastore",
    outcome_model = "te_outcome_model"
  )
)


#' @rdname trial_sequence-class
#' @slot censor_weight te_weight. Object to define weighting to account for
#'   informative censoring
#' @slot censor_weight te_weight. Object to define weighting to account for
#'   informative censoring due to treatment switching
#' @export
setClass(
  "trial_sequence_PP",
  contains = "trial_sequence",
  prototype = list(
    estimand = "Per-protocol"
  ),
  slots = c(
    censor_weight = "te_weight",
    switch_weight = "te_weight"
  )
)


#' @rdname trial_sequence-class
#' @export
setClass(
  "trial_sequence_ITT",
  contains = "trial_sequence",
  prototype = list(
    estimand = "Intention-to-treat"
  ),
  slots = c(censor_weight = "te_weight")
)

#' @rdname trial_sequence-class
#' @export
setClass(
  "trial_sequence_AT",
  contains = "trial_sequence",
  prototype = list(
    estimand = "As treated"
  ),
  slots = c(
    censor_weight = "te_weight",
    switch_weight = "te_weight"
  )
)


#' Create a sequence of emulated target trials object
#'
#' @param estimand The name of the estimand for this analysis, either one of `"ITT"`, `"PP"`, `"AT"` for
#' intention-to-treat, per-protocol, as-treated estimands respectively, or the name of a class extending
#' [trial_sequence-class]
#'
#' @param ... Other parameters used when creating object
#' @returns An estimand specific trial sequence object
#'
#' @export
#' @examples
#' trial_sequence("ITT")
#'
trial_sequence <- function(estimand, ...) {
  estimand_class_name <- switch(
    EXPR = estimand,
    ITT = "trial_sequence_ITT",
    PP = "trial_sequence_PP",
    AT = "trial_sequence_AT",
    estimand
  )
  estimand_class <- getClass(estimand_class_name, .Force = TRUE)
  if (is.null(estimand_class)) {
    stop("No class found with name ", estimand_class_name)
  } else if (!extends(estimand_class, "trial_sequence")) {
    stop(estimand_class_name, " does not extend class trial_sequence")
  }
  new(estimand_class_name, ...)
}

# Show
setMethod(
  "show",
  c(object = "trial_sequence"),
  function(object) {
    catn("Trial Sequence Object")
    catn("Estimand:", object@estimand)
    show(object@data)
  }
)


#' Set the trial data
#'
#' @param trial_sequence A [trial_sequence-class] object
#' @param data A `data.frame` containing all the required variables in the
#'   person-time format, i.e., the <U+2018>long<U+2019> format.
#' @param id Name of the variable for identifiers of the individuals. Default is
#'   <U+2018>id<U+2019>.
#' @param period Name of the variable for the visit/period. Default is <U+2018>period<U+2019>.
#' @param treatment Name of the variable for the treatment indicator at that
#'   visit/period. Default is <U+2018>treatment<U+2019>.
#' @param outcome Name of the variable for the indicator of the outcome event at
#'   that visit/period. Default is <U+2018>outcome<U+2019>.
#' @param eligible Name of the variable for the indicator of eligibility for the
#'   target trial at that visit/period. Default is <U+2018>eligible<U+2019>.
#'
#' @return A `trial_sequence` object with data
#' @export
#'
#' @examples
#' data(trial_example)
#' trial_sequence("ITT") |>
#'   set_data(
#'     data = trial_example,
#'     id = "id",
#'     period = "period",
#'     eligible = "eligible",
#'     treatment = "treatment"
#'   )
#'
setGeneric("set_data", function(object, data, ...) standardGeneric("set_data"))

setMethod(
  "set_data",
  c(object = "trial_sequence_ITT", data = "data.frame"),
  function(object,
           data,
           id = "id",
           period = "period",
           treatment = "treatment",
           outcome = "outcome",
           eligible = "eligible") {
    callNextMethod(object, data, censor_at_switch = FALSE, id, period, treatment, outcome, eligible)
  }
)

setMethod(
  "set_data",
  c(object = "trial_sequence_AT", data = "data.frame"),
  function(object,
           data,
           id = "id",
           period = "period",
           treatment = "treatment",
           outcome = "outcome",
           eligible = "eligible") {
    callNextMethod(object, data, censor_at_switch = FALSE, id, period, treatment, outcome, eligible)
  }
)


setMethod(
  "set_data",
  c(object = "trial_sequence_PP", data = "data.frame"),
  function(object,
           data,
           id = "id",
           period = "period",
           treatment = "treatment",
           outcome = "outcome",
           eligible = "eligible",
           eligible_wts_0 = NULL,
           eligible_wts_1 = NULL) {
    cols <- colnames(data)
    if (test_string(eligible_wts_0)) {
      assert_names(cols, must.include = c(eligible_wts_0))
      colnames(data)[which(cols == eligible_wts_0)] <- "eligible_wts_0"
    }
    if (test_string(eligible_wts_1)) {
      assert_names(cols, must.include = c(eligible_wts_1))
      colnames(data)[which(cols == eligible_wts_1)] <- "eligible_wts_1"
    }
    callNextMethod(object, data, censor_at_switch = TRUE, id, period, treatment, outcome, eligible)
  }
)


setMethod(
  "set_data",
  c(object = "trial_sequence", data = "data.frame"),
  function(object,
           data,
           censor_at_switch,
           id = "id",
           period = "period",
           treatment = "treatment",
           outcome = "outcome",
           eligible = "eligible") {
    assert_class(object, "trial_sequence")
    assert_class(data, "data.frame")
    assert_names(
      colnames(data),
      must.include = c(id, period, treatment, outcome, eligible),
      what = "colnames",
      .var.name = "data"
    )

    trial_data <- as.data.table(data)
    data.table::setnames(
      trial_data,
      old = c(id, period, treatment, outcome, eligible),
      new = c("id", "period", "treatment", "outcome", "eligible")
    )
    trial_data <- data_manipulation(trial_data, use_censor = censor_at_switch)
    object@data <- new(
      "te_data",
      data = trial_data,
      nobs = nrow(trial_data),
      n = uniqueN(trial_data[, "id"])
    )
    object
  }
)

#' Set censoring weight model
#'
#'
setGeneric("set_censor_weight_model", function(object, ...) standardGeneric("set_censor_weight_model"))
setMethod()


#' Set switching weight model
#'
#'
setGeneric("set_switch_weight_model", function(object, ...) standardGeneric("set_switch_weight_model"))
