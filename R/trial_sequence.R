#' @include classes.R te_data.R te_weights.R
NULL

#' Trial Sequence class
#'
#' @slot data te_data.
#' @slot estimand character. Descriptive name of estimand.
#' @slot expansion te_expansion
#' @slot outcome_model te_outcome_model.
#'
#' @export
setClass("trial_sequence",
  slots = c(
    data = "te_data",
    estimand = "character",
    expansion = "te_expansion",
    outcome_model = "te_outcome_model",
    censor_weights = "te_weights_spec"
  ),
  prototype = prototype(
    censor_weights = new("te_weights_unset")
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
    estimand = "Per-protocol",
    switch_weights = new("te_weights_unset")
  ),
  slots = c(
    switch_weights = "te_weights_spec"
  )
)


#' @rdname trial_sequence-class
#' @export
setClass(
  "trial_sequence_ITT",
  contains = "trial_sequence",
  prototype = list(
    estimand = "Intention-to-treat"
  )
)

#' @rdname trial_sequence-class
#' @export
setClass(
  "trial_sequence_AT",
  contains = "trial_sequence",
  prototype = list(
    estimand = "As treated",
    switch_weights = new("te_weights_unset")
  ),
  slots = c(
    switch_weights = "te_weights_spec"
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
    catn("IPW for informative censoring:")
    show(object@censor_weights)
  }
)

setMethod(
  "show",
  c(object = "trial_sequence_PP"),
  function(object) {
    callNextMethod()
    catn("IPW for treatment switch censoring:")
    show(object@switch_weights)
  }
)

setMethod(
  "show",
  c(object = "trial_sequence_AT"),
  function(object) {
    callNextMethod()
    catn("IPW for treatment switch censoring:")
    show(object@switch_weights)
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
      disjunct.from = c("wt", "wtC", "weight"),
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
#' @param object trial_sequence.
#' @param numerator
#' @param denominator
#' @param pool_models
#' @param model_fitter
#' @param censor_event string. Name of column containing censoring indicator.
#'
#' @return `object` is returned with `@censor_weights` set
#' @export
#'
#' @examples
setGeneric("set_censor_weight_model", function(object, ...) standardGeneric("set_censor_weight_model"))

setMethod(
  "set_censor_weight_model",
  c(object = "trial_sequence"),
  function(object,
           censor_event,
           numerator,
           denominator,
           pool_models = c("none", "both", "numerator"),
           model_fitter = stats_glm_logit()) {
    if (missing(numerator)) numerator <- ~0
    if (missing(denominator)) denominator <- ~0
    # check which of these can be a null model TODO
    assert_formula(numerator)
    assert_formula(denominator)
    assert_string(censor_event)
    assert_names(colnames(object@data@data), must.include = censor_event)

    numerator <- update.formula(numerator, paste("1 -", censor_event, "~ ."))
    denominator <- update.formula(denominator, paste("1 -", censor_event, "~ ."))

    assert_class(model_fitter, "te_weights_fitter")
    object@censor_weights <- new(
      "te_weights_spec",
      numerator = numerator,
      denominator = denominator,
      pool_numerator = pool_models %in% c("numerator", "both"),
      pool_denominator = pool_models == "both",
      model_fitter = model_fitter
    )
    object
  }
)

setMethod(
  "set_censor_weight_model",
  c(object = "trial_sequence_PP"),
  function(object,
           censor_event,
           numerator,
           denominator,
           pool_models = "none",
           model_fitter = stats_glm_logit()) {
    assert_choice(pool_models, c("both", "numerator", "none"))
    callNextMethod()
  }
)
setMethod(
  "set_censor_weight_model",
  c(object = "trial_sequence_ITT"),
  function(object,
           censor_event,
           numerator,
           denominator,
           pool_models = "numerator",
           model_fitter = stats_glm_logit()) {
    assert_choice(pool_models, c("both", "numerator"))
    callNextMethod()
  }
)
setMethod(
  "set_censor_weight_model",
  c(object = "trial_sequence_AT"),
  function(object,
           censor_event,
           numerator,
           denominator,
           pool_models = "none",
           model_fitter = stats_glm_logit()) {
    assert_choice(pool_models, c("both", "numerator", "none"))
    callNextMethod()
  }
)


setGeneric("set_switch_weight_model", function(object, ..., model_fitter) standardGeneric("set_switch_weight_model"))


#' Set switching weight model
#'
#' @param object trial_sequence.
#' @param numerator
#' @param denominator
#' @param model_fitter
#'
#' @return `object` is returned with `@switch_weights` set
#' @export
#'
#' @examples
setMethod(
  "set_switch_weight_model",
  c(object = "trial_sequence"),
  function(object, numerator, denominator, model_fitter) {
    if (missing(numerator)) numerator <- ~0
    if (missing(denominator)) denominator <- ~0
    # check which of these can be a null model
    assert_formula(numerator)
    assert_formula(denominator)
    numerator <- update.formula(numerator, treatment ~ .)
    denominator <- update.formula(denominator, treatment ~ .)

    object@switch_weights <- new(
      "te_weights_spec",
      numerator = numerator,
      denominator = denominator,
      model_fitter = model_fitter
    )
    object
  }
)

setMethod(
  "set_switch_weight_model",
  c(object = "trial_sequence_ITT"),
  function(object, ...) {
    stop("Switching weights are not support for intention-to-treat analyses")
  }
)


# Set expansion options
setGeneric("set_expansion_options", function(object, ...) standardGeneric("set_expansion_options"))

setMethod(
  "set_expansion_options",
  c(object = "trial_sequence"),
  function(object, output, chunks) {
    assert_class(output, "te_datastore")
    assert_integerish(chunks)
    object@expansion <- new("te_expansion", chunks = chunks, datastore = output)
    object
  }
)

# Calculate Weights
setGeneric("calculate_weights", function(object, ...) standardGeneric("calculate_weights"))

setMethod(
  "calculate_weights",
  c(object = "trial_sequence_ITT"),
  function(object, quiet = FALSE) {
    use_censor_weights <- !is(object@censor_weights, "te_weights_unset")
    calculate_weights_trial_seq(object, quiet, switch_weights = FALSE, censor_weights = use_censor_weights)
  }
)
setMethod(
  "calculate_weights",
  c(object = "trial_sequence_AT"),
  function(object, quiet = FALSE) {
    use_censor_weights <- !is(object@censor_weights, "te_weights_unset")
    if (is(object@censor_weights, "te_weights_unset")) {
      stop("Switch weight models are not specified. Use set_switch_weight_model()")
    }
    calculate_weights_trial_seq(object, quiet, switch_weights = TRUE, censor_weights = use_censor_weights)
  }
)
setMethod(
  "calculate_weights",
  c(object = "trial_sequence_PP"),
  function(object, quiet = FALSE) {
    use_censor_weights <- !is(object@censor_weights, "te_weights_unset")
    if (is(object@censor_weights, "te_weights_unset")) {
      stop("Switch weight models are not specified. Use set_switch_weight_model()")
    }
    calculate_weights_trial_seq(object, quiet, switch_weights = TRUE, censor_weights = use_censor_weights)
  }
)
