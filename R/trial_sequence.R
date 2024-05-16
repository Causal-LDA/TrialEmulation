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
    censor_weights = new("te_weights_unset"),
    data = new("te_data_unset")
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

# Show(trial_sequence) ------
setMethod(
  "show",
  c(object = "trial_sequence"),
  function(object) {
    catn("Trial Sequence Object")
    catn("Estimand:", object@estimand)
    show(object@data)
    catn("")
    catn("IPW for informative censoring:")
    show(object@censor_weights)
  }
)

setMethod(
  "show",
  c(object = "trial_sequence_PP"),
  function(object) {
    callNextMethod()
    catn("")
    catn("IPW for treatment switch censoring:")
    show(object@switch_weights)
  }
)

setMethod(
  "show",
  c(object = "trial_sequence_AT"),
  function(object) {
    callNextMethod()
    catn("")
    catn("IPW for treatment switch censoring:")
    show(object@switch_weights)
  }
)

# set_data --------

#' Set the trial data
#'
#' @param object A [trial_sequence-class] object
#' @param data A `data.frame` containing all the required variables in the person-time format, i.e., the
#'   <U+2018>long<U+2019> format.
#' @param id Name of the variable for identifiers of the individuals. Default is <U+2018>id<U+2019>.
#' @param period Name of the variable for the visit/period. Default is <U+2018>period<U+2019>.
#' @param treatment Name of the variable for the treatment indicator at that visit/period. Default is
#'   <U+2018>treatment<U+2019>.
#' @param outcome Name of the variable for the indicator of the outcome event at that visit/period. Default is
#'   <U+2018>outcome<U+2019>.
#' @param eligible Name of the variable for the indicator of eligibility for the target trial at that visit/period.
#'   Default is <U+2018>eligible<U+2019>.
#' @param expand_variables A character vector of column names of variables that may be used after expansion in the
#'   marginal structural model. Columns not specified here may be dropped during data expansion.
#' @param ... Other arguments used by methods internally.
#'
#' @return An updated [trial_sequence][trial_sequence-class] object with data
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

#' @rdname set_data
setMethod(
  "set_data",
  c(object = "trial_sequence_ITT", data = "data.frame"),
  function(object,
           data,
           id = "id",
           period = "period",
           treatment = "treatment",
           outcome = "outcome",
           eligible = "eligible",
           expand_variables = NA_character_) {
    callNextMethod(object, data, censor_at_switch = FALSE, id, period, treatment, outcome, eligible, expand_variables)
  }
)

#' @rdname set_data
setMethod(
  "set_data",
  c(object = "trial_sequence_AT", data = "data.frame"),
  function(object,
           data,
           id = "id",
           period = "period",
           treatment = "treatment",
           outcome = "outcome",
           eligible = "eligible",
           expand_variables = NA_character_) {
    callNextMethod(object, data, censor_at_switch = FALSE, id, period, treatment, outcome, eligible, expand_variables)
  }
)

#' @rdname set_data
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
           expand_variables = NA_character_) {
    callNextMethod(object, data, censor_at_switch = TRUE, id, period, treatment, outcome, eligible, expand_variables)
  }
)

#' @rdname internal-methods
#' @inheritParams set_data
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
           eligible = "eligible",
           expand_variables = NA_character_) {
    assert_class(object, "trial_sequence")
    assert_class(data, "data.frame")
    assert_names(
      colnames(data),
      must.include = c(id, period, treatment, outcome, eligible),
      disjunct.from = c("wt", "wtC", "weight"),
      what = "colnames",
      .var.name = "data"
    )
    if (test_character(expand_variables, all.missing = FALSE)) {
      assert_names(colnames(data), must.include = expand_variables, what = "colnames", .var.name = "data")
    }

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
      n = uniqueN(trial_data[, "id"]),
      expand_variables = expand_variables
    )
    object
  }
)

# set_censor_weight_model -------------

#' Set censoring weight model
#'
#' @param object trial_sequence.
#' @param numerator A RHS formula to specify the logistic models for estimating the numerator terms of the inverse
#'   probability of censoring weights.
#' @param denominator A RHS formula to specify the logistic models for estimating the denominator terms of the inverse
#'   probability of censoring weights.
#' @param pool_models Fit pooled or separate censoring models for those treated and those untreated at the immediately
#'   previous visit. Pooling can be specified for the models for the numerator and denominator terms of the inverse
#'   probability of censoring weights. One of "none", "numerator", or "both" (default is "none" except when estimand =
#'   "ITT" then default is "numerator").
#' @param model_fitter An object of class  `te_weights_fitter` which determines the method used for fitting the weight
#'   models. For logistic regression use [stats_glm_logit()].
#' @param censor_event string. Name of column containing censoring indicator.
#'
#' @return `object` is returned with `@censor_weights` set
#' @export
#'
#' @examples
#' trial_sequence("ITT") |>
#'   set_data(data = data_censored) |>
#'   set_censor_weight_model(
#'     censor_event = "censored",
#'     numerator = ~ age_s + x1 + x3,
#'     denominator = ~ x3 + x4,
#'     pool_models = "both",
#'     model_fitter = stats_glm_logit(save_path = tempdir())
#'   )
setGeneric(
  "set_censor_weight_model",
  function(object,
           censor_event,
           numerator,
           denominator,
           pool_models,
           model_fitter) {
    standardGeneric("set_censor_weight_model")
  }
)

#' @rdname set_censor_weight_model
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

#' @rdname set_censor_weight_model
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

#' @rdname set_censor_weight_model
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

#' @rdname set_censor_weight_model
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



# set_switch_weight_model --------

#' Set switching weight model
#'
#' @param object A [trial_sequence] object.
#' @param numerator Right hand side formula for the numerator model
#' @param denominator Right hand side formula for the denominator model
#' @param model_fitter A [te_weights_fitter-class] object, such as [stats_glm_logit]
#' @param eligible_wts_0 Name of column containing indicator (0/1) for observation to be excluded/included in weight
#'   model.
#' @param eligible_wts_1 Exclude some observations when fitting the models for the inverse probability of treatment
#'   weights. For example, if it is assumed that an individual will stay on treatment for at least 2 visits, the first 2
#'   visits after treatment initiation by definition have a probability of staying on the treatment of 1.0 and should
#'   thus be excluded from the weight models for those who are on treatment at the immediately previous visit. Users can
#'   define a variable that indicates that these 2 observations are ineligible for the weight model for those who are on
#'   treatment at the immediately previous visit and add the variable name in the argument `eligible_wts_1`. Similar
#'   definitions are applied to `eligible_wts_0` for excluding observations when fitting the models for the inverse
#'   probability of treatment weights for those who are not on treatment at the immediately previous visit.
#' @param ... Other arguments used by methods.
#'
#' @return `object` is returned with `@switch_weights` set
#' @export
#'
#' @examples
#' trial_sequence("PP") |>
#'   set_data(data = data_censored) |>
#'   set_switch_weight_model(
#'     numerator = ~ age_s + x1 + x3,
#'     denominator = ~ x3 + x4,
#'     model_fitter = stats_glm_logit(tempdir())
#'   )
setGeneric(
  "set_switch_weight_model",
  function(object, numerator, denominator, model_fitter, ...) standardGeneric("set_switch_weight_model")
)

#' @rdname set_switch_weight_model
setMethod(
  "set_switch_weight_model",
  c(object = "trial_sequence"),
  function(object, numerator, denominator, model_fitter, eligible_wts_0 = NULL, eligible_wts_1 = NULL) {
    if (is(object@data, "te_data_unset")) {
      stop("Please use set_data() to set up the data before setting switch weight models", call. = TRUE)
    }

    cols <- colnames(object@data@data)
    if (!is.null(eligible_wts_0)) {
      assert_names(cols, must.include = c(eligible_wts_0))
      colnames(object@data@data)[which(cols == eligible_wts_0)] <- "eligible_wts_0"
    }
    if (!is.null(eligible_wts_1)) {
      assert_names(cols, must.include = c(eligible_wts_1))
      colnames(object@data@data)[which(cols == eligible_wts_1)] <- "eligible_wts_1"
    }

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

#' @rdname set_switch_weight_model
setMethod(
  "set_switch_weight_model",
  c(object = "trial_sequence_ITT"),
  function(object, numerator, denominator, model_fitter) {
    stop("Switching weights are not support for intention-to-treat analyses")
  }
)


# Set Outcome Model -----

setGeneric("set_outcome_model", function(object, ...) standardGeneric("set_outcome_model"))
setMethod(
  "set_outcome_model",
  c(object = "trial_sequence"),
  function(object, formula, treatment_var, fitter) {
    object@outcome <- new("te_outcome_model", formula = formula, treatment_var = treatment_var, fitter = fitter)
    object
  }
)


# Set expansion options -----

# add roxygen header
#' Set expansion options
#' @param object A [trial_sequence] object
#' @param output A [te_datastore][te_datastore-class] object as created by a `save_to_*` function.
#' @param chunk_size An integer specifying the number of patients to include in each expansion iteration
#' @param first_period An integer specifying the first period to include in the expansion
#' @param last_period An integer specifying the last period to include in the expansion
#' @param ... Arguments used in methods
#' @return `object` is returned with `@expansion` set
#' @export
#' @family save_to
#' @examples
#' output_dir <- file.path(tempdir(check = TRUE), "expanded_data")
#' ITT_trial <- trial_sequence("ITT") |>
#'   set_data(data = data_censored) |>
#'   set_expansion_options(output = save_to_csv(output_dir), chunk_size = 500)
#'
#' # Delete directory
#' unlink(output_dir, recursive = TRUE)
setGeneric("set_expansion_options", function(object, ...) standardGeneric("set_expansion_options"))

#' @rdname set_expansion_options
setMethod(
  "set_expansion_options",
  c(object = "trial_sequence"),
  function(object, output, chunk_size, first_period, last_period) {
    assert_class(output, "te_datastore")
    assert_integerish(chunk_size)
    if (missing(first_period)) first_period <- min(object@data@data$period)
    if (missing(last_period)) last_period <- max(object@data@data$period)
    assert_integerish(first_period)
    assert_integerish(last_period)
    object@expansion <- new(
      "te_expansion",
      chunk_size = chunk_size,
      datastore = output,
      first_period = as.integer(first_period),
      last_period = as.integer(last_period)
    )
    object
  }
)

# Calculate Weights -------


#' Calculate Inverse Probability of Censoring Weights
#'
#' @param object A [trial_sequence] object
#' @param quiet Prints model summaries is `TRUE`.
#' @param ... Other arguments used by methods.
#'
#' @return
#' @export
#'
#' @examples
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


# Expand trials --------

#' Expand trials
#' @param object A [trial_sequence] object
#' @param ... Other arguments used in methods
#'
#' @returns The [trial_sequence] `object` with a data set containing the full sequence of target trials. The data is
#'   stored according to the options set with [set_expansion_options()] and especially the `save_to_*` function.
#'
#' @export
setGeneric("expand_trials", function(object, ...) standardGeneric("expand_trials"))

#' @rdname internal-methods
setMethod(
  "expand_trials",
  c(object = "trial_sequence_PP"),
  function(object) {
    expand_trials_trial_seq(object, censor_at_switch = TRUE, keeplist = NULL)
  }
)

#' @rdname internal-methods
setMethod(
  "expand_trials",
  c(object = "trial_sequence_ITT"),
  function(object) {
    expand_trials_trial_seq(object, censor_at_switch = FALSE, keeplist = NULL)
  }
)

#' @rdname internal-methods
setMethod(
  "expand_trials",
  c(object = "trial_sequence_AT"),
  function(object) {
    expand_trials_trial_seq(object, censor_at_switch = FALSE, keeplist = "dose")
  }
)
