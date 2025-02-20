#' @include te_expansion.R te_data.R te_weights.R te_datastore.R te_outcome_model.R
NULL

#' Trial Sequence class
#'
#' @slot data te_data.
#' @slot estimand character. Descriptive name of estimand.
#' @slot expansion te_expansion
#' @slot outcome_model te_outcome_model.
#' @slot outcome_data te_outcome_data.
#'
#' @export
setClass("trial_sequence",
  slots = c(
    data = "te_data",
    estimand = "character",
    expansion = "te_expansion",
    outcome_model = "te_outcome_model",
    censor_weights = "te_weights_spec",
    outcome_data = "te_outcome_data"
  ),
  prototype = prototype(
    censor_weights = new("te_weights_unset"),
    data = new("te_data_unset"),
    expansion = new("te_expansion_unset"),
    outcome_model = new("te_outcome_model_unset")
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
#' `r lifecycle::badge('experimental')`
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
  estimand <- switch(
    EXPR = estimand,
    ITT = "trial_sequence_ITT",
    PP = "trial_sequence_PP",
    AT = "trial_sequence_AT",
    estimand
  )
  estimand_class <- getClass(estimand)
  if (!extends(estimand_class, "trial_sequence")) {
    stop(estimand, " does not extend class trial_sequence")
  }

  object <- new(estimand, ...)
  object
}

# Show(trial_sequence) ------
setMethod(
  "show",
  c(object = "trial_sequence"),
  function(object) {
    catn("Trial Sequence Object")

    catn("Estimand:", object@estimand)
    catn("")
    catn("Data:")
    show(object@data)
    catn("")

    catn("IPW for informative censoring:")
    show(object@censor_weights)

    if (.hasSlot(object, "switch_weights")) {
      catn("")
      catn("IPW for treatment switch censoring:")
      show(object@switch_weights)
    }
    catn("")

    if (!is(object@data, "te_data_unset")) {
      show(object@expansion)
      catn("")
    }

    catn("Outcome model:")
    show(object@outcome_model)
    catn("")

    if (object@expansion@datastore@N > 0) {
      show(object@outcome_data)
    }
  }
)

# set_data --------

#' Set the trial data
#'
#' `r lifecycle::badge('experimental')`
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
           eligible = "eligible") {
    callNextMethod(object, data, censor_at_switch = FALSE, id, period, treatment, outcome, eligible)
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
           eligible = "eligible") {
    callNextMethod(object, data, censor_at_switch = FALSE, id, period, treatment, outcome, eligible)
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
           eligible = "eligible") {
    callNextMethod(object, data, censor_at_switch = TRUE, id, period, treatment, outcome, eligible)
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
           eligible = "eligible") {
    assert_class(object, "trial_sequence")
    assert_class(data, "data.frame")

    data_cols <- c(id, period, treatment, outcome, eligible)
    new_col_names <- c("id", "period", "treatment", "outcome", "eligible")

    assert_names(
      colnames(data),
      must.include = data_cols,
      disjunct.from = c("wt", "wtC", "weight", "dose", "assigned_treatment"),
      what = "colnames",
      .var.name = "data"
    )

    if (any(dups <- duplicated(data_cols))) {
      stop(
        "Duplicate column names specified: ",
        toString(paste0(new_col_names[dups], " = ", dQuote(data_cols[dups], FALSE)))
      )
    }

    trial_data <- as.data.table(data)
    data.table::setnames(trial_data, old = data_cols, new = new_col_names)
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

# set_censor_weight_model -------------

#' Set censoring weight model
#'
#' `r lifecycle::badge('experimental')`
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
#' @param model_fitter An object of class  `te_model_fitter` which determines the method used for fitting the weight
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
           pool_models = NULL,
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
    if (missing(numerator)) numerator <- ~1
    if (missing(denominator)) denominator <- ~1
    assert_formula(numerator)
    if ("time_on_regime" %in% rhs_vars(numerator)) stop("time_on_regime should not be used in numerator", call. = FALSE)
    assert_formula(denominator)
    assert_string(censor_event)
    assert_names(colnames(object@data@data), must.include = censor_event)
    numerator <- update.formula(numerator, paste("1 -", censor_event, "~ ."))
    denominator <- update.formula(denominator, paste("1 -", censor_event, "~ ."))

    assert_class(model_fitter, "te_model_fitter")
    object@censor_weights <- new(
      "te_weights_spec",
      numerator = numerator,
      denominator = denominator,
      pool_numerator = pool_models %in% c("numerator", "both"),
      pool_denominator = pool_models == "both",
      model_fitter = model_fitter
    )
    object <- update_outcome_formula(object)
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
    pool_models <- if (is.null(pool_models)) {
      "none"
    } else {
      checkmate::matchArg(pool_models, c("both", "numerator", "none"))
    }
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
    pool_models <- if (is.null(pool_models)) {
      "numerator"
    } else {
      checkmate::matchArg(pool_models, c("both", "numerator"))
    }
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
    pool_models <- if (is.null(pool_models)) {
      "none"
    } else {
      checkmate::matchArg(pool_models, c("both", "numerator", "none"))
    }
    callNextMethod()
  }
)



# set_switch_weight_model --------

#' Set switching weight model
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param object A [trial_sequence] object.
#' @param numerator Right hand side formula for the numerator model
#' @param denominator Right hand side formula for the denominator model
#' @param model_fitter A [te_model_fitter-class] object, such as [stats_glm_logit]
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

    if (missing(numerator)) numerator <- ~1
    if (missing(denominator)) denominator <- ~1
    # check which of these can be a null model
    assert_formula(numerator)
    if ("time_on_regime" %in% rhs_vars(numerator)) stop("time_on_regime should not be used in numerator", call. = FALSE)
    assert_formula(denominator)
    numerator <- update.formula(numerator, treatment ~ .)
    denominator <- update.formula(denominator, treatment ~ .)

    object@switch_weights <- new(
      "te_weights_spec",
      numerator = numerator,
      denominator = denominator,
      model_fitter = model_fitter
    )
    object <- update_outcome_formula(object)
    object
  }
)

#' @rdname set_switch_weight_model
setMethod(
  "set_switch_weight_model",
  c(object = "trial_sequence_ITT"),
  function(object, numerator, denominator, model_fitter) {
    stop("Switching weights are not supported for intention-to-treat analyses", call. = FALSE)
  }
)


# Set Outcome Model -----

#' @rdname set_outcome_model
setMethod(
  "set_outcome_model",
  c(object = "trial_sequence"),
  function(object,
           treatment_var = ~0,
           adjustment_terms = ~1,
           followup_time_terms = ~ followup_time + I(followup_time^2),
           trial_period_terms = ~ trial_period + I(trial_period^2),
           model_fitter = stats_glm_logit(save_path = NA)) {
    if (test_class(object@data, "te_data_unset")) stop("Use set_data() before set_outcome_model()")
    collection <- makeAssertCollection()
    formula_list <- list(
      treatment = as_formula(treatment_var, add = collection),
      adjustment = as_formula(adjustment_terms, add = collection),
      followup = as_formula(followup_time_terms, add = collection),
      period = as_formula(trial_period_terms, add = collection),
      stabilised = as_formula(get_stabilised_weights_terms(object), add = collection)
    )
    adjustment <- unique(c(all.vars(formula_list$adjustment), all.vars(formula_list$stabilised)))
    assert_names(
      adjustment,
      subset.of = colnames(object@data@data),
      what = "Variables in formulas",
      .var.name = "adjustment_terms",
      add = collection
    )
    reportAssertions(collection)
    treatment <- all.vars(formula_list$treatment)

    object@outcome_model <- new(
      "te_outcome_model",
      treatment_var = treatment,
      adjustment_vars = adjustment,
      model_fitter = model_fitter,
      adjustment_terms = formula_list$adjustment,
      treatment_terms = formula_list$treatment,
      followup_time_terms = formula_list$followup,
      trial_period_terms = formula_list$period,
      stabilised_weights_terms = formula_list$stabilised
    )
    object <- update_outcome_formula(object)
    object
  }
)

#' @rdname set_outcome_model
setMethod(
  "set_outcome_model",
  c(object = "trial_sequence_ITT"),
  function(object,
           adjustment_terms = ~1,
           followup_time_terms = ~ followup_time + I(followup_time^2),
           trial_period_terms = ~ trial_period + I(trial_period^2),
           model_fitter = stats_glm_logit(save_path = NA)) {
    callNextMethod(
      object,
      treatment_var = "assigned_treatment",
      adjustment_terms = adjustment_terms,
      followup_time_terms = followup_time_terms,
      trial_period_terms = trial_period_terms,
      model_fitter = model_fitter
    )
  }
)

#' @rdname set_outcome_model
setMethod(
  "set_outcome_model",
  c(object = "trial_sequence_PP"),
  function(object,
           adjustment_terms = ~1,
           followup_time_terms = ~ followup_time + I(followup_time^2),
           trial_period_terms = ~ trial_period + I(trial_period^2),
           model_fitter = stats_glm_logit(save_path = NA)) {
    callNextMethod(
      object,
      treatment_var = "assigned_treatment",
      adjustment_terms = adjustment_terms,
      followup_time_terms = followup_time_terms,
      trial_period_terms = trial_period_terms,
      model_fitter = model_fitter
    )
  }
)

#' @rdname set_outcome_model
setMethod(
  "set_outcome_model",
  c(object = "trial_sequence_AT"),
  function(object,
           treatment_var = "dose",
           adjustment_terms = ~1,
           followup_time_terms = ~ followup_time + I(followup_time^2),
           trial_period_terms = ~ trial_period + I(trial_period^2),
           model_fitter = stats_glm_logit(save_path = NA)) {
    callNextMethod(
      object,
      treatment_var = treatment_var,
      adjustment_terms = adjustment_terms,
      followup_time_terms = followup_time_terms,
      trial_period_terms = trial_period_terms,
      model_fitter = model_fitter
    )
  }
)

get_stabilised_weights_terms <- function(object) {
  assert_class(object, "trial_sequence")
  stabilised_terms <- ~1

  if (.hasSlot(object, "censor_weights")) {
    if (!is(object@censor_weights, "te_weights_unset")) {
      stabilised_terms <- add_rhs(stabilised_terms, object@censor_weights@numerator)
    }
  }
  if (.hasSlot(object, "switch_weights")) {
    if (!is(object@switch_weights, "te_weights_unset")) {
      stabilised_terms <- add_rhs(stabilised_terms, object@switch_weights@numerator)
    }
  }
  stabilised_terms
}

update_outcome_formula <- function(object) {
  assert_class(object, "trial_sequence")

  object@outcome_model@stabilised_weights_terms <- get_stabilised_weights_terms(object)

  formula_list <- list(
    ~1,
    treatment_terms = object@outcome_model@treatment_terms,
    adjustment_terms = object@outcome_model@adjustment_terms,
    followup_time_terms = object@outcome_model@followup_time_terms,
    trial_period_terms = object@outcome_model@trial_period_terms,
    stabilised_weights_terms = object@outcome_model@stabilised_weights_terms
  )
  keep <- lengths(formula_list) > 0
  outcome_formula <- Reduce(add_rhs, formula_list[keep])
  formula.tools::lhs(outcome_formula) <- quote(outcome)
  object@outcome_model@formula <- outcome_formula

  object@outcome_model@adjustment_vars <- unique(
    c(all.vars(formula_list$adjustment), all.vars(formula_list$stabilised))
  )

  object
}

# Set expansion options -----

#' Set expansion options
#'
#' `r lifecycle::badge('experimental')`
#'
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

#' @rdname internal-methods
setMethod(
  "set_expansion_options",
  c(object = "trial_sequence"),
  function(object, output, chunk_size = 0, first_period = 0, last_period = Inf, censor_at_switch) {
    assert_class(output, "te_datastore")
    assert_integerish(chunk_size, lower = 0, len = 1, any.missing = FALSE)
    if (first_period != 0) assert_integerish(first_period)
    if (last_period != Inf) assert_integerish(last_period)

    object@expansion <- new(
      "te_expansion",
      chunk_size = chunk_size,
      datastore = output,
      first_period = first_period,
      last_period = last_period,
      censor_at_switch = censor_at_switch
    )
    object
  }
)

#' @rdname set_expansion_options
setMethod(
  "set_expansion_options",
  c(object = "trial_sequence_ITT"),
  function(object, output, chunk_size = 0, first_period = 0, last_period = Inf) {
    callNextMethod(object, output, chunk_size, first_period, last_period, censor_at_switch = FALSE)
  }
)

#' @rdname set_expansion_options
setMethod(
  "set_expansion_options",
  c(object = "trial_sequence_PP"),
  function(object, output, chunk_size, first_period = 0, last_period = Inf) {
    callNextMethod(object, output, chunk_size, first_period, last_period, censor_at_switch = TRUE)
  }
)

#' @rdname set_expansion_options
setMethod(
  "set_expansion_options",
  c(object = "trial_sequence_ITT"),
  function(object, output, chunk_size, first_period = 0, last_period = Inf) {
    callNextMethod(object, output, chunk_size, first_period, last_period, censor_at_switch = FALSE)
  }
)


# Calculate Weights -------


#' Calculate Inverse Probability of Censoring Weights
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param object A [trial_sequence] object
#' @param quiet Prints model summaries is `TRUE`.
#' @param ... Other arguments used by methods.
#'
#' @return A [trial_sequence] object with updated `censor_weights` and/or `switch_weights` slots
#' @export
#'
#' @examples
#' \dontshow{
#' data.table::setDTthreads(2)
#' }
#' save_dir <- file.path(tempdir(), "switch_models")
#' ts <- trial_sequence("PP") |>
#'   set_data(
#'     data = data_censored,
#'     id = "id",
#'     period = "period",
#'     treatment = "treatment",
#'     outcome = "outcome",
#'     eligible = "eligible"
#'   ) |>
#'   set_switch_weight_model(
#'     numerator = ~ age + x1 + x3,
#'     denominator = ~age,
#'     model_fitter = stats_glm_logit(save_path = save_dir)
#'   ) |>
#'   calculate_weights()
#'
setGeneric("calculate_weights", function(object, ...) standardGeneric("calculate_weights"))

#' @rdname calculate_weights
setMethod(
  "calculate_weights",
  c(object = "trial_sequence_ITT"),
  function(object, quiet = FALSE) {
    use_censor_weights <- !is(object@censor_weights, "te_weights_unset")
    calculate_weights_trial_seq(object, quiet, switch_weights = FALSE, censor_weights = use_censor_weights)
  }
)

#' @rdname calculate_weights
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

#' @rdname calculate_weights
setMethod(
  "calculate_weights",
  c(object = "trial_sequence_PP"),
  function(object, quiet = FALSE) {
    use_censor_weights <- !is(object@censor_weights, "te_weights_unset")
    if (is(object@switch_weights, "te_weights_unset")) {
      stop("Switch weight models are not specified. Use set_switch_weight_model()")
    }
    calculate_weights_trial_seq(object, quiet, switch_weights = TRUE, censor_weights = use_censor_weights)
  }
)


# Expand trials --------

#' Expand trials
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param object A [trial_sequence] object
#'
#' @returns The [trial_sequence] `object` with a data set containing the full sequence of target trials. The data is
#'   stored according to the options set with [set_expansion_options()] and especially the `save_to_*` function.
#'
#' @export
setGeneric("expand_trials", function(object) standardGeneric("expand_trials"))

#' @rdname internal-methods
setMethod(
  "expand_trials",
  c(object = "trial_sequence"),
  function(object) {
    if (test_class(object@expansion, "te_expansion_unset")) stop("Use set_expansion_options() before expand_trials()")
    if (test_class(object@data, "te_data_unset")) stop("Use set_data() before expand_trials()")
    expand_trials_trial_seq(object)
  }
)


#' @rdname ipw_data
setMethod(
  "ipw_data",
  c(object = "trial_sequence"),
  function(object) {
    object@data@data
  }
)

#' @rdname ipw_data
setMethod(
  "ipw_data<-",
  c(object = "trial_sequence"),
  function(object, value) {
    object@data <- new(
      "te_data",
      data = value,
      nobs = nrow(value),
      n = uniqueN(value[, "id"])
    )
    validObject(object)
    object
  }
)

#' @rdname outcome_data
setMethod(
  "outcome_data",
  c(object = "trial_sequence"),
  function(object) {
    object@outcome_data@data
  }
)


#' @rdname outcome_data
setMethod(
  "outcome_data<-",
  c(object = "trial_sequence"),
  function(object, value) {
    if (is(object, "trial_sequence_PP") || is(object, "trial_sequence_ITT")) {
      if (!"assigned_treatment" %in% colnames(value)) stop("assigned_treatment column is not found.")
    }
    new_outcome_data <- te_outcome_data(value)
    object@outcome_data <- new_outcome_data
    validObject(object)
    object
  }
)


#' @rdname load_expanded_data
setMethod(
  f = "load_expanded_data",
  signature = "trial_sequence",
  definition = function(object, p_control, period, subset_condition, seed) {
    checkmate::assert_count(object@expansion@datastore@N, positive = TRUE)
    checkmate::assert_number(p_control, lower = 0, upper = 1, null.ok = TRUE)
    checkmate::assert_integerish(period, null.ok = TRUE, any.missing = FALSE, lower = 0)
    if (!is.null(subset_condition)) {
      checkmate::assert_string(subset_condition, null.ok = TRUE)
    }

    checkmate::assert_integerish(seed, null.ok = TRUE, len = 1, any.missing = FALSE)

    if (is.null(p_control)) {
      data_table <- read_expanded_data(object@expansion@datastore, period = period, subset_condition = subset_condition)
      data_table$sample_weight <- 1
    } else {
      data_table <- sample_expanded_data(
        object@expansion@datastore,
        period = period,
        subset_condition = subset_condition,
        p_control = p_control,
        seed = seed
      )
    }

    object@outcome_data <- te_outcome_data(data_table, p_control, subset_condition)

    object
  }
)




#' @rdname predict_marginal
setMethod(
  f = "predict",
  signature = "trial_sequence_ITT",
  function(object,
           newdata,
           predict_times,
           conf_int = TRUE,
           samples = 100,
           type = c("cum_inc", "survival")) {
    predict(
      object = object@outcome_model@fitted,
      newdata = newdata,
      predict_times = predict_times,
      conf_int = conf_int,
      samples = samples,
      type = type
    )
  }
)

#' @rdname predict_marginal
setMethod(
  f = "predict",
  signature = "trial_sequence_PP",
  function(object,
           newdata,
           predict_times,
           conf_int = TRUE,
           samples = 100,
           type = c("cum_inc", "survival")) {
    predict(
      object = object@outcome_model@fitted,
      newdata = newdata,
      predict_times = predict_times,
      conf_int = conf_int,
      samples = samples,
      type = type
    )
  }
)
