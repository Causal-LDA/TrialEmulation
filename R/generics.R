#' Summary methods
#'
#' `r lifecycle::badge('stable')`
#' Print summaries of data and model objects produced by `TrialEmulation`.
#' @rdname summary_TE
#'
#' @param object Object to print summary
#' @param ... Additional arguments passed to print methods.
#'
#' @returns No value, displays summaries of object.
#' @export
summary.TE_data_prep <- function(object, ...) {
  cat("Number of observations in expanded data:", object$N, "\n")
  cat("First trial period:", object$min_period, "\n")
  cat("Last trial period:", object$max_period, "\n\n")

  has_cens_w <- test_list(object$censor_models, min.len = 1, all.missing = TRUE)
  has_switch_w <- test_list(object$switch_models, min.len = 1, all.missing = TRUE)

  cat(console_line(), "\n")
  if (has_cens_w || has_switch_w) {
    cat_underline("Weight models")
    if (has_switch_w) {
      cat_underline("Treatment switch models")
      for (n in names(object$switch_models)) {
        cat("switch_models$", n, ":\n ", sep = "")
        print(object$switch_models[[n]], full = FALSE, ...)
        cat(console_line(), "\n")
      }
    }
    if (has_cens_w) {
      if (length(object$censor_models)) {
        cat_underline("Censoring models")
        for (n in names(object$censor_models)) {
          cat("censor_models$", n, ":\n", sep = "")
          print(object$censor_models[[n]], full = FALSE, ...)
          cat(console_line(), "\n")
        }
      }
    }
  }
}

#' @rdname summary_TE
#' @export
summary.TE_data_prep_sep <- function(object, ...) {
  cat("Expanded Trial Emulation data\n\n")

  n_files <- length(object$data)
  cat("Expanded data saved in ", n_files, " csv file", if (n_files > 1) "s" else "", ":\n", sep = "")
  print(data.table(data = object$data), topn = 3, nrows = 5, col.names = "none", class = FALSE, ...)
  cat("\n\n")
  NextMethod()
}

#' @rdname summary_TE
#' @export
summary.TE_data_prep_dt <- function(object, ...) {
  cat("Expanded Trial Emulation data\n\n")
  print(object$data, topn = 3, nrows = 3, class = FALSE, ...)
  cat("\n")
  NextMethod()
}

#' @export
#' @rdname summary_TE
summary.TE_msm <- function(object, ...) {
  cat("Trial Emulation Outcome Model\n\n")
  cat("Outcome model formula:\n")
  print(object$model$formula, showEnv = FALSE)
  cat("\n")
  cat("Coefficent summary (robust):\n")
  summary(object$robust, ...)

  object_name <- match.call()[["object"]]

  cat("\n")
  cat(object_name, "$model contains the fitted glm model object.\n", sep = "")
  cat(object_name, "$robust$matrix contains the full robust covariance matrix.\n", sep = "")
}

#' @export
#' @rdname summary_TE
summary.TE_robust <- function(object, ...) {
  to_print <- object$summary
  to_print$p_value <- format.pval(to_print$p_value, ...)
  print.data.frame(to_print, row.names = FALSE, ...)
}



#' Print a weight summary object
#'
#' `r lifecycle::badge('stable')`
#' @param x print TE_weight_summary object.
#' @param full Print full or short summary.
#' @param ... Arguments passed to [print.data.frame].
#' @export
#' @returns No return value, only for printing.
#' @rdname print_TE
print.TE_weight_summary <- function(x, full = TRUE, ...) {
  cat(x$description, "\n\n")
  print.data.frame(x$summary, row.names = FALSE, ...)
  cat("\n")
  if (full) {
    print.data.frame(x$fit_summary, row.names = FALSE, ...)
    if (!is.null(x$path)) {
      cat("\n")
      cat("Object saved at \"", x$path, "\"", sep = "")
    }
  }
}



#' Internal Methods
#' @name internal-methods
#' @description Various S4 methods which are not directly for use by users.
#' @keywords internal
#'
NULL

#' Method to save expanded data
#'
#' This method is used internally by [expand_trials] to save the data to the "datastore" defined in
#' [set_expansion_options].
#'
#' @param object An object of class [te_datastore][te_datastore-class] or a child class.
#' @param data A data frame containing the expanded trial data. The columns `trial_period` and `id` are present, which
#'  may be used in methods to save the data in an optimal way, such as with indexes, keys or separate files.
#'
#' @return An updated `object` with the data stored. Notably `object@N` should be increased
#' @export
#'
#' @examples
#' temp_dir <- tempfile("csv_dir_")
#' dir.create(temp_dir)
#' datastore <- save_to_csv(temp_dir)
#' data(vignette_switch_data)
#' save_expanded_data(datastore, vignette_switch_data[1:200, ])
#'
#' # delete after use
#' unlink(temp_dir, recursive = TRUE)
setGeneric("save_expanded_data", function(object, data) standardGeneric("save_expanded_data"))


#' Method to read expanded data
#'
#' This method is used on [te_datastore-class] objects to read selected data and return one `data.table`.
#'
#' @param object An object of class [te_datastore-class].
#' @param period An integerish vector of non-zero length to select trial period(s) or `NULL` (default) to
#'  select all files.
#' @param subset_condition A string of length 1 or `NULL` (default).
#'
#' @return A `data.frame` of class `data.table`.
#' @export
#'
#' @examples
#' # create a te_datastore_csv object and save some data
#' temp_dir <- tempfile("csv_dir_")
#' dir.create(temp_dir)
#' datastore <- save_to_csv(temp_dir)
#' data(vignette_switch_data)
#' expanded_csv_data <- save_expanded_data(datastore, vignette_switch_data[1:200, ])
#'
#' # read expanded data
#' read_expanded_data(expanded_csv_data)
#'
#' # delete after use
#' unlink(temp_dir, recursive = TRUE)
setGeneric("read_expanded_data", function(object, period = NULL, subset_condition = NULL) {
  standardGeneric("read_expanded_data")
})


#' Method to read, subset and sample expanded data
#'
#' This method is used on [trial_sequence-class] objects to read, subset and sample expanded data.
#'
#' @param object An object of class [trial_sequence-class].
#' @param p_control Probability of selecting a control.
#' @param period An integerish vector of non-zero length to select trial period(s) or `NULL` (default) to
#'  select all trial periods.
#' @param subset_condition A string or `NULL` (default). `subset_condition` will be translated to a call
#'  (in case the expanded data is saved as a data.table or in the csv format) or to a SQL-query
#'  (in case the expanded data is saved as a duckdb file).
#'
#'  The operators `"==", "!=", ">", ">=", "<", "<=", %in%", "&", "|"` are supported.
#'  Numeric vectors can be written as `c(1, 2, 3)` or `1:3`. Variables are not supported.
#'
#'  *Note*: Make sure numeric vectors written as `1:3` are surrounded by spaces, e.g. `a %in% c( 1:4 , 6:9 )`,
#'    otherwise the code will fail.
#' @param seed An integer seed or `NULL` (default).
#'
#'  *Note*: The same seed will return a different result depending on the class of the [te_datastore-class]
#'    object contained in the [trial_sequence-class] object.
#'
#' @return An updated [trial_sequence-class] object, the data is stored in slot `@outcome_data`
#'    as a [te_outcome_data-class] object.
#' @noRd
#'
#' @examples
#' # create a trial_sequence-class object
#' trial_itt_dir <- file.path(tempdir(), "trial_itt")
#' dir.create(trial_itt_dir)
#' trial_itt <- trial_sequence(estimand = "ITT") |>
#'   set_data(data = data_censored) |>
#'   set_outcome_model(adjustment_terms = ~ x1 + x2)
#'
#' trial_itt_csv <- set_expansion_options(
#'   trial_itt,
#'   output = save_to_csv(file.path(trial_itt_dir, "trial_csvs")),
#'   chunk_size = 500
#' ) |>
#'   expand_trials()
#'
#' # sample_controls default behaviour returns all trial_periods
#' sample_controls(trial_itt_csv, p_control = 0.01)
#'
#' # sample_controls can subset the data before sampling
#' sample_controls(
#'   trial_itt_csv,
#'   p_control = 0.2,
#'   period = 1:10,
#'   subset_condition = "followup_time %in% 1:20 & x2 < 1",
#' )
#'
#' # delete after use
#' unlink(trial_itt_dir, recursive = TRUE)
setGeneric(
  "sample_controls",
  function(object, p_control, period = NULL, subset_condition = NULL, seed = NULL) standardGeneric("sample_controls")
)


#' Method to read, subset and sample expanded data
#'
#' `r lifecycle::badge('experimental')`
#'
#' This method is used on [trial_sequence-class] objects to read, subset and sample expanded data.
#'
#' @param object An object of class [trial_sequence-class].
#' @param p_control Probability of selecting a control, `NULL` for no sampling (default).
#' @param period An integerish vector of non-zero length to select trial period(s) or `NULL` (default) to
#'  select all trial periods.
#' @param subset_condition A string or `NULL` (default). `subset_condition` will be translated to a call
#'  (in case the expanded data is saved as a data.table or in the csv format) or to a SQL-query
#'  (in case the expanded data is saved as a duckdb file).
#'
#'  The operators `"==", "!=", ">", ">=", "<", "<=", %in%", "&", "|"` are supported.
#'  Numeric vectors can be written as `c(1, 2, 3)` or `1:3`. Variables are not supported.
#'
#'  *Note*: Make sure numeric vectors written as `1:3` are surrounded by spaces, e.g. `a %in% c( 1:4 , 6:9 )`,
#'    otherwise the code will fail.
#' @param seed An integer seed or `NULL` (default).
#'
#'  *Note*: The same seed will return a different result depending on the class of the [te_datastore-class]
#'    object contained in the [trial_sequence-class] object.
#'
#' @return An updated [trial_sequence-class] object, the data is stored in slot `@outcome_data`
#'    as a [te_outcome_data-class] object.
#' @export
#'
#' @examples
#' # create a trial_sequence-class object
#' trial_itt_dir <- file.path(tempdir(), "trial_itt")
#' dir.create(trial_itt_dir)
#' trial_itt <- trial_sequence(estimand = "ITT") |>
#'   set_data(data = data_censored) |>
#'   set_outcome_model(adjustment_terms = ~ x1 + x2)
#'
#' trial_itt_csv <- set_expansion_options(
#'   trial_itt,
#'   output = save_to_csv(file.path(trial_itt_dir, "trial_csvs")),
#'   chunk_size = 500
#' ) |>
#'   expand_trials()
#'
#' # load_expanded_data default behaviour returns all trial_periods and doesn't sample
#' load_expanded_data(trial_itt_csv)
#'
#' # load_expanded_data can subset the data before sampling
#' load_expanded_data(
#'   trial_itt_csv,
#'   p_control = 0.2,
#'   period = 1:20,
#'   subset_condition = "followup_time %in% 1:20 & x2 < 1",
#' )
#'
#' # delete after use
#' unlink(trial_itt_dir, recursive = TRUE)
setGeneric(
  "load_expanded_data",
  function(object, p_control = NULL, period = NULL, subset_condition = NULL, seed = NULL) {
    standardGeneric("load_expanded_data")
  }
)


#' Internal method to sample expanded data
#'
#' @param object An object of class [te_datastore-class].
#' @param p_control Probability of selecting a control.
#' @param period An integerish vector of non-zero length to select trial period(s) or `NULL` (default) to
#'  select all trial periods.
#' @param subset_condition A string or `NULL`.

#' @param seed An integer seed or `NULL` (default).
#'
#' @return A `data.frame` of class `data.table`.
#' @export
#'
#' @examples
#' # Data object normally created by [expand_trials]
#' datastore <- new("te_datastore_datatable", data = te_data_ex$data, N = 50139L)
#'
#' sample_expanded_data(datastore, period = 260:275, p_control = 0.2, seed = 123)
setGeneric(
  "sample_expanded_data",
  function(object, p_control, period = NULL, subset_condition = NULL, seed) standardGeneric("sample_expanded_data")
)


#' Method for fitting outcome models
#'
#' @param object A `te_outcome_fitter` object
#' @param data `data.frame` containing outcomes and covariates as defined in `formula`.
#' @param formula `formula` describing the model.
#' @param weights `numeric` vector of weights.
#'
#' @return An object of class `te_outcome_fitted`
#' @export
#' @keywords internal
setGeneric("fit_outcome_model", function(object, data, formula, weights = NULL) standardGeneric("fit_outcome_model"))

#' Method for fitting weight models
#'
#' @param object The object determining which method should be used, containing any slots containing user defined
#'   parameters.
#' @param data `data.frame` containing outcomes and covariates as defined in `formula`.
#' @param formula `formula` describing the model.
#' @param label A short string describing the model.
#'
#' @return An object of class `te_weights_fitted`
#' @export
#'
#' @examples
#' fitter <- stats_glm_logit(tempdir())
#' data(data_censored)
#' # Not usually called directly by a user
#' fitted <- fit_weights_model(
#'   object = fitter,
#'   data = data_censored,
#'   formula = 1 - censored ~ x1 + age_s + treatment,
#'   label = "Example model for censoring"
#' )
#' fitted
#' unlink(fitted@summary$save_path$path)
setGeneric("fit_weights_model", function(object, data, formula, label) standardGeneric("fit_weights_model"))


#' Specify the outcome model
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' The time-to-event model for `outcome` is specified with this method. Any adjustment terms can be specified.
#' For ITT and PP estimands the `treatment_var` is not specified as it is automatically defined as
#' `assigned_treatment`. Importantly, the modelling of "time" is specified in this model with arguments for
#' trial start time and follow up time within the trial.
#'
#' @param object A trial_sequence object
#' @param ... Parameters used by methods
#' @param treatment_var The treatment term, only used for "as treated" estimands. PP and ITT are fixed to use
#' "assigned_treatment".
#' @param adjustment_terms Formula terms for any covariates to adjust the outcome model.
#' @param followup_time_terms Formula terms for `followup_time`, the time period relative to the start of the trial.
#' @param trial_period_terms Formula terms for `trial_period`, the time period of the start of the trial.
#' @param model_fitter A `te_model_fitter` object, e.g. from `stats_glm_logit()`.
#'
#' @return A modified `object` with the `outcome_model` slot set
#' @export
#' @examples
#' trial_sequence("ITT") |>
#'   set_data(data_censored) |>
#'   set_outcome_model(
#'     adjustment_terms = ~age_s,
#'     followup_time_terms = ~ stats::poly(followup_time, degree = 2)
#'   )
#'
setGeneric("set_outcome_model", function(object, ...) standardGeneric("set_outcome_model"))


#' IPW Data Accessor and Setter
#'
#' `r lifecycle::badge('experimental')`
#'
#' Generic function to access and update the data used for inverse probability weighting.
#'
#' @param object `trial_sequence` object
#'
#' @return The data from the `@data` slot of `object` used for inverse probability weighting.
#' @export
#' @examples
#' ts <- trial_sequence("ITT")
#' ts <- set_data(ts, data_censored)
#' ipw_data(ts)
#' data.table::set(ipw_data(ts), j = "dummy", value = TRUE)
#'
#' # or with the setter method:
#' new_data <- ipw_data(ts)
#' new_data$x2sq <- new_data$x2^2
#' ipw_data(ts) <- new_data
setGeneric("ipw_data", function(object) standardGeneric("ipw_data"))

#' @rdname ipw_data
#' @param value `data.table` to replace and update in `@data`
#' @details
#' The setter method `ipw_data(object) <- value` does not perform the same checks and manipulations
#' as [set_data()]. To completely replace the data please use [set_data()]. This `ipw_data<-` method allows
#' small changes such as adding a new column.
#'
#' @export
setGeneric("ipw_data<-", function(object, value) standardGeneric("ipw_data<-"))


#' Outcome Data Accessor and Setter
#'
#' `r lifecycle::badge('experimental')`
#'
#' Generic function to outcome data
#'
#' @param object `trial_sequence` object
#'
#' @return The `object` with updated outcome data
#' @export
#' @examples
#' ts <- trial_sequence("ITT")
#' new_data <- data.table::data.table(vignette_switch_data[1:200, ])
#' new_data$weight <- 1
#' outcome_data(ts) <- new_data
setGeneric("outcome_data", function(object) standardGeneric("outcome_data"))

#' @rdname outcome_data
#' @param value `data.table` to replace and update in `@outcome_data`
#' @export
setGeneric("outcome_data<-", function(object, value) standardGeneric("outcome_data<-"))


#' Fit the marginal structural model for the sequence of emulated trials
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param object A `trial_sequence` object
#' @param weight_cols character vector of column names in expanded outcome dataset, ie `outcome_data(object)`. If
#' multiple columns are specified, the element wise product will be used. Specify `NULL` if no weight columns should be
#' used.
#' @param modify_weights a function to transform the weights (or `NULL` for no transformation).
#' Must take a numeric vector of weights and a vector of positive, finite weights of the same length.
#' See examples for some possible function definitions.
#'
#' Before the outcome marginal structural model can be fit, the outcome model must be specified with
#' [set_outcome_model()] and the data must be expanded into the trial sequence with [expand_trials()].
#'
#' The model is fit based on the `model_fitter` specified in [set_outcome_model] using the internal `fit_outcome_model`
#' method.
#' @return A modified `trial_sequence` object with updated `outcome_model` slot.
#' @export
#' @examples
#' trial_seq_object <- trial_sequence("ITT") |>
#'   set_data(data_censored) |>
#'   set_outcome_model(
#'     adjustment_terms = ~age_s,
#'     followup_time_terms = ~ stats::poly(followup_time, degree = 2)
#'   ) |>
#'   set_expansion_options(output = save_to_datatable(), chunk_size = 500) |>
#'   expand_trials() |>
#'   load_expanded_data()
#'
#' fit_msm(trial_seq_object)
#'
#' # Using modify_weights functions ----
#'
#' # returns a function that truncates weights to limits
#' limit_weight <- function(lower_limit, upper_limit) {
#'   function(w) {
#'     w[w > upper_limit] <- upper_limit
#'     w[w < lower_limit] <- lower_limit
#'     w
#'   }
#' }
#'
#' # calculate 1st and 99th percentile limits and truncate
#' p99_weight <- function(w) {
#'   p99 <- quantile(w, prob = c(0.01, 0.99), type = 1)
#'   limit_weight(p99[1], p99[2])(w)
#' }
#'
#' # set all weights to 1
#' all_ones <- function(w) {
#'   rep(1, length(w))
#' }
#'
#' fit_msm(trial_seq_object, modify_weights = limit_weight(0.01, 4))
#' fit_msm(trial_seq_object, modify_weights = p99_weight)
setGeneric("fit_msm", function(object,
                               weight_cols = c("weight", "sample_weight"),
                               modify_weights = NULL) {
  standardGeneric("fit_msm")
})

#' Predict marginal cumulative incidences with confidence intervals for a target trial population
#'
#' `r lifecycle::badge('stable')`
#' This function predicts the marginal cumulative incidences when a target trial population receives either the
#' treatment or non-treatment at baseline (for an intention-to-treat analysis) or either sustained treatment or
#' sustained non-treatment (for a per-protocol analysis). The difference between these cumulative incidences is the
#' estimated causal effect of treatment. Currently, the `predict` function only provides marginal intention-to-treat and
#' per-protocol effects, therefore it is only valid when `estimand_type = "ITT"` or `estimand_type = "PP"`.
#'
#' @param object Object from [trial_msm()] or [initiators()] or [trial_sequence].
#' @param newdata Baseline trial data that characterise the target trial population that marginal cumulative incidences
#'   or survival probabilities are predicted for.  `newdata` must have the same columns and formats of variables as in
#'   the fitted marginal structural model specified in [trial_msm()] or [initiators()]. If `newdata` contains rows with
#'   `followup_time > 0` these will be removed.
#' @param type Specify cumulative incidences or survival probabilities to be predicted. Either cumulative incidence
#'   (`"cum_inc"`) or survival probability (`"survival"`).
#' @param predict_times Specify the follow-up visits/times where the marginal cumulative incidences or survival
#'   probabilities are predicted.
#' @param conf_int Construct the point-wise 95-percent confidence intervals of cumulative incidences for the target
#'   trial population under treatment and non-treatment and their differences by simulating the parameters in the
#'   marginal structural model from a multivariate normal distribution with the mean equal to the marginal structural
#'   model parameter estimates and the variance equal to the estimated robust covariance matrix.
#' @param samples Number of samples used to construct the simulation-based confidence intervals.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A list of three data frames containing the cumulative incidences for each of the assigned treatment options
#'   (treatment and non-treatment) and the difference between them.
#' @name predict_marginal
#' @aliases predict
#' @export
setGeneric("predict", def = function(object, ...) standardGeneric("predict"))
