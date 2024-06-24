#' Summary methods
#'
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
#' save_expanded_data(datastore, vignette_switch_data)
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
#' select all files.
#'
#' @return A `data.frame` of class `data.table`.
#' @export
#'
#' @examples
#' # create a te_datastore_csv object
#' temp_dir <- tempfile("csv_dir_")
#' dir.create(temp_dir)
#' datastore <- save_to_csv(temp_dir)
#' data(vignette_switch_data)
#' expanded_csv_data <- save_expanded_data(datastore, vignette_switch_data)
#'
#' # read expanded data
#' read_expanded_data(expanded_csv_data)
#'
#' # delete after use
#' unlink(temp_dir, recursive = TRUE)
setGeneric("read_expanded_data", function(object, period = NULL) standardGeneric("read_expanded_data"))


#' Method for fitting outcome models
#'
#' @param object A `te_outcome_fitter` object
#' @param data `data.frame` containing outcomes and covariates as defined in `formula`.
#' @param formula `formula` describing the model.
#'
#' @return An object of class `te_outcome_fitted`
#' @export
#' @keywords internal
#' @examples
setGeneric("fit_outcome_model", function(object, data, formula, weights) standardGeneric("fit_outcome_model"))

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
