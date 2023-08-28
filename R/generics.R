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
  print(data.table(data = object$data), topn = 3, n = 5, col.names = "none", ...)
  cat("\n\n")
  NextMethod()
}

#' @rdname summary_TE
#' @export
summary.TE_data_prep_dt <- function(object, ...) {
  cat("Expanded Trial Emulation data\n\n")
  print(object$data, topn = 3, nrows = 3, ...)
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



#' Print a Weight Summary Object
#'
#' @param x print.TE_weight_summary object.
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


#' Extract Weights
#'
#' @param object Object to extract weights from
#' @param ... Not used.
#'
#' @export
#' @importFrom stats weights
#' @returns Weights extracted from `object` as a numeric vector.
#' @rdname weights_TE
weights.TE_data_prep_dt <- function(object, ...) {
  object$data[["weight"]]
}

#' @export
#' @rdname weights_TE
weights.TE_data_prep_sep <- function(object, ...) {
  warning(
    "weights() not supported when data prepared with separate_files=TRUE.",
    "Sample data first with case_control_sampling_trials()"
  )
}
