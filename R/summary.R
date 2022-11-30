
#' Summary methods
#' @rdname summary
#'
#' @param object Object to print summary
#' @param digits Number of digits to print. Passed to print methods such as [print.data.frame].
#' @param ... Additional arguments passed to print methods.
#' @export
summary.TE_data_prep <- function(object, digits = 4, ...) {
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
        print(object$switch_models[[n]], full = FALSE, digits = digits, ...)
        cat(console_line(), "\n")
      }
    }
    if (has_cens_w) {
      if (length(object$censor_models)) {
        cat_underline("Censoring models")
        for (n in names(object$censor_models)) {
          cat("censor_models$", n, ":\n", sep = "")
          print(object$censor_models[[n]], full = FALSE, digits = digits, ...)
          cat(console_line(), "\n")
        }
      }
    }
  }
}

#' @rdname summary
#' @export
summary.TE_data_prep_sep <- function(object, digits = 4, ...) {
  cat("Expanded Trial Emulation data\n\n")

  n_files <- length(object$data)
  cat("Expanded data saved in ", n_files, " csv file", if (n_files > 1) "s" else "", ":\n", sep = "")
  print(data.table(data = object$data), topn = 3, n = 5, col.names = "none", digits = digits, ...)
  cat("\n\n")
  NextMethod()
}

#' @rdname summary
#' @export
summary.TE_data_prep_dt <- function(object, digits = 4, ...) {
  cat("Expanded Trial Emulation data\n\n")
  print(object$data, topn = 3, nrows = 3, digits = digits, ...)
  cat("\n")
  NextMethod()
}

#' @export
#' @rdname summary
summary.TE_model <- function(object, digits = 4, ...) {
  cat("Trial Emulation Outcome Model\n\n")
  cat("Outcome model formula:\n")
  print(object$model$formula, showEnv = FALSE)
  cat("\n")
  cat("Coefficent summary (robust):\n")
  summary(object$robust, digits = digits, ...)

  object_name <- match.call()[["object"]]

  cat("\n")
  cat(object_name, "$model contains the fitted glm model object.\n", sep = "")
  cat(object_name, "$robust$matrix contains the full robust covariance matrix.\n", sep = "")
}

#' @export
#' @rdname summary
summary.TE_robust <- function(object, digits = 4, ...) {
  to_print <- object$summary
  to_print$p_value <- format.pval(to_print$p_value, digits = digits, ...)
  print.data.frame(to_print, digits = digits, row.names = FALSE, ...)
}



#' Print a Weight Summary Object
#'
#' @param x print.TE_weight_summary object.
#' @param full Print full or short summary.
#' @param digits Number of digits to print. Passed to [print.data.frame].
#' @param ... Arguments passed to [print.data.frame].
#' @export
print.TE_weight_summary <- function(x, full = TRUE, digits = 4, ...) {
  cat(x$description, "\n\n")
  print.data.frame(x$summary, row.names = FALSE, digits = digits, ...)
  cat("\n")
  if (full) {
    print.data.frame(x$fit_summary, row.names = FALSE, digits = digits, ...)
    if (!is.null(x$path)) {
      cat("\n")
      cat("Object saved at \"", x$path, "\"", sep = "")
    }
  }
}
