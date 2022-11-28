
#' Summary methods for data_preparation objects
#'
#' @param object `TE_data_prep` object from `data_preparation()`
#' @param ... Not used
#' @export
summary.TE_data_prep <- function(object, ...) {
  cat("Number of observations in expanded data:", object$N, "\n")
  cat("First trial period:", object$min_period, "\n")
  cat("Last trial period:", object$max_period, "\n\n")

  has_cens_w <- length(object$censor_models) > 0
  has_switch_w <- length(object$switch_models) > 0

  if (has_cens_w || has_switch_w) {
    cat("Weight models\n\n")

    if (has_switch_w) {
      cat("Treatment switch models\n\n")
      lapply(names(object$switch_models), function(n) {
        cat("switch_models$", n, ": ", sep = "")
        print(object$switch_models[[n]], full = FALSE)
        cat("\n")
      })
    }
    if (has_cens_w) {
      if (length(object$censor_models)) {
        cat("Censoring models\n")
        lapply(names(object$censor_models), function(n) {
          cat("censor_models$", n, ":\n", sep = "")
          print(object$censor_models[[n]], full = FALSE)
          cat("\n")
        })
      }
    }
  }
}

#' @rdname summary.TE_data_prep
#' @export
summary.TE_data_prep_sep <- function(object, ...) {
  cat("Expanded Trial Emulation data\n\n")

  n_files <- length(object$data)
  cat("Expanded data saved in ", n_files, " csv file", if (n_files > 1) "s" else "", ":\n", sep = "")
  print(data.table(data = object$data), topn = 3, n = 5, col.names = "none")
  cat("\n\n")
  NextMethod()
}

#' @rdname summary.TE_data_prep
#' @export
summary.TE_data_prep_dt <- function(object, ...) {
  cat("Expanded Trial Emulation data\n\n")
  print(object$data, topn = 3, nrows = 3)
  cat("\n")
  NextMethod()
}


#' Print a Weight Summary Object
#'
#' @param object print.TE_weight_summary object
#' @param ... not used
#' @export
print.TE_weight_summary <- function(object, full = TRUE, ...) {
  cat(object$description, "\n\n")
  print.data.frame(object$summary, row.names = FALSE)
  cat("\n")
  if (full) {
    print.data.frame(object$fit_summary, row.names = FALSE)
    if (!is.null(object$path)) {
      cat("\n")
      cat("Object saved at \"", object$path, "\"", sep = "")
    }
  }
}
