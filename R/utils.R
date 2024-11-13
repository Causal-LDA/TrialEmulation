#' Conditional Printing
#'
#' @param quiet (`logical`) Messages printed if `FALSE`
#' @param x Object to print.
#' @param ... Passed to `print` function.
#'
#' @return Value of `print(...)` or `NULL`
#' @noRd
#' @examples
#' quiet_print(quiet = FALSE, "loud hello")
#' quiet_print(quiet = TRUE, "quiet hello")
quiet_print <- function(quiet, x, ...) {
  if (isFALSE(quiet)) {
    print(x, ...)
  }
}


#' Conditional Messages
#'
#' @param quiet (`logical`) Messages printed if `FALSE`
#' @param x Object to print.
#' @param ... Passed to `message` function.
#'
#' @noRd
quiet_msg <- function(quiet, x, ...) {
  if (isFALSE(quiet)) {
    message(x, ...)
  }
}

#' Print a line
#'
#' @param quiet Print if `TRUE `
#' @noRd
quiet_line <- function(quiet) {
  quiet_msg(quiet, paste0(console_line(), "\n"))
}

console_line <- function(prop = 0.75) {
  strrep("-", prop * getOption("width"))
}

cat_underline <- function(text, newlines = 2) {
  cat(text, "\n", strrep("-", nchar(text)), strrep("\n", newlines), sep = "")
}

#' Print with timing statement
#'
#' @param quiet Print if `TRUE `,
#' @param x Message to print
#' @param proc_time Result of `system.time()`. Elapsed time will be extracted,
#' formatted for printing and `paste0()`ed to `x`.
#' @noRd
quiet_msg_time <- function(quiet, msg, proc_time) {
  time <- proc_time["elapsed"]
  time <- if (time < 10) sprintf("%0.1f s", time) else sprintf("%.5g s", time)
  quiet_msg(quiet, paste0(msg, time))
}

#' Assert Monotonicity
#'
#' @param x numeric vector
#' @param increasing Test for increasing or decreasing.
#'
#' @return Nothing if check is successful, error otherwise.
#' @noRd
#' @examples
#' TrialEmulation:::assert_monotonic(1:3)
#' TrialEmulation:::assert_monotonic(c(0.02, 0.0187, 0.005), FALSE)
assert_monotonic <- function(x, increasing = TRUE) {
  if (isTRUE(increasing) && !all(x == cummax(x))) {
    stop("Not monotonically increasing")
  } else if (isFALSE(increasing) && !all(x == cummin(x))) {
    stop("Not monotonically decreasing")
  }
}


#' Coerce to a Formula with RHS only
#'
#' @param x A formula or character vector.
#' @param add A collection to store assertion messages. See [checkmate::AssertCollection].
#'
#' @return A formula
#' @noRd
#' @examples
#' as_formula(c("age", "sex"))
as_formula <- function(x, add = NULL) {
  assert_multi_class(x, classes = c("formula", "character"), add = add)
  if (test_string(x, pattern = "~")) {
    x <- as.formula(x)
  } else if (is.character(x)) {
    x <- formula(paste("~", paste(x, collapse = " + ")))
  }
  formula.tools::lhs(x) <- NULL
  x
}

#' Add RHS Parts of Formulas
#'
#' @param f1 formula to extract right side
#' @param f2 formula to extract right side
#'
#' @return A formula of the form `~ rhs(f1) + rhs(f2)`
#' @noRd
#' @examples
#' TrialEmulation:::add_rhs(~ a + b, z ~ c + log(d))
#' # ~ a + b + c + log(d)
add_rhs <- function(f1, f2) {
  update.formula(f1, substitute(~ . + add, list(add = formula.tools::rhs(f2))))
}

#' Get variables from RHS of formula
#' @noRd
rhs_vars <- function(f) {
  setdiff(
    all.vars(f),
    formula.tools::lhs.vars(f)
  )
}

#' Extract Baseline Observations
#'
#' @param trial_file Path to an expanded trial csv file
#' @param baseline_file Path to csv to save baseline observations
#' @param quiet Don't print progress messages.
#'
#' @details
#' Reads `trial_file` and saves the observations with `followup_time == 0` to `baseline_file` csv.
#'
#' @returns The file path of the csv if successful.
#' @noRd
extract_baseline <- function(trial_file, baseline_file, quiet = TRUE) {
  # Dummy assignments for data.table
  followup_time <- NULL

  if (file.exists(trial_file)) {
    quiet_msg("Extracting baseline observations from ", trial_file)
    fwrite(fread(trial_file)[followup_time == 0, ], file = baseline_file)
    return(baseline_file)
  } else {
    assert_file_exists(trial_file)
  }
}

#' Concatenate and Print with Final NewLine
#'
#' @inheritParams base::cat
#'
#' @details Simply passes arguments to `cat` with an additional `"\n"` after all arguments.
#'
#' @returns None (invisible NULL)
#' @noRd
catn <- function(...) {
  cat(..., "\n")
}


#' Drop paths in snapshot tests
#'
#' @param x snapshot
#'
#' @return snapshot without paths
#' @noRd
drop_path <- function(x) {
  output <- sub("Path: [[:graph:]]*", "Path:", x)
  output
}
