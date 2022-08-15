
#' Conditional Printing
#'
#' @param quiet (`logical`) Messages printed if `FALSE`
#' @param ... Passed to `print` function.
#'
#' @return Value of `print(...)` or `NULL`
#' @export
#' @examples
#' h_quiet_print(quiet = FALSE, "loud hello")
#' h_quiet_print(quiet = TRUE, "quiet hello")
h_quiet_print <- function(quiet, ...){
  if (isFALSE(quiet)) {
    print(...)
  }
}

#' Assert Monotonicity
#'
#' @param x numeric vector
#' @param increasing Test for increasong or decreasing.
#'
#' @return Nothing if check is successful, error otherwise.
#' @export
#'
#' @examples
#' assert_monotonic(1:3)
#' assert_monotonic(c(0.02, 0.0187, 0.005), FALSE)
assert_monotonic <- function(x, increasing = TRUE) {
  if (isTRUE(increasing) & !all(x == cummax(x))) {
    stop("Not monotonically increasing")
  } else if (isFALSE(increasing) & !all(x == cummin(x))) {
    stop("Not monotonically decreasing")
  }
}
