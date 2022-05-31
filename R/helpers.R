
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
