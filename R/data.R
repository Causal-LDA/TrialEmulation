#' Example of longitundinal data for randomised trial emulation
#'
#' A dataset containing the treatment, outcomes and other attributes of 503 patients for randomised trial emulation.
#' See `vignette("Getting-Started")`.
#'
#'
#' @format A data frame with 48400 rows and 11 variables:
#' \describe{
#'   \item{id}{patient identifier}
#'   \item{eligible}{eligible for trial start in this period, 1=yes, 0=no}
#'   \item{period}{time period}
#'   \item{outcome}{indicator for outcome in this period, 1=event occurred, 0=no event}
#'   \item{treatment}{indicator for receiving treatment in this period, 1=treatment, 0=no treatment}
#'   \item{catvarA}{A categorical variable relating to treatment and the outcome}
#'   \item{catvarB}{A categorical variable relating to treatment and the outcome}
#'   \item{catvarC}{A categorical variable relating to treatment and the outcome}
#'   \item{nvarA}{A numerical variable relating to treatment and the outcome}
#'   \item{nvarB}{A numerical variable relating to treatment and the outcome}
#'   \item{nvarC}{A numerical variable relating to treatment and the outcome}
#' }
#'
"trial_example"

#' Example of expanded longitundinal data for randomised trial emulation
#'
#' This is the expanded dataset created in the `vignette("Getting-Started")` known as `switch_data`.
#'
#' @format A data frame with 1939053 rows and 7 variables:
#' \describe{
#'   \item{id}{patient identifier}
#'   \item{for_period}{trial start time period}
#'   \item{followup_time}{follow up time within trial}
#'   \item{outcome}{indicator for outcome in this period, 1=event occurred, 0=no event}
#'   \item{treatment}{indicator for receiving treatment in this period, 1=treatment, 0=no treatment}
#'   \item{assigned_treatment}{indicator for assigned treatment at baseline of the trial, 1=treatment, 0=no treatment}
#'   \item{weight}{weights for use with model fitting}
#'   \item{catvarA}{A categorical variable relating to treatment and the outcome}
#'   \item{catvarB}{A categorical variable relating to treatment and the outcome}
#'   \item{catvarC}{A categorical variable relating to treatment and the outcome}
#'   \item{nvarA}{A numerical variable relating to treatment and the outcome}
#'   \item{nvarB}{A numerical variable relating to treatment and the outcome}
#'   \item{nvarC}{A numerical variable relating to treatment and the outcome}
#' }
#'
"vignette_switch_data"
