#' Example of longitudinal data for sequential trial emulation
#'
#' A dataset containing the treatment, outcomes and other attributes of 503 patients for sequential trial emulation.
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

#' Example of expanded longitudinal data for sequential trial emulation
#'
#' This is the expanded dataset created in the `vignette("Getting-Started")` known as `switch_data`.
#'
#' @format A data frame with 1939053 rows and 7 variables:
#' \describe{
#'   \item{id}{patient identifier}
#'   \item{trial_period}{trial start time period}
#'   \item{followup_time}{follow up time within trial}
#'   \item{outcome}{indicator for outcome in this period, 1=event occurred, 0=no event}
#'   \item{treatment}{indicator for receiving treatment in this period, 1=treatment, 0=non-treatment}
#'   \item{assigned_treatment}{indicator for assigned treatment at baseline of the trial, 1=treatment, 0=non-treatment}
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

#' Example of a prepared data object
#'
#' A small example object from [data_preparation] used in examples.
#'  It is created with the following code:
#'
#' ```{r, eval = FALSE}
#' dat <- trial_example[trial_example$id < 200, ]
#'
#' te_data_ex <- data_preparation(
#' data = dat,
#'  outcome_cov = c("nvarA", "catvarA"),
#'  first_period = 260,
#'  last_period = 280
#' )
#' ```
#'
#' @seealso [te_model_ex]
"te_data_ex"

#' Example of a fitted marginal structural model object
#'
#' A small example object from [trial_msm] used in examples. It is created with the
#' following code:
#'
#' ```{r, eval = FALSE}
#' te_model_ex <- trial_msm(
#'  data = data_subset,
#'  outcome_cov = c("catvarA", "nvarA"),
#'  last_followup = 40,
#'  model_var = "assigned_treatment",
#'  include_followup_time = ~followup_time,
#'  include_trial_period = ~trial_period,
#'  use_sample_weights = FALSE,
#'  quiet = TRUE,
#'  glm_function = "glm"
#'  )
#'  ```
#'
#' @seealso [te_data_ex]
"te_model_ex"


#' Example of longitudinal data for sequential trial emulation containing censoring
#'
#' This data contains data from 89 patients followed for up to 19 periods.
#'
#' @format A data frame with 725 rows and 12 variables:
#' \describe{
#'   \item{id}{patient identifier}
#'   \item{period}{time period}
#'   \item{treatment}{indicator for receiving treatment in this period, 1=treatment, 0=non-treatment}
#'   \item{x1}{A time-varying categorical variable relating to treatment and the outcome}
#'   \item{x2}{A time-varying numeric variable relating to treatment and the outcome}
#'   \item{x3}{A fixed categorical variable relating to treatment and the outcome}
#'   \item{x4}{A fixed categorical variable relating to treatment and the outcome}
#'   \item{age}{patient age in years}
#'   \item{age_s}{patient age}
#'   \item{outcome}{indicator for outcome in this period, 1=event occurred, 0=no event}
#'   \item{censored}{indicator for patient being censored in this period, 1=censored, 0=not censored}
#'   \item{eligible}{indicator for eligibility for trial start in this period, 1=yes, 0=no}
#' }
#'
"data_censored"
