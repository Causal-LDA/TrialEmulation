# A wrapper function to perform data preparation and model fitting in a sequence of emulated target trials

**\[stable\]**

## Usage

``` r
initiators(
  data,
  id = "id",
  period = "period",
  treatment = "treatment",
  outcome = "outcome",
  eligible = "eligible",
  outcome_cov = ~1,
  estimand_type = c("ITT", "PP", "As-Treated"),
  model_var = NULL,
  switch_n_cov = ~1,
  switch_d_cov = ~1,
  first_period = NA,
  last_period = NA,
  first_followup = NA,
  last_followup = NA,
  use_censor_weights = FALSE,
  save_weight_models = FALSE,
  analysis_weights = c("asis", "unweighted", "p99", "weight_limits"),
  weight_limits = c(0, Inf),
  cense = NA,
  pool_cense = c("none", "both", "numerator"),
  cense_d_cov = ~1,
  cense_n_cov = ~1,
  include_followup_time = ~followup_time + I(followup_time^2),
  include_trial_period = ~trial_period + I(trial_period^2),
  eligible_wts_0 = NA,
  eligible_wts_1 = NA,
  where_var = NULL,
  where_case = NA,
  data_dir,
  glm_function = "glm",
  quiet = FALSE,
  ...
)
```

## Arguments

- data:

  A `data.frame` containing all the required variables in the
  person-time format, i.e., the \`long' format.

- id:

  Name of the variable for identifiers of the individuals. Default is
  \`id'.

- period:

  Name of the variable for the visit/period. Default is \`period'.

- treatment:

  Name of the variable for the treatment indicator at that visit/period.
  Default is \`treatment'.

- outcome:

  Name of the variable for the indicator of the outcome event at that
  visit/period. Default is \`outcome'.

- eligible:

  Name of the variable for the indicator of eligibility for the target
  trial at that visit/period. Default is \`eligible'.

- outcome_cov:

  A RHS formula with baseline covariates to be adjusted for in the
  marginal structural model for the emulated trials. Note that if a
  time-varying covariate is specified in `outcome_cov`, only its value
  at each of the trial baselines will be included in the expanded data.

- estimand_type:

  Specify the estimand for the causal analyses in the sequence of
  emulated trials. `estimand_type = "ITT"` will perform
  intention-to-treat analyses, where treatment switching after trial
  baselines are ignored. `estimand_type = "PP"` will perform
  per-protocol analyses, where individuals' follow-ups are artificially
  censored and inverse probability of treatment weighting is applied.
  `estimand_type = "As-Treated"` will fit a standard marginal structural
  model for all possible treatment sequences, where individuals'
  follow-ups are not artificially censored but treatment switching after
  trial baselines are accounted for by applying inverse probability of
  treatment weighting.

- model_var:

  Treatment variables to be included in the marginal structural model
  for the emulated trials. `model_var = "assigned_treatment"` will
  create a variable `assigned_treatment` that is the assigned treatment
  at the trial baseline, typically used for ITT and per-protocol
  analyses. `model_var = "dose"` will create a variable `dose` that is
  the cumulative number of treatments received since the trial baseline,
  typically used in as-treated analyses.

- switch_n_cov:

  A RHS formula to specify the logistic models for estimating the
  numerator terms of the inverse probability of treatment weights. A
  derived variable named `time_on_regime` containing the duration of
  time that the individual has been on the current
  treatment/non-treatment is available for use in these models.

- switch_d_cov:

  A RHS formula to specify the logistic models for estimating the
  denominator terms of the inverse probability of treatment weights.

- first_period:

  First time period to be set as trial baseline to start expanding the
  data.

- last_period:

  Last time period to be set as trial baseline to start expanding the
  data.

- first_followup:

  First follow-up time/visit in the trials to be included in the
  marginal structural model for the outcome event.

- last_followup:

  Last follow-up time/visit in the trials to be included in the marginal
  structural model for the outcome event.

- use_censor_weights:

  Require the inverse probability of censoring weights. If
  `use_censor_weights = TRUE`, then the variable name of the censoring
  indicator needs to be provided in the argument `cense`.

- save_weight_models:

  Save model objects for estimating the weights in `data_dir`.

- analysis_weights:

  Choose which type of weights to be used for fitting the marginal
  structural model for the outcome event.

  - `"asis"`: use the weights as calculated.

  - `"p99"`: use weights truncated at the 1st and 99th percentiles
    (based on the distribution of weights in the entire sample).

  - `"weight_limits"`: use weights truncated at the values specified in
    `weight_limits`.

  - `"unweighted"`: set all analysis weights to 1, even if treatment
    weights or censoring weights were calculated.

- weight_limits:

  Lower and upper limits to truncate weights, given as `c(lower, upper)`

- cense:

  Variable name for the censoring indicator. Required if
  `use_censor_weights = TRUE`.

- pool_cense:

  Fit pooled or separate censoring models for those treated and those
  untreated at the immediately previous visit. Pooling can be specified
  for the models for the numerator and denominator terms of the inverse
  probability of censoring weights. One of `"none"`, `"numerator"`, or
  `"both"` (default is `"none"` except when `estimand_type = "ITT"` then
  default is `"numerator"`).

- cense_d_cov:

  A RHS formula to specify the logistic models for estimating the
  denominator terms of the inverse probability of censoring weights.

- cense_n_cov:

  A RHS formula to specify the logistic models for estimating the
  numerator terms of the inverse probability of censoring weights.

- include_followup_time:

  The model to include the follow up time/visit of the trial
  (`followup_time`) in the marginal structural model, specified as a RHS
  formula.

- include_trial_period:

  The model to include the trial period (`trial_period`) in the marginal
  structural model, specified as a RHS formula.

- eligible_wts_0:

  See definition for `eligible_wts_1`

- eligible_wts_1:

  Exclude some observations when fitting the models for the inverse
  probability of treatment weights. For example, if it is assumed that
  an individual will stay on treatment for at least 2 visits, the first
  2 visits after treatment initiation by definition have a probability
  of staying on the treatment of 1.0 and should thus be excluded from
  the weight models for those who are on treatment at the immediately
  previous visit. Users can define a variable that indicates that these
  2 observations are ineligible for the weight model for those who are
  on treatment at the immediately previous visit and add the variable
  name in the argument `eligible_wts_1`. Similar definitions are applied
  to `eligible_wts_0` for excluding observations when fitting the models
  for the inverse probability of treatment weights for those who are not
  on treatment at the immediately previous visit.

- where_var:

  Specify the variable names that will be used to define subgroup
  conditions when fitting the marginal structural model for a subgroup
  of individuals. Need to specify jointly with the argument
  `where_case`.

- where_case:

  Define conditions using variables specified in `where_var` when
  fitting a marginal structural model for a subgroup of the individuals.
  For example, if `where_var= "age"`, `where_case = "age >= 30"` will
  only fit the marginal structural model to the subgroup of individuals.
  who are 30 years old or above.

- data_dir:

  Directory to save model objects in.

- glm_function:

  Specify which glm function to use for the marginal structural model
  from the `stats` or `parglm` packages. The default function is the
  `glm` function in the `stats` package. Users can also specify
  `glm_function = "parglm"` such that the `parglm` function in the
  `parglm` package can be used for fitting generalized linear models in
  parallel. The default control setting for `parglm` is `nthreads = 4`
  and `method = "FAST"`, where four cores and Fisher information are
  used for faster computation. Users can change the default control
  setting by passing the arguments `nthreads` and `method` in the
  `parglm.control` function of the `parglm` package, or alternatively,
  by passing a `control` argument with a list produced by
  `parglm.control(nthreads = , method = )`.

- quiet:

  Suppress the printing of progress messages and summaries of the fitted
  models.

- ...:

  Additional arguments passed to `glm_function`. This may be used to
  specify initial values of parameters or arguments to `control`. See
  [stats::glm](https://rdrr.io/r/stats/glm.html),
  [parglm::parglm](https://rdrr.io/pkg/parglm/man/parglm.html) and
  [`parglm::parglm.control()`](https://rdrr.io/pkg/parglm/man/parglm.control.html)
  for more information.

## Value

Returns the result of
[`trial_msm()`](https://causal-lda.github.io/TrialEmulation/reference/trial_msm.md)
from the expanded data. An object of class `TE_msm` containing

- model:

  a `glm` object

- robust:

  a list containing a summary table of estimated regression coefficients
  and the robust covariance matrix

## Details

An all-in-one analysis using a sequence of emulated target trials. This
provides a simplified interface to the main functions
[`data_preparation()`](https://causal-lda.github.io/TrialEmulation/reference/data_preparation.md)
and
[`trial_msm()`](https://causal-lda.github.io/TrialEmulation/reference/trial_msm.md).
