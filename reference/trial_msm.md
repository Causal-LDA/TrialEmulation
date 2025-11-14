# Fit the marginal structural model for the sequence of emulated trials

**\[stable\]**

## Usage

``` r
trial_msm(
  data,
  outcome_cov = ~1,
  estimand_type = c("ITT", "PP", "As-Treated"),
  model_var = NULL,
  first_followup = NA,
  last_followup = NA,
  analysis_weights = c("asis", "unweighted", "p99", "weight_limits"),
  weight_limits = c(0, Inf),
  include_followup_time = ~followup_time + I(followup_time^2),
  include_trial_period = ~trial_period + I(trial_period^2),
  where_case = NA,
  glm_function = c("glm", "parglm"),
  use_sample_weights = TRUE,
  quiet = FALSE,
  ...
)
```

## Arguments

- data:

  A `data.frame` containing all the required variables in the
  person-time format, i.e., the \`long' format.

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

- first_followup:

  First follow-up time/visit in the trials to be included in the
  marginal structural model for the outcome event.

- last_followup:

  Last follow-up time/visit in the trials to be included in the marginal
  structural model for the outcome event.

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

- include_followup_time:

  The model to include the follow up time/visit of the trial
  (`followup_time`) in the marginal structural model, specified as a RHS
  formula.

- include_trial_period:

  The model to include the trial period (`trial_period`) in the marginal
  structural model, specified as a RHS formula.

- where_case:

  Define conditions using variables specified in `where_var` when
  fitting a marginal structural model for a subgroup of the individuals.
  For example, if `where_var= "age"`, `where_case = "age >= 30"` will
  only fit the marginal structural model to the subgroup of individuals.
  who are 30 years old or above.

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

- use_sample_weights:

  Use case-control sampling weights in addition to inverse probability
  weights for treatment and censoring. `data` must contain a column
  `sample_weight`. The final weights used in the pooled logistic
  regression are calculated as `weight = weight * sample_weight`.

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

Object of class `TE_msm` containing

- model:

  a `glm` object

- robust:

  a list containing a summary table of estimated regression coefficients
  and the robust covariance matrix

- args:

  a list contain the parameters used to prepare and fit the model

## Details

Apply a weighted pooled logistic regression to fit the marginal
structural model for the sequence of emulated trials and calculates the
robust covariance matrix of parameter using the sandwich estimator.

The model formula is constructed by combining the arguments
`outcome_cov`, `model_var`, `include_followup_time`, and
`include_trial_period`.
