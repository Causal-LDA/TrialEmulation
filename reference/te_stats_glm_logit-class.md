# Fit Models using logistic stats::glm

The classes and (internal) methods defined for using
[stats::glm](https://rdrr.io/r/stats/glm.html) to fit the logistic
regression models.

## Usage

``` r
# S4 method for class 'te_stats_glm_logit'
fit_weights_model(object, data, formula, label)

# S4 method for class 'te_stats_glm_logit'
fit_outcome_model(object, data, formula, weights = NULL)

# S4 method for class 'te_stats_glm_logit_outcome_fitted'
predict(
  object,
  newdata,
  predict_times,
  conf_int = TRUE,
  samples = 100,
  type = c("cum_inc", "survival")
)
```

## Arguments

- object:

  Object to dispatch method on

- data:

  `data.frame` containing outcomes and covariates as defined in
  `formula`.

- formula:

  `formula` describing the model.

- label:

  A short string describing the model.

- weights:

  `numeric` vector of weights.

- newdata:

  Baseline trial data that characterise the target trial population that
  marginal cumulative incidences or survival probabilities are predicted
  for. `newdata` must have the same columns and formats of variables as
  in the fitted marginal structural model specified in
  [`trial_msm()`](https://causal-lda.github.io/TrialEmulation/reference/trial_msm.md)
  or
  [`initiators()`](https://causal-lda.github.io/TrialEmulation/reference/initiators.md).
  If `newdata` contains rows with `followup_time > 0` these will be
  removed.

- predict_times:

  Specify the follow-up visits/times where the marginal cumulative
  incidences or survival probabilities are predicted.

- conf_int:

  Construct the point-wise 95-percent confidence intervals of cumulative
  incidences for the target trial population under treatment and
  non-treatment and their differences by simulating the parameters in
  the marginal structural model from a multivariate normal distribution
  with the mean equal to the marginal structural model parameter
  estimates and the variance equal to the estimated robust covariance
  matrix.

- samples:

  Number of samples used to construct the simulation-based confidence
  intervals.

- type:

  Specify cumulative incidences or survival probabilities to be
  predicted. Either cumulative incidence (`"cum_inc"`) or survival
  probability (`"survival"`).

## Functions

- `fit_weights_model(te_stats_glm_logit)`: Fit the weight models object
  via
  [calculate_weights](https://causal-lda.github.io/TrialEmulation/reference/calculate_weights.md)
  on `trial_sequence`

- `fit_outcome_model(te_stats_glm_logit)`: Fit the outcome model object
  via
  [fit_msm](https://causal-lda.github.io/TrialEmulation/reference/fit_msm.md)
  on `trial_sequence`

- `predict(te_stats_glm_logit_outcome_fitted)`: Predict from the fitted
  model object via
  [predict](https://causal-lda.github.io/TrialEmulation/reference/predict_marginal.md)
  on `trial_sequence`

## See also

Other model_fitter_classes:
[`te_parsnip_model-class`](https://causal-lda.github.io/TrialEmulation/reference/te_parsnip_model-class.md)
