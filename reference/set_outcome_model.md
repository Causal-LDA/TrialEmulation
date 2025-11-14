# Specify the outcome model

**\[experimental\]**

The time-to-event model for `outcome` is specified with this method. Any
adjustment terms can be specified. For ITT and PP estimands the
`treatment_var` is not specified as it is automatically defined as
`assigned_treatment`. Importantly, the modelling of "time" is specified
in this model with arguments for trial start time and follow up time
within the trial.

## Usage

``` r
set_outcome_model(object, ...)

# S4 method for class 'trial_sequence'
set_outcome_model(
  object,
  treatment_var = ~0,
  adjustment_terms = ~1,
  followup_time_terms = ~followup_time + I(followup_time^2),
  trial_period_terms = ~trial_period + I(trial_period^2),
  model_fitter = stats_glm_logit(save_path = NA)
)

# S4 method for class 'trial_sequence_ITT'
set_outcome_model(
  object,
  adjustment_terms = ~1,
  followup_time_terms = ~followup_time + I(followup_time^2),
  trial_period_terms = ~trial_period + I(trial_period^2),
  model_fitter = stats_glm_logit(save_path = NA)
)

# S4 method for class 'trial_sequence_PP'
set_outcome_model(
  object,
  adjustment_terms = ~1,
  followup_time_terms = ~followup_time + I(followup_time^2),
  trial_period_terms = ~trial_period + I(trial_period^2),
  model_fitter = stats_glm_logit(save_path = NA)
)

# S4 method for class 'trial_sequence_AT'
set_outcome_model(
  object,
  treatment_var = "dose",
  adjustment_terms = ~1,
  followup_time_terms = ~followup_time + I(followup_time^2),
  trial_period_terms = ~trial_period + I(trial_period^2),
  model_fitter = stats_glm_logit(save_path = NA)
)
```

## Arguments

- object:

  A trial_sequence object

- ...:

  Parameters used by methods

- treatment_var:

  The treatment term, only used for "as treated" estimands. PP and ITT
  are fixed to use "assigned_treatment".

- adjustment_terms:

  Formula terms for any covariates to adjust the outcome model.

- followup_time_terms:

  Formula terms for `followup_time`, the time period relative to the
  start of the trial.

- trial_period_terms:

  Formula terms for `trial_period`, the time period of the start of the
  trial.

- model_fitter:

  A `te_model_fitter` object, e.g. from
  [`stats_glm_logit()`](https://causal-lda.github.io/TrialEmulation/reference/stats_glm_logit.md).

## Value

A modified `object` with the `outcome_model` slot set

## Examples

``` r
trial_sequence("ITT") |>
  set_data(data_censored) |>
  set_outcome_model(
    adjustment_terms = ~age_s,
    followup_time_terms = ~ stats::poly(followup_time, degree = 2)
  )
#> Trial Sequence Object 
#> Estimand: Intention-to-treat 
#>  
#> Data: 
#>  - N: 725 observations from 89 patients 
#>         id period treatment    x1           x2    x3        x4   age      age_s
#>      <int>  <int>     <num> <num>        <num> <int>     <num> <num>      <num>
#>   1:     1      0         1     1  1.146148362     0 0.7342030    36 0.08333333
#>   2:     1      1         1     1  0.002200337     0 0.7342030    37 0.16666667
#>  ---                                                                           
#> 724:    99      6         1     1 -0.033762356     1 0.5752681    71 3.00000000
#> 725:    99      7         0     0 -1.340496520     1 0.5752681    72 3.08333333
#>      outcome censored eligible time_on_regime
#>        <num>    <int>    <num>          <num>
#>   1:       0        0        1              0
#>   2:       0        0        0              1
#>  ---                                         
#> 724:       0        0        0              1
#> 725:       1        0        0              2
#>  
#> IPW for informative censoring: 
#>  - No weight model specified 
#>  
#> Sequence of Trials Data: 
#> - Use set_expansion_options() and expand_trials() to construct the sequence of trials dataset. 
#>  
#> Outcome model: 
#> - Formula: outcome ~ assigned_treatment + age_s + stats::poly(followup_time, degree = 2) + trial_period + I(trial_period^2) 
#> - Treatment variable: assigned_treatment 
#> - Adjustment variables: age_s 
#> - Model fitter type: te_stats_glm_logit 
#>  
#> Use fit_msm() to fit the outcome model 
#>  
```
