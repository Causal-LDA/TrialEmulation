# Set switching weight model

**\[experimental\]**

## Usage

``` r
set_switch_weight_model(object, numerator, denominator, model_fitter, ...)

# S4 method for class 'trial_sequence'
set_switch_weight_model(
  object,
  numerator,
  denominator,
  model_fitter,
  eligible_wts_0 = NULL,
  eligible_wts_1 = NULL
)

# S4 method for class 'trial_sequence_ITT'
set_switch_weight_model(object, numerator, denominator, model_fitter)
```

## Arguments

- object:

  A
  [trial_sequence](https://causal-lda.github.io/TrialEmulation/reference/trial_sequence.md)
  object.

- numerator:

  Right hand side formula for the numerator model

- denominator:

  Right hand side formula for the denominator model

- model_fitter:

  A
  [te_model_fitter](https://causal-lda.github.io/TrialEmulation/reference/te_model_fitter-class.md)
  object, such as
  [stats_glm_logit](https://causal-lda.github.io/TrialEmulation/reference/stats_glm_logit.md)

- ...:

  Other arguments used by methods.

- eligible_wts_0:

  Name of column containing indicator (0/1) for observation to be
  excluded/included in weight model.

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

## Value

`object` is returned with `@switch_weights` set

## Examples

``` r
trial_sequence("PP") |>
  set_data(data = data_censored) |>
  set_switch_weight_model(
    numerator = ~ age_s + x1 + x3,
    denominator = ~ x3 + x4,
    model_fitter = stats_glm_logit(tempdir())
  )
#> Trial Sequence Object 
#> Estimand: Per-protocol 
#>  
#> Data: 
#>  - N: 321 observations from 89 patients 
#>         id period treatment    x1           x2    x3        x4   age      age_s
#>      <int>  <int>     <num> <num>        <num> <int>     <num> <num>      <num>
#>   1:     1      0         1     1  1.146148362     0 0.7342030    36 0.08333333
#>   2:     1      1         1     1  0.002200337     0 0.7342030    37 0.16666667
#>  ---                                                                           
#> 320:    99      1         1     0 -1.106480738     1 0.5752681    66 2.58333333
#> 321:    99      2         0     0  1.650478074     1 0.5752681    67 2.66666667
#>      outcome censored eligible time_on_regime
#>        <num>    <int>    <num>          <num>
#>   1:       0        0        1              0
#>   2:       0        0        0              1
#>  ---                                         
#> 320:       0        0        0              1
#> 321:       0        0        0              2
#>  
#> IPW for informative censoring: 
#>  - No weight model specified 
#>  
#> IPW for treatment switch censoring: 
#>  - Numerator formula: treatment ~ age_s + x1 + x3 
#>  - Denominator formula: treatment ~ x3 + x4 
#>  - Model fitter type: te_stats_glm_logit 
#>  - Weight models not fitted. Use calculate_weights() 
#>  
#> Sequence of Trials Data: 
#> - Use set_expansion_options() and expand_trials() to construct the sequence of trials dataset. 
#>  
#> Outcome model: 
#>  - Outcome model not specified. Use set_outcome_model() 
```
