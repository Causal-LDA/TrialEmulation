# Set censoring weight model

**\[experimental\]**

## Usage

``` r
set_censor_weight_model(
  object,
  censor_event,
  numerator,
  denominator,
  pool_models = NULL,
  model_fitter
)

# S4 method for class 'trial_sequence'
set_censor_weight_model(
  object,
  censor_event,
  numerator,
  denominator,
  pool_models = c("none", "both", "numerator"),
  model_fitter = stats_glm_logit()
)

# S4 method for class 'trial_sequence_PP'
set_censor_weight_model(
  object,
  censor_event,
  numerator,
  denominator,
  pool_models = "none",
  model_fitter = stats_glm_logit()
)

# S4 method for class 'trial_sequence_ITT'
set_censor_weight_model(
  object,
  censor_event,
  numerator,
  denominator,
  pool_models = "numerator",
  model_fitter = stats_glm_logit()
)

# S4 method for class 'trial_sequence_AT'
set_censor_weight_model(
  object,
  censor_event,
  numerator,
  denominator,
  pool_models = "none",
  model_fitter = stats_glm_logit()
)
```

## Arguments

- object:

  trial_sequence.

- censor_event:

  string. Name of column containing censoring indicator.

- numerator:

  A RHS formula to specify the logistic models for estimating the
  numerator terms of the inverse probability of censoring weights.

- denominator:

  A RHS formula to specify the logistic models for estimating the
  denominator terms of the inverse probability of censoring weights.

- pool_models:

  Fit pooled or separate censoring models for those treated and those
  untreated at the immediately previous visit. Pooling can be specified
  for the models for the numerator and denominator terms of the inverse
  probability of censoring weights. One of "none", "numerator", or
  "both" (default is "none" except when estimand = "ITT" then default is
  "numerator").

- model_fitter:

  An object of class `te_model_fitter` which determines the method used
  for fitting the weight models. For logistic regression use
  [`stats_glm_logit()`](https://causal-lda.github.io/TrialEmulation/reference/stats_glm_logit.md).

## Value

`object` is returned with `@censor_weights` set

## Examples

``` r
trial_sequence("ITT") |>
  set_data(data = data_censored) |>
  set_censor_weight_model(
    censor_event = "censored",
    numerator = ~ age_s + x1 + x3,
    denominator = ~ x3 + x4,
    pool_models = "both",
    model_fitter = stats_glm_logit(save_path = tempdir())
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
#>  - Numerator formula: 1 - censored ~ age_s + x1 + x3 
#>  - Denominator formula: 1 - censored ~ x3 + x4 
#>  - Numerator and denominotor models are pooled across treatment arms. 
#>  - Model fitter type: te_stats_glm_logit 
#>  - Weight models not fitted. Use calculate_weights() 
#>  
#> Sequence of Trials Data: 
#> - Use set_expansion_options() and expand_trials() to construct the sequence of trials dataset. 
#>  
#> Outcome model: 
#>  - Outcome model not specified. Use set_outcome_model() 
```
