# Fit the marginal structural model for the sequence of emulated trials

**\[experimental\]**

## Usage

``` r
fit_msm(
  object,
  weight_cols = c("weight", "sample_weight"),
  modify_weights = NULL
)

# S4 method for class 'trial_sequence'
fit_msm(
  object,
  weight_cols = c("weight", "sample_weight"),
  modify_weights = NULL
)
```

## Arguments

- object:

  A `trial_sequence` object

- weight_cols:

  character vector of column names in expanded outcome dataset, ie
  `outcome_data(object)`. If multiple columns are specified, the element
  wise product will be used. Specify `NULL` if no weight columns should
  be used.

- modify_weights:

  a function to transform the weights (or `NULL` for no transformation).
  Must take a numeric vector of weights and a vector of positive, finite
  weights of the same length. See examples for some possible function
  definitions.

  Before the outcome marginal structural model can be fit, the outcome
  model must be specified with
  [`set_outcome_model()`](https://causal-lda.github.io/TrialEmulation/reference/set_outcome_model.md)
  and the data must be expanded into the trial sequence with
  [`expand_trials()`](https://causal-lda.github.io/TrialEmulation/reference/expand_trials.md).

  The model is fit based on the `model_fitter` specified in
  [set_outcome_model](https://causal-lda.github.io/TrialEmulation/reference/set_outcome_model.md)
  using the internal `fit_outcome_model` method.

## Value

A modified `trial_sequence` object with updated `outcome_model` slot.

## Examples

``` r
trial_seq_object <- trial_sequence("ITT") |>
  set_data(data_censored) |>
  set_outcome_model(
    adjustment_terms = ~age_s,
    followup_time_terms = ~ stats::poly(followup_time, degree = 2)
  ) |>
  set_expansion_options(output = save_to_datatable(), chunk_size = 500) |>
  expand_trials() |>
  load_expanded_data()

fit_msm(trial_seq_object)
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
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
#>      outcome censored eligible time_on_regime    wt
#>        <num>    <int>    <num>          <num> <num>
#>   1:       0        0        1              0     1
#>   2:       0        0        0              1     1
#>  ---                                               
#> 724:       0        0        0              1     1
#> 725:       1        0        0              2     1
#>  
#> IPW for informative censoring: 
#>  - No weight model specified 
#>  
#> Sequence of Trials Data: 
#> - Chunk size: 500 
#> - Censor at switch: FALSE 
#> - First period: 0 | Last period: Inf 
#>  
#> A TE Datastore Datatable object 
#> N: 1558 observations 
#>          id trial_period followup_time outcome weight treatment      age_s
#>       <int>        <int>         <int>   <num>  <num>     <num>      <num>
#>    1:     1            0             0       0      1         1 0.08333333
#>    2:     1            0             1       0      1         1 0.08333333
#>   ---                                                                     
#> 1557:    99            0             6       0      1         1 2.50000000
#> 1558:    99            0             7       1      1         0 2.50000000
#>       assigned_treatment
#>                    <num>
#>    1:                  1
#>    2:                  1
#>   ---                   
#> 1557:                  1
#> 1558:                  1
#>  
#> Outcome model: 
#> - Formula: outcome ~ assigned_treatment + age_s + stats::poly(followup_time, degree = 2) + trial_period + I(trial_period^2) 
#> - Treatment variable: assigned_treatment 
#> - Adjustment variables: age_s 
#> - Model fitter type: te_stats_glm_logit 
#>  
#> Model Summary: 
#>  
#>  term                                    estimate std.error statistic p.value
#>  (Intercept)                              -5.46    0.52     -10.60    3.1e-26
#>  assigned_treatment                        1.34    0.53       2.50    1.2e-02
#>  age_s                                     0.48    0.34       1.42    1.5e-01
#>  stats::poly(followup_time, degree = 2)1  -2.23   14.99      -0.15    8.8e-01
#>  stats::poly(followup_time, degree = 2)2 -20.02   14.45      -1.39    1.7e-01
#>  trial_period                              7.05    0.97       7.23    4.8e-13
#>  I(trial_period^2)                        -7.51    0.54     -13.96    2.8e-44
#>  conf.low conf.high
#>   -6.47   -4.4     
#>    0.29    2.4     
#>   -0.18    1.2     
#>  -31.60   27.1     
#>  -48.34    8.3     
#>    5.14    9.0     
#>   -8.57   -6.5     
#>  
#>  null.deviance df.null logLik AIC BIC deviance df.residual nobs
#>  160           1557    -68.4  151 188 137      1551        1558
#>  
#> Outcome data 
#> N: 1558 observations from 89 patients in 18 trial periods 
#> Periods: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 
#>          id trial_period followup_time outcome weight treatment      age_s
#>       <int>        <int>         <int>   <num>  <num>     <num>      <num>
#>    1:     1            0             0       0      1         1 0.08333333
#>    2:     1            0             1       0      1         1 0.08333333
#>   ---                                                                     
#> 1557:    99            0             6       0      1         1 2.50000000
#> 1558:    99            0             7       1      1         0 2.50000000
#>       assigned_treatment sample_weight     w
#>                    <num>         <num> <num>
#>    1:                  1             1     1
#>    2:                  1             1     1
#>   ---                                       
#> 1557:                  1             1     1
#> 1558:                  1             1     1

# Using modify_weights functions ----

# returns a function that truncates weights to limits
limit_weight <- function(lower_limit, upper_limit) {
  function(w) {
    w[w > upper_limit] <- upper_limit
    w[w < lower_limit] <- lower_limit
    w
  }
}

# calculate 1st and 99th percentile limits and truncate
p99_weight <- function(w) {
  p99 <- quantile(w, prob = c(0.01, 0.99), type = 1)
  limit_weight(p99[1], p99[2])(w)
}

# set all weights to 1
all_ones <- function(w) {
  rep(1, length(w))
}

fit_msm(trial_seq_object, modify_weights = limit_weight(0.01, 4))
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
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
#>      outcome censored eligible time_on_regime    wt
#>        <num>    <int>    <num>          <num> <num>
#>   1:       0        0        1              0     1
#>   2:       0        0        0              1     1
#>  ---                                               
#> 724:       0        0        0              1     1
#> 725:       1        0        0              2     1
#>  
#> IPW for informative censoring: 
#>  - No weight model specified 
#>  
#> Sequence of Trials Data: 
#> - Chunk size: 500 
#> - Censor at switch: FALSE 
#> - First period: 0 | Last period: Inf 
#>  
#> A TE Datastore Datatable object 
#> N: 1558 observations 
#>          id trial_period followup_time outcome weight treatment      age_s
#>       <int>        <int>         <int>   <num>  <num>     <num>      <num>
#>    1:     1            0             0       0      1         1 0.08333333
#>    2:     1            0             1       0      1         1 0.08333333
#>   ---                                                                     
#> 1557:    99            0             6       0      1         1 2.50000000
#> 1558:    99            0             7       1      1         0 2.50000000
#>       assigned_treatment
#>                    <num>
#>    1:                  1
#>    2:                  1
#>   ---                   
#> 1557:                  1
#> 1558:                  1
#>  
#> Outcome model: 
#> - Formula: outcome ~ assigned_treatment + age_s + stats::poly(followup_time, degree = 2) + trial_period + I(trial_period^2) 
#> - Treatment variable: assigned_treatment 
#> - Adjustment variables: age_s 
#> - Model fitter type: te_stats_glm_logit 
#>  
#> Model Summary: 
#>  
#>  term                                    estimate std.error statistic p.value
#>  (Intercept)                              -5.46    0.52     -10.60    3.1e-26
#>  assigned_treatment                        1.34    0.53       2.50    1.2e-02
#>  age_s                                     0.48    0.34       1.42    1.5e-01
#>  stats::poly(followup_time, degree = 2)1  -2.23   14.99      -0.15    8.8e-01
#>  stats::poly(followup_time, degree = 2)2 -20.02   14.45      -1.39    1.7e-01
#>  trial_period                              7.05    0.97       7.23    4.8e-13
#>  I(trial_period^2)                        -7.51    0.54     -13.96    2.8e-44
#>  conf.low conf.high
#>   -6.47   -4.4     
#>    0.29    2.4     
#>   -0.18    1.2     
#>  -31.60   27.1     
#>  -48.34    8.3     
#>    5.14    9.0     
#>   -8.57   -6.5     
#>  
#>  null.deviance df.null logLik AIC BIC deviance df.residual nobs
#>  160           1557    -68.4  151 188 137      1551        1558
#>  
#> Outcome data 
#> N: 1558 observations from 89 patients in 18 trial periods 
#> Periods: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 
#>          id trial_period followup_time outcome weight treatment      age_s
#>       <int>        <int>         <int>   <num>  <num>     <num>      <num>
#>    1:     1            0             0       0      1         1 0.08333333
#>    2:     1            0             1       0      1         1 0.08333333
#>   ---                                                                     
#> 1557:    99            0             6       0      1         1 2.50000000
#> 1558:    99            0             7       1      1         0 2.50000000
#>       assigned_treatment sample_weight     w
#>                    <num>         <num> <num>
#>    1:                  1             1     1
#>    2:                  1             1     1
#>   ---                                       
#> 1557:                  1             1     1
#> 1558:                  1             1     1
fit_msm(trial_seq_object, modify_weights = p99_weight)
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
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
#>      outcome censored eligible time_on_regime    wt
#>        <num>    <int>    <num>          <num> <num>
#>   1:       0        0        1              0     1
#>   2:       0        0        0              1     1
#>  ---                                               
#> 724:       0        0        0              1     1
#> 725:       1        0        0              2     1
#>  
#> IPW for informative censoring: 
#>  - No weight model specified 
#>  
#> Sequence of Trials Data: 
#> - Chunk size: 500 
#> - Censor at switch: FALSE 
#> - First period: 0 | Last period: Inf 
#>  
#> A TE Datastore Datatable object 
#> N: 1558 observations 
#>          id trial_period followup_time outcome weight treatment      age_s
#>       <int>        <int>         <int>   <num>  <num>     <num>      <num>
#>    1:     1            0             0       0      1         1 0.08333333
#>    2:     1            0             1       0      1         1 0.08333333
#>   ---                                                                     
#> 1557:    99            0             6       0      1         1 2.50000000
#> 1558:    99            0             7       1      1         0 2.50000000
#>       assigned_treatment
#>                    <num>
#>    1:                  1
#>    2:                  1
#>   ---                   
#> 1557:                  1
#> 1558:                  1
#>  
#> Outcome model: 
#> - Formula: outcome ~ assigned_treatment + age_s + stats::poly(followup_time, degree = 2) + trial_period + I(trial_period^2) 
#> - Treatment variable: assigned_treatment 
#> - Adjustment variables: age_s 
#> - Model fitter type: te_stats_glm_logit 
#>  
#> Model Summary: 
#>  
#>  term                                    estimate std.error statistic p.value
#>  (Intercept)                              -5.46    0.52     -10.60    3.1e-26
#>  assigned_treatment                        1.34    0.53       2.50    1.2e-02
#>  age_s                                     0.48    0.34       1.42    1.5e-01
#>  stats::poly(followup_time, degree = 2)1  -2.23   14.99      -0.15    8.8e-01
#>  stats::poly(followup_time, degree = 2)2 -20.02   14.45      -1.39    1.7e-01
#>  trial_period                              7.05    0.97       7.23    4.8e-13
#>  I(trial_period^2)                        -7.51    0.54     -13.96    2.8e-44
#>  conf.low conf.high
#>   -6.47   -4.4     
#>    0.29    2.4     
#>   -0.18    1.2     
#>  -31.60   27.1     
#>  -48.34    8.3     
#>    5.14    9.0     
#>   -8.57   -6.5     
#>  
#>  null.deviance df.null logLik AIC BIC deviance df.residual nobs
#>  160           1557    -68.4  151 188 137      1551        1558
#>  
#> Outcome data 
#> N: 1558 observations from 89 patients in 18 trial periods 
#> Periods: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 
#>          id trial_period followup_time outcome weight treatment      age_s
#>       <int>        <int>         <int>   <num>  <num>     <num>      <num>
#>    1:     1            0             0       0      1         1 0.08333333
#>    2:     1            0             1       0      1         1 0.08333333
#>   ---                                                                     
#> 1557:    99            0             6       0      1         1 2.50000000
#> 1558:    99            0             7       1      1         0 2.50000000
#>       assigned_treatment sample_weight     w
#>                    <num>         <num> <num>
#>    1:                  1             1     1
#>    2:                  1             1     1
#>   ---                                       
#> 1557:                  1             1     1
#> 1558:                  1             1     1
```
