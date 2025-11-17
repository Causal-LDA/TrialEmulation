# Method to read, subset and sample expanded data

**\[experimental\]**

## Usage

``` r
load_expanded_data(
  object,
  p_control = NULL,
  period = NULL,
  subset_condition = NULL,
  seed = NULL
)

# S4 method for class 'trial_sequence'
load_expanded_data(
  object,
  p_control = NULL,
  period = NULL,
  subset_condition = NULL,
  seed = NULL
)
```

## Arguments

- object:

  An object of class
  [trial_sequence](https://causal-lda.github.io/TrialEmulation/reference/trial_sequence-class.md).

- p_control:

  Probability of selecting a control, `NULL` for no sampling (default).

- period:

  An integerish vector of non-zero length to select trial period(s) or
  `NULL` (default) to select all trial periods.

- subset_condition:

  A string or `NULL` (default). `subset_condition` will be translated to
  a call (in case the expanded data is saved as a data.table or in the
  csv format) or to a SQL-query (in case the expanded data is saved as a
  duckdb file).

  The operators `"==", "!=", ">", ">=", "<", "<=", %in%", "&", "|"` are
  supported. Numeric vectors can be written as `c(1, 2, 3)` or `1:3`.
  Variables are not supported.

  *Note*: Make sure numeric vectors written as `1:3` are surrounded by
  spaces, e.g. `a %in% c( 1:4 , 6:9 )`, otherwise the code will fail.

- seed:

  An integer seed or `NULL` (default).

  *Note*: The same seed will return a different result depending on the
  class of the
  [te_datastore](https://causal-lda.github.io/TrialEmulation/reference/te_datastore-class.md)
  object contained in the
  [trial_sequence](https://causal-lda.github.io/TrialEmulation/reference/trial_sequence-class.md)
  object.

## Value

An updated
[trial_sequence](https://causal-lda.github.io/TrialEmulation/reference/trial_sequence-class.md)
object, the data is stored in slot `@outcome_data` as a
[te_outcome_data](https://causal-lda.github.io/TrialEmulation/reference/te_outcome_data-class.md)
object.

## Details

This method is used on
[trial_sequence](https://causal-lda.github.io/TrialEmulation/reference/trial_sequence-class.md)
objects to read, subset and sample expanded data.

## Examples

``` r
# create a trial_sequence-class object
trial_itt_dir <- file.path(tempdir(), "trial_itt")
dir.create(trial_itt_dir)
trial_itt <- trial_sequence(estimand = "ITT") |>
  set_data(data = data_censored) |>
  set_outcome_model(adjustment_terms = ~ x1 + x2)

trial_itt_csv <- set_expansion_options(
  trial_itt,
  output = save_to_csv(file.path(trial_itt_dir, "trial_csvs")),
  chunk_size = 500
) |>
  expand_trials()

# load_expanded_data default behaviour returns all trial_periods and doesn't sample
load_expanded_data(trial_itt_csv)
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
#> A TE Datastore CSV object 
#> N: 1558 observations 
#> Periods: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 
#> Path: /tmp/RtmpdaYz1N/trial_itt/trial_csvs 
#> Columns: id, trial_period, followup_time, outcome, weight, treatment, x1, x2, assigned_treatment 
#>  
#> Outcome model: 
#> - Formula: outcome ~ assigned_treatment + x1 + x2 + followup_time + I(followup_time^2) + trial_period + I(trial_period^2) 
#> - Treatment variable: assigned_treatment 
#> - Adjustment variables: x1 x2 
#> - Model fitter type: te_stats_glm_logit 
#>  
#> Use fit_msm() to fit the outcome model 
#>  
#> Outcome data 
#> N: 1558 observations from 89 patients in 18 trial periods 
#> Periods: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 
#>          id trial_period followup_time outcome weight treatment    x1       x2
#>       <int>        <int>         <int>   <int>  <int>     <int> <int>    <num>
#>    1:     1            0             0       0      1         1     1 1.146148
#>    2:     1            0             1       0      1         1     1 1.146148
#>   ---                                                                         
#> 1557:    54           17             1       0      1         0     0 1.846423
#> 1558:    54           17             2       0      1         0     0 1.846423
#>       assigned_treatment sample_weight
#>                    <int>         <num>
#>    1:                  1             1
#>    2:                  1             1
#>   ---                                 
#> 1557:                  1             1
#> 1558:                  1             1

# load_expanded_data can subset the data before sampling
load_expanded_data(
  trial_itt_csv,
  p_control = 0.2,
  period = 1:20,
  subset_condition = "followup_time %in% 1:20 & x2 < 1",
)
#> Warning: The following periods don't exist in the data and were omitted: 18, 19, 20
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
#> A TE Datastore CSV object 
#> N: 1558 observations 
#> Periods: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 
#> Path: /tmp/RtmpdaYz1N/trial_itt/trial_csvs 
#> Columns: id, trial_period, followup_time, outcome, weight, treatment, x1, x2, assigned_treatment 
#>  
#> Outcome model: 
#> - Formula: outcome ~ assigned_treatment + x1 + x2 + followup_time + I(followup_time^2) + trial_period + I(trial_period^2) 
#> - Treatment variable: assigned_treatment 
#> - Adjustment variables: x1 x2 
#> - Model fitter type: te_stats_glm_logit 
#>  
#> Use fit_msm() to fit the outcome model 
#>  
#> Outcome data 
#> N: 137 observations from 24 patients in 14 trial periods 
#> Periods: 1 2 3 4 5 6 7 8 9 10 11 12 14 15 
#> Subset condition: followup_time %in% 1:20 & x2 < 1 
#> Sampling control observations with probability: 0.2 
#>         id trial_period followup_time outcome weight treatment    x1         x2
#>      <int>        <int>         <int>   <int>  <int>     <int> <int>      <num>
#>   1:    27            1             1       0      1         0     0  0.6897105
#>   2:    88            1             1       0      1         0     1 -0.3808923
#>  ---                                                                           
#> 136:    54           14             3       0      1         1     1  0.5460615
#> 137:    54           15             4       0      1         0     0  0.6850722
#>      assigned_treatment sample_weight
#>                   <int>         <num>
#>   1:                  0             5
#>   2:                  0             5
#>  ---                                 
#> 136:                  0             5
#> 137:                  0             5

# delete after use
unlink(trial_itt_dir, recursive = TRUE)
```
