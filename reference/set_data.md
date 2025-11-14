# Set the trial data

**\[experimental\]**

## Usage

``` r
set_data(object, data, ...)

# S4 method for class 'trial_sequence_ITT,data.frame'
set_data(
  object,
  data,
  id = "id",
  period = "period",
  treatment = "treatment",
  outcome = "outcome",
  eligible = "eligible"
)

# S4 method for class 'trial_sequence_AT,data.frame'
set_data(
  object,
  data,
  id = "id",
  period = "period",
  treatment = "treatment",
  outcome = "outcome",
  eligible = "eligible"
)

# S4 method for class 'trial_sequence_PP,data.frame'
set_data(
  object,
  data,
  id = "id",
  period = "period",
  treatment = "treatment",
  outcome = "outcome",
  eligible = "eligible"
)
```

## Arguments

- object:

  A
  [trial_sequence](https://causal-lda.github.io/TrialEmulation/reference/trial_sequence-class.md)
  object

- data:

  A `data.frame` containing all the required variables in the
  person-time format, i.e., the \<U+2018\>long\<U+2019\> format.

- ...:

  Other arguments used by methods internally.

- id:

  Name of the variable for identifiers of the individuals. Default is
  \<U+2018\>id\<U+2019\>.

- period:

  Name of the variable for the visit/period. Default is
  \<U+2018\>period\<U+2019\>.

- treatment:

  Name of the variable for the treatment indicator at that visit/period.
  Default is \<U+2018\>treatment\<U+2019\>.

- outcome:

  Name of the variable for the indicator of the outcome event at that
  visit/period. Default is \<U+2018\>outcome\<U+2019\>.

- eligible:

  Name of the variable for the indicator of eligibility for the target
  trial at that visit/period. Default is \<U+2018\>eligible\<U+2019\>.

## Value

An updated
[trial_sequence](https://causal-lda.github.io/TrialEmulation/reference/trial_sequence-class.md)
object with data

## Examples

``` r
data(trial_example)
trial_sequence("ITT") |>
  set_data(
    data = trial_example,
    id = "id",
    period = "period",
    eligible = "eligible",
    treatment = "treatment"
  )
#> Trial Sequence Object 
#> Estimand: Intention-to-treat 
#>  
#> Data: 
#>  - N: 48400 observations from 503 patients 
#>           id eligible period outcome treatment catvarA catvarB catvarC nvarA
#>        <int>    <int>  <int>   <int>     <int>   <int>   <int>   <int> <int>
#>     1:     1        1    261       0         0       2       1       0     1
#>     2:     1        1    262       0         0       2       1       0     1
#>    ---                                                                      
#> 48399:   503        0    271       0         0       0       2       0     0
#> 48400:   503        0    272       1         1       0       2       0     0
#>        nvarB nvarC time_on_regime
#>        <int> <int>          <num>
#>     1:    15    70              0
#>     2:    15    70              1
#>    ---                           
#> 48399:    63    64             37
#> 48400:    78    64              1
#>  
#> IPW for informative censoring: 
#>  - No weight model specified 
#>  
#> Sequence of Trials Data: 
#> - Use set_expansion_options() and expand_trials() to construct the sequence of trials dataset. 
#>  
#> Outcome model: 
#>  - Outcome model not specified. Use set_outcome_model() 
```
