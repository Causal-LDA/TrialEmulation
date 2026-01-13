# Internal Methods

Various S4 methods which are not directly for use by users.

## Usage

``` r
# S4 method for class 'trial_sequence,data.frame'
set_data(
  object,
  data,
  censor_at_switch,
  id = "id",
  period = "period",
  treatment = "treatment",
  outcome = "outcome",
  eligible = "eligible"
)

# S4 method for class 'trial_sequence'
set_expansion_options(
  object,
  output,
  chunk_size = 0,
  first_period = 0,
  last_period = Inf,
  censor_at_switch
)

# S4 method for class 'trial_sequence'
expand_trials(object)
```

## Arguments

- object:

  A
  [trial_sequence](https://causal-lda.github.io/TrialEmulation/reference/trial_sequence-class.md)
  object

- data:

  A `data.frame` containing all the required variables in the
  person-time format, i.e., the \<U+2018\>long\<U+2019\> format.

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
