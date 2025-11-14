# Create a sequence of emulated target trials object

**\[experimental\]**

## Usage

``` r
trial_sequence(estimand, ...)
```

## Arguments

- estimand:

  The name of the estimand for this analysis, either one of `"ITT"`,
  `"PP"`, `"AT"` for intention-to-treat, per-protocol, as-treated
  estimands respectively, or the name of a class extending
  [trial_sequence](https://causal-lda.github.io/TrialEmulation/reference/trial_sequence-class.md)

- ...:

  Other parameters used when creating object

## Value

An estimand specific trial sequence object

## Examples

``` r
trial_sequence("ITT")
#> Trial Sequence Object 
#> Estimand: Intention-to-treat 
#>  
#> Data: 
#>  - No data has been set. Use set_data() 
#>  
#> IPW for informative censoring: 
#>  - No weight model specified 
#>  
#> Outcome model: 
#>  - Outcome model not specified. Use set_outcome_model() 
```
