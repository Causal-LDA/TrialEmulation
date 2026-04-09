# Expand trials

**\[experimental\]**

## Usage

``` r
expand_trials(object)
```

## Arguments

- object:

  A
  [trial_sequence](https://causal-lda.github.io/TrialEmulation/reference/trial_sequence.md)
  object

## Value

The
[trial_sequence](https://causal-lda.github.io/TrialEmulation/reference/trial_sequence.md)
`object` with a data set containing the full sequence of target trials.
The data is stored according to the options set with
[`set_expansion_options()`](https://causal-lda.github.io/TrialEmulation/reference/set_expansion_options.md)
and especially the `save_to_*` function.
