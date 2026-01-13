# Case-control sampling of expanded data for the sequence of emulated trials

**\[stable\]**

## Usage

``` r
case_control_sampling_trials(
  data_prep,
  p_control = NULL,
  subset_condition,
  sort = FALSE
)
```

## Arguments

- data_prep:

  Result from
  [`data_preparation()`](https://causal-lda.github.io/TrialEmulation/reference/data_preparation.md).

- p_control:

  Control sampling probability for selecting potential controls at each
  follow-up time of each trial.

- subset_condition:

  Expression used to [`subset()`](https://rdrr.io/r/base/subset.html)
  the trial data before case-control sampling.

- sort:

  Sort data before applying case-control sampling to make sure that the
  resulting data are identical when sampling from the expanded data
  created with `separate_files = TRUE` or `separate_files = FALSE`.

## Value

A `data.frame` or a [`split()`](https://rdrr.io/r/base/split.html)
`data.frame` if `length(p_control) > 1`. An additional column
`sample_weight` containing the sample weights will be added to the
result. These can be included in the models fit with
[`trial_msm()`](https://causal-lda.github.io/TrialEmulation/reference/trial_msm.md).

## Details

Perform case-control sampling of expanded data to create a data set of
reduced size and calculate sampling weights to be used in
[`trial_msm()`](https://causal-lda.github.io/TrialEmulation/reference/trial_msm.md).

## Examples

``` r
# If necessary reduce the number of threads for data.table
data.table::setDTthreads(2)

data("te_data_ex")
samples <- case_control_sampling_trials(te_data_ex, p_control = 0.01)
```
