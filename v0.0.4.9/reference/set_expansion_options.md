# Set expansion options

**\[experimental\]**

## Usage

``` r
set_expansion_options(object, ...)

# S4 method for class 'trial_sequence_ITT'
set_expansion_options(
  object,
  output,
  chunk_size,
  first_period = 0,
  last_period = Inf
)

# S4 method for class 'trial_sequence_PP'
set_expansion_options(
  object,
  output,
  chunk_size,
  first_period = 0,
  last_period = Inf
)

# S4 method for class 'trial_sequence_ITT'
set_expansion_options(
  object,
  output,
  chunk_size,
  first_period = 0,
  last_period = Inf
)
```

## Arguments

- object:

  A
  [trial_sequence](https://causal-lda.github.io/TrialEmulation/reference/trial_sequence.md)
  object

- ...:

  Arguments used in methods

- output:

  A
  [te_datastore](https://causal-lda.github.io/TrialEmulation/reference/te_datastore-class.md)
  object as created by a `save_to_*` function.

- chunk_size:

  An integer specifying the number of patients to include in each
  expansion iteration

- first_period:

  An integer specifying the first period to include in the expansion

- last_period:

  An integer specifying the last period to include in the expansion

## Value

`object` is returned with `@expansion` set

## See also

Other save_to:
[`save_to_csv()`](https://causal-lda.github.io/TrialEmulation/reference/save_to_csv.md),
[`save_to_datatable()`](https://causal-lda.github.io/TrialEmulation/reference/save_to_datatable.md),
[`save_to_duckdb()`](https://causal-lda.github.io/TrialEmulation/reference/save_to_duckdb.md)

## Examples

``` r
output_dir <- file.path(tempdir(check = TRUE), "expanded_data")
ITT_trial <- trial_sequence("ITT") |>
  set_data(data = data_censored) |>
  set_expansion_options(output = save_to_csv(output_dir), chunk_size = 500)

# Delete directory
unlink(output_dir, recursive = TRUE)
```
