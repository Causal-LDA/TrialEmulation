# Save expanded data as CSV

**\[experimental\]**

## Usage

``` r
save_to_csv(path)
```

## Arguments

- path:

  Directory to save CSV files in. Must be empty.

## Value

A
[te_datastore_csv](https://causal-lda.github.io/TrialEmulation/reference/te_datastore_csv-class.md)
object.

## See also

Other save_to:
[`save_to_datatable()`](https://causal-lda.github.io/TrialEmulation/reference/save_to_datatable.md),
[`save_to_duckdb()`](https://causal-lda.github.io/TrialEmulation/reference/save_to_duckdb.md),
[`set_expansion_options()`](https://causal-lda.github.io/TrialEmulation/reference/set_expansion_options.md)

## Examples

``` r
csv_dir <- file.path(tempdir(), "expanded_trials_csv")
dir.create(csv_dir)
csv_datastore <- save_to_csv(path = csv_dir)

trial_to_expand <- trial_sequence("ITT") |>
  set_data(data = data_censored) |>
  set_expansion_options(output = csv_datastore, chunk_size = 500)

# Delete directory after use
unlink(csv_dir)
```
