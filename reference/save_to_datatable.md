# Save expanded data as a `data.table`

**\[experimental\]**

## Usage

``` r
save_to_datatable()
```

## See also

Other save_to:
[`save_to_csv()`](https://causal-lda.github.io/TrialEmulation/reference/save_to_csv.md),
[`save_to_duckdb()`](https://causal-lda.github.io/TrialEmulation/reference/save_to_duckdb.md),
[`set_expansion_options()`](https://causal-lda.github.io/TrialEmulation/reference/set_expansion_options.md)

## Examples

``` r
trial_to_expand <- trial_sequence("ITT") |>
  set_data(data = data_censored) |>
  set_expansion_options(output = save_to_datatable(), chunk_size = 500)
```
