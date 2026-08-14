# Save expanded data to `DuckDB`

**\[experimental\]**

## Usage

``` r
save_to_duckdb(path)
```

## Arguments

- path:

  Directory to save `DuckDB` database file in.

## Value

A
[te_datastore_duckdb](https://causal-lda.github.io/TrialEmulation/reference/te_datastore_duckdb-class.md)
object.

## See also

Other save_to:
[`save_to_csv()`](https://causal-lda.github.io/TrialEmulation/reference/save_to_csv.md),
[`save_to_datatable()`](https://causal-lda.github.io/TrialEmulation/reference/save_to_datatable.md),
[`set_expansion_options()`](https://causal-lda.github.io/TrialEmulation/reference/set_expansion_options.md)

## Examples

``` r
if (require(duckdb)) {
  duckdb_dir <- file.path(tempdir(), "expanded_trials_duckdb")

  trial_to_expand <- trial_sequence("ITT") |>
    set_data(data = data_censored) |>
    set_expansion_options(output = save_to_duckdb(path = duckdb_dir), chunk_size = 500)

  # Delete directory after use
  unlink(duckdb_dir)
}
#> Loading required package: duckdb
#> Loading required package: DBI
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpEaIeEK/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
```
