# te_datastore_duckdb, functions and methods

This method is used internally by
[expand_trials](https://causal-lda.github.io/TrialEmulation/reference/expand_trials.md)
to save the data to the "datastore" defined in
[set_expansion_options](https://causal-lda.github.io/TrialEmulation/reference/set_expansion_options.md).

## Usage

``` r
# S4 method for class 'te_datastore_duckdb'
save_expanded_data(object, data)

# S4 method for class 'te_datastore_duckdb'
read_expanded_data(object, period = NULL, subset_condition = NULL)

# S4 method for class 'te_datastore_duckdb'
sample_expanded_data(
  object,
  p_control,
  period = NULL,
  subset_condition = NULL,
  seed
)
```

## Arguments

- object:

  An object of class
  [te_datastore](https://causal-lda.github.io/TrialEmulation/reference/te_datastore-class.md)
  or a child class.

- data:

  A data frame containing the expanded trial data. The columns
  `trial_period` and `id` are present, which may be used in methods to
  save the data in an optimal way, such as with indexes, keys or
  separate files.

- period:

  An integerish vector of non-zero length to select trial period(s) or
  `NULL` (default) to select all files.

- subset_condition:

  A string of length 1 or `NULL` (default).

- p_control:

  Probability of selecting a control.

- seed:

  An integer seed or `NULL` (default).

## Value

A 'te_datastore_duckdb' object.

## Slots

- `path`:

  Path to the duckdb file containing the data.

- `table`:

  .

- `con`:

  S4 object of class duckdb_connection.

## Examples

``` r
temp_dir <- tempfile("csv_dir_")
dir.create(temp_dir)
datastore <- save_to_csv(temp_dir)
data(vignette_switch_data)
save_expanded_data(datastore, vignette_switch_data[1:200, ])
#> A TE Datastore CSV object 
#> N: 200 observations 
#> Periods: 261 262 263 264 265 266 267 268 269 270 271 272 273 274 275 276 277 278 279 
#> Path: /tmp/Rtmpq180rT/csv_dir_18ef74c70fea 
#> Columns: id, trial_period, followup_time, outcome, weight, treatment, catvarA, catvarB, catvarC, nvarA, nvarB, nvarC, assigned_treatment 

# delete after use
unlink(temp_dir, recursive = TRUE)
```
