# Method to save expanded data

This method is used internally by
[expand_trials](https://causal-lda.github.io/TrialEmulation/reference/expand_trials.md)
to save the data to the "datastore" defined in
[set_expansion_options](https://causal-lda.github.io/TrialEmulation/reference/set_expansion_options.md).

## Usage

``` r
save_expanded_data(object, data)

# S4 method for class 'te_datastore_datatable'
save_expanded_data(object, data)
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

## Value

An updated `object` with the data stored. Notably `object@N` should be
increased

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
#> Path: /tmp/RtmpcAcDkQ/csv_dir_18f328fcdec0 
#> Columns: id, trial_period, followup_time, outcome, weight, treatment, catvarA, catvarB, catvarC, nvarA, nvarB, nvarC, assigned_treatment 

# delete after use
unlink(temp_dir, recursive = TRUE)
```
