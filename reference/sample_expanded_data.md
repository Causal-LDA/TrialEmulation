# Internal method to sample expanded data

Internal method to sample expanded data

## Usage

``` r
sample_expanded_data(
  object,
  p_control,
  period = NULL,
  subset_condition = NULL,
  seed
)

# S4 method for class 'te_datastore'
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
  [te_datastore](https://causal-lda.github.io/TrialEmulation/reference/te_datastore-class.md).

- p_control:

  Probability of selecting a control.

- period:

  An integerish vector of non-zero length to select trial period(s) or
  `NULL` (default) to select all trial periods.

- subset_condition:

  A string or `NULL`.

- seed:

  An integer seed or `NULL` (default).

## Value

A `data.frame` of class `data.table`.

## Examples

``` r
# Data object normally created by [expand_trials]
datastore <- new("te_datastore_datatable", data = te_data_ex$data, N = 50139L)

sample_expanded_data(datastore, period = 260:275, p_control = 0.2, seed = 123)
#>          id trial_period followup_time outcome weight treatment nvarA catvarA
#>       <int>        <int>         <int>   <num>  <num>     <int> <int>  <fctr>
#>    1:   101          260             0       0      1         0     0       0
#>    2:   127          260             0       0      1         0     1       0
#>    3:    94          260             0       0      1         0     0       1
#>    4:    13          260             0       0      1         1     1       0
#>    5:   108          261             0       0      1         0     1       0
#>   ---                                                                        
#> 7717:     5          261           135       0      1         1     0       0
#> 7718:    24          261           135       0      1         1     0       2
#> 7719:     5          260           136       0      1         1     0       0
#> 7720:    24          260           136       0      1         1     0       2
#> 7721:   103          260           136       0      1         0     0       0
#>       assigned_treatment sample_weight
#>                    <int>         <num>
#>    1:                  0             5
#>    2:                  0             5
#>    3:                  0             5
#>    4:                  1             5
#>    5:                  0             5
#>   ---                                 
#> 7717:                  0             5
#> 7718:                  0             5
#> 7719:                  0             5
#> 7720:                  0             5
#> 7721:                  0             5
```
