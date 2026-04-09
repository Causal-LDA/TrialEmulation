# Method to read expanded data

This method is used on
[te_datastore](https://causal-lda.github.io/TrialEmulation/reference/te_datastore-class.md)
objects to read selected data and return one `data.table`.

## Usage

``` r
read_expanded_data(object, period = NULL, subset_condition = NULL)

# S4 method for class 'te_datastore_datatable'
read_expanded_data(object, period = NULL, subset_condition = NULL)
```

## Arguments

- object:

  An object of class
  [te_datastore](https://causal-lda.github.io/TrialEmulation/reference/te_datastore-class.md).

- period:

  An integerish vector of non-zero length to select trial period(s) or
  `NULL` (default) to select all files.

- subset_condition:

  A string of length 1 or `NULL` (default).

## Value

A `data.frame` of class `data.table`.

## Examples

``` r
# create a te_datastore_csv object and save some data
temp_dir <- tempfile("csv_dir_")
dir.create(temp_dir)
datastore <- save_to_csv(temp_dir)
data(vignette_switch_data)
expanded_csv_data <- save_expanded_data(datastore, vignette_switch_data[1:200, ])

# read expanded data
read_expanded_data(expanded_csv_data)
#>         id trial_period followup_time outcome weight treatment catvarA catvarB
#>      <int>        <int>         <int>   <int>  <int>     <int>   <int>   <int>
#>   1:     1          261             0       0      1         0       2       1
#>   2:     1          261             1       0      1         0       2       1
#>   3:     1          261             2       0      1         0       2       1
#>   4:     1          261             3       0      1         0       2       1
#>   5:     1          261             4       0      1         0       2       1
#>  ---                                                                          
#> 196:     1          277             1       0      1         0       1       1
#> 197:     1          277             2       0      1         0       1       1
#> 198:     1          278             0       0      1         0       1       1
#> 199:     1          278             1       0      1         0       1       1
#> 200:     1          279             0       0      1         0       1       1
#>      catvarC nvarA nvarB nvarC assigned_treatment
#>        <int> <int> <int> <int>              <int>
#>   1:       0     1    15    70                  0
#>   2:       0     1    15    70                  0
#>   3:       0     1    15    70                  0
#>   4:       0     1    15    70                  0
#>   5:       0     1    15    70                  0
#>  ---                                             
#> 196:       0     0    10    72                  0
#> 197:       0     0    10    72                  0
#> 198:       1     0    10    72                  0
#> 199:       1     0    10    72                  0
#> 200:       1     0    11    72                  0

# delete after use
unlink(temp_dir, recursive = TRUE)
```
