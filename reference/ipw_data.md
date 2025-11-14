# IPW Data Accessor and Setter

**\[experimental\]**

## Usage

``` r
ipw_data(object)

ipw_data(object) <- value

# S4 method for class 'trial_sequence'
ipw_data(object)

# S4 method for class 'trial_sequence'
ipw_data(object) <- value
```

## Arguments

- object:

  `trial_sequence` object

- value:

  `data.table` to replace and update in `@data`

## Value

The data from the `@data` slot of `object` used for inverse probability
weighting.

## Details

Generic function to access and update the data used for inverse
probability weighting.

The setter method `ipw_data(object) <- value` does not perform the same
checks and manipulations as
[`set_data()`](https://causal-lda.github.io/TrialEmulation/reference/set_data.md).
To completely replace the data please use
[`set_data()`](https://causal-lda.github.io/TrialEmulation/reference/set_data.md).
This `ipw_data<-` method allows small changes such as adding a new
column.

## Examples

``` r
ts <- trial_sequence("ITT")
ts <- set_data(ts, data_censored)
ipw_data(ts)
#> Key: <id>
#> Indices: <first>, <am_1>
#>         id period treatment    x1           x2    x3        x4   age      age_s
#>      <int>  <int>     <num> <num>        <num> <int>     <num> <num>      <num>
#>   1:     1      0         1     1  1.146148362     0 0.7342030    36 0.08333333
#>   2:     1      1         1     1  0.002200337     0 0.7342030    37 0.16666667
#>   3:     1      2         1     0 -0.481762418     0 0.7342030    38 0.25000000
#>   4:     1      3         1     0  0.007872396     0 0.7342030    39 0.33333333
#>   5:     1      4         1     1  0.216053715     0 0.7342030    40 0.41666667
#>  ---                                                                           
#> 721:    99      3         0     0 -0.747905701     1 0.5752681    68 2.75000000
#> 722:    99      4         0     0 -0.790056043     1 0.5752681    69 2.83333333
#> 723:    99      5         1     1  0.387429397     1 0.5752681    70 2.91666667
#> 724:    99      6         1     1 -0.033762356     1 0.5752681    71 3.00000000
#> 725:    99      7         0     0 -1.340496520     1 0.5752681    72 3.08333333
#>      outcome censored eligible time_of_event  first  am_1  cumA switch
#>        <num>    <int>    <num>         <num> <lgcl> <num> <num>  <num>
#>   1:       0        0        1          9999   TRUE     0     1      0
#>   2:       0        0        0          9999  FALSE     1     2      0
#>   3:       0        0        0          9999  FALSE     1     3      0
#>   4:       0        0        0          9999  FALSE     1     4      0
#>   5:       0        0        0          9999  FALSE     1     5      0
#>  ---                                                                  
#> 721:       0        0        0             7  FALSE     0     2      0
#> 722:       0        0        0             7  FALSE     0     2      0
#> 723:       0        0        0             7  FALSE     0     3      1
#> 724:       0        0        0             7  FALSE     1     4      0
#> 725:       1        0        0             7  FALSE     1     4      1
#>      regime_start time_on_regime eligible0 eligible1
#>             <int>          <num>     <num>     <num>
#>   1:            0              0         1         0
#>   2:            0              1         0         1
#>   3:            0              2         0         1
#>   4:            0              3         0         1
#>   5:            0              4         0         1
#>  ---                                                
#> 721:            2              1         1         0
#> 722:            2              2         1         0
#> 723:            5              3         1         0
#> 724:            5              1         0         1
#> 725:            7              2         0         1
data.table::set(ipw_data(ts), j = "dummy", value = TRUE)

# or with the setter method:
new_data <- ipw_data(ts)
new_data$x2sq <- new_data$x2^2
ipw_data(ts) <- new_data
```
