# Robust Variance Calculation

This function performs the calculation of robust standard errors based
on variances estimated using
[`sandwich::vcovCL`](https://sandwich.R-Forge.R-project.org/reference/vcovCL.html).

## Usage

``` r
robust_calculation(model, data_id)
```

## Arguments

- model:

  The logistic regression model object.

- data_id:

  Values of id column of the data (ie `data[, id]`) to identify
  clusters.

## Value

A list with elements `summary`, a table with the model summary using the
robust variance estimates, and `matrix`, the `sandwich` covariance
matrix.
