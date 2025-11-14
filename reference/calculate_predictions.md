# Calculate and transform predictions

Calculate and transform predictions

## Usage

``` r
calculate_predictions(
  newdata,
  model,
  treatment_values,
  pred_fun,
  coefs_mat,
  matrix_n_col
)
```

## Arguments

- newdata:

  New data to predict outcome

- model:

  GLM object

- treatment_values:

  Named vector of value to insert into `assigned_treatment` column

- pred_fun:

  Function to transform prediction matrix

- coefs_mat:

  Matrix of coefficients corresponding to `model_matrix`.

- matrix_n_col:

  Expected number of column after prediction.

## Value

A matrix with transformed predicted values. Number of columns
corresponds to the number of rows of `coefs_mat`
