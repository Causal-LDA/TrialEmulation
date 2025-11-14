# Method for fitting outcome models

Method for fitting outcome models

## Usage

``` r
fit_outcome_model(object, data, formula, weights = NULL)
```

## Arguments

- object:

  A `te_outcome_fitter` object

- data:

  `data.frame` containing outcomes and covariates as defined in
  `formula`.

- formula:

  `formula` describing the model.

- weights:

  `numeric` vector of weights.

## Value

An object of class `te_outcome_fitted`
