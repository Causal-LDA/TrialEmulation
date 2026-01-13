# Fit Models using parsnip

The classes and (internal) methods defined for using parsnip to fit the
weight models.

## Usage

``` r
# S4 method for class 'te_parsnip_model'
fit_weights_model(object, data, formula, label)
```

## Arguments

- object:

  The object determining which method should be used, containing any
  slots containing user defined parameters.

- data:

  `data.frame` containing outcomes and covariates as defined in
  `formula`.

- formula:

  `formula` describing the model.

- label:

  A short string describing the model.

## Functions

- `fit_weights_model(te_parsnip_model)`: Fit the weight models object
  via
  [calculate_weights](https://causal-lda.github.io/TrialEmulation/reference/calculate_weights.md)
  on `trial_sequence`

## Slots

- `model_spec`:

  A model specification defined with the `parsnip` package.

## See also

Other model_fitter_classes:
[`te_stats_glm_logit-class`](https://causal-lda.github.io/TrialEmulation/reference/te_stats_glm_logit-class.md)
