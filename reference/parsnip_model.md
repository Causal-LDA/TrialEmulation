# Fit outcome models using `parsnip` models

**\[experimental\]**

## Usage

``` r
parsnip_model(model_spec, save_path)
```

## Arguments

- model_spec:

  A `parsnip` model definition with `mode = "classification"`.

- save_path:

  Directory to save models. Set to `NA` if models should not be saved.

## Value

An object of class `te_parsnip_model` inheriting from
[te_model_fitter](https://causal-lda.github.io/TrialEmulation/reference/te_model_fitter-class.md)
which is used for dispatching methods for the fitting models.

## Details

Specify that the models should be fit using a classification model
specified with the `parsnip` package.

Warning: This functionality is experimental and not recommended for use
in analyses. \\sqrt{n}\\-consistency estimation and valid inference of
the parameters in marginal structural models for emulated trials
generally require that the weights for treatment switching and censoring
be estimated at parametric rates, which is generally not possible when
using data-adaptive estimation of high-dimensional regressions.
Therefore, we only recommend using
[`stats_glm_logit()`](https://causal-lda.github.io/TrialEmulation/reference/stats_glm_logit.md).

## See also

Other model_fitter:
[`stats_glm_logit()`](https://causal-lda.github.io/TrialEmulation/reference/stats_glm_logit.md),
[`te_model_fitter-class`](https://causal-lda.github.io/TrialEmulation/reference/te_model_fitter-class.md)

## Examples

``` r
if (FALSE) { # \dontrun{
if (
  requireNamespace("parsnip", quietly = TRUE) &&
    requireNamespace("rpart", quietly = TRUE)
) {
  # Use a decision tree model fitted with the rpart package
  parsnip_model(
    model_spec = parsnip::decision_tree(tree_depth = 30) |>
      set_mode("classification") |>
      set_engine("rpart"),
    save_path = tempdir()
  )
}
} # }
```
