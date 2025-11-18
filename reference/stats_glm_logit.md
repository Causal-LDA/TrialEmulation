# Fit outcome models using `stats::glm`

**\[experimental\]**

## Usage

``` r
stats_glm_logit(save_path)
```

## Arguments

- save_path:

  Directory to save models. Set to `NA` if models should not be saved.

## Value

An object of class `te_stats_glm_logit` inheriting from
[te_model_fitter](https://causal-lda.github.io/TrialEmulation/reference/te_model_fitter-class.md)
which is used for dispatching methods for the fitting models.

## Details

Specify that the pooled logistic regression outcome models should be fit
using [stats::glm](https://rdrr.io/r/stats/glm.html) with
`family = binomial(link = "logit")`.

Outcome models additional calculate robust variance estimates using
[`sandwich::vcovCL`](https://sandwich.R-Forge.R-project.org/reference/vcovCL.html).

## See also

Other model_fitter:
[`parsnip_model()`](https://causal-lda.github.io/TrialEmulation/reference/parsnip_model.md),
[`te_model_fitter-class`](https://causal-lda.github.io/TrialEmulation/reference/te_model_fitter-class.md)

## Examples

``` r
stats_glm_logit(save_path = tempdir())
#> An object of class "te_stats_glm_logit"
#> Slot "save_path":
#> [1] "/tmp/Rtmpq180rT"
#> 
```
