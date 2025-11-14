# Method for fitting weight models

Method for fitting weight models

## Usage

``` r
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

## Value

An object of class `te_weights_fitted`

## Examples

``` r
fitter <- stats_glm_logit(tempdir())
data(data_censored)
# Not usually called directly by a user
fitted <- fit_weights_model(
  object = fitter,
  data = data_censored,
  formula = 1 - censored ~ x1 + age_s + treatment,
  label = "Example model for censoring"
)
fitted
#> Model: Example model for censoring 
#>  
#>  term        estimate  std.error statistic p.value     
#>  (Intercept) 1.4373145 0.2417658 5.9450688 2.763407e-09
#>  x1          0.5292698 0.3213183 1.6471823 9.952059e-02
#>  age_s       1.2239608 0.1724601 7.0970652 1.274340e-12
#>  treatment   0.1017731 0.3009513 0.3381712 7.352342e-01
#>  
#>  null.deviance df.null logLik    AIC      BIC     deviance df.residual nobs
#>  404.2156      724     -167.8072 343.6143 361.959 335.6143 721         725 
#>  
#>  path                                  
#>  /tmp/RtmpfjhUdd/model_1e25212968ed.rds
#>  
unlink(fitted@summary$save_path$path)
```
