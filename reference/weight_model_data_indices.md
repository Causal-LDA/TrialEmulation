# Data used in weight model fitting

**\[experimental\]**

## Usage

``` r
weight_model_data_indices(
  object,
  type = c("switch", "censor"),
  model,
  set_col = NULL
)
```

## Arguments

- object:

  A
  [trial_sequence](https://causal-lda.github.io/TrialEmulation/reference/trial_sequence.md)
  object

- type:

  Select a censoring or switching model

- model:

  The model name

- set_col:

  A character string to specifying a new column to contain indicators
  for observations used in fitting this model.

## Value

If `set_col` is not specified a logical `data.table` column is returned.
Otherwise

## Examples

``` r
trial_pp <- trial_sequence("PP") |>
  set_data(data_censored) |>
  set_switch_weight_model(
    numerator = ~age,
    denominator = ~ age + x1 + x3,
    model_fitter = stats_glm_logit(tempdir())
  ) |>
  calculate_weights()
ipw_data(trial_pp)
#> Key: <id>
#> Indices: <am_1>, <treatment>
#>         id period treatment    x1           x2    x3        x4   age      age_s
#>      <int>  <int>     <num> <num>        <num> <int>     <num> <num>      <num>
#>   1:     1      0         1     1  1.146148362     0 0.7342030    36 0.08333333
#>   2:     1      1         1     1  0.002200337     0 0.7342030    37 0.16666667
#>   3:     1      2         1     0 -0.481762418     0 0.7342030    38 0.25000000
#>   4:     1      3         1     0  0.007872396     0 0.7342030    39 0.33333333
#>   5:     1      4         1     1  0.216053715     0 0.7342030    40 0.41666667
#>  ---                                                                           
#> 317:    98      2         1     0 -0.735240928     0 0.3174175    66 2.58333333
#> 318:    98      3         0     0 -0.425345479     0 0.3174175    67 2.66666667
#> 319:    99      0         1     1 -0.346377841     1 0.5752681    65 2.50000000
#> 320:    99      1         1     0 -1.106480738     1 0.5752681    66 2.58333333
#> 321:    99      2         0     0  1.650478074     1 0.5752681    67 2.66666667
#>      outcome censored eligible time_of_event  first  am_1  cumA switch
#>        <num>    <int>    <num>         <num> <lgcl> <num> <num>  <num>
#>   1:       0        0        1          9999   TRUE     0     1      0
#>   2:       0        0        0          9999  FALSE     1     2      0
#>   3:       0        0        0          9999  FALSE     1     3      0
#>   4:       0        0        0          9999  FALSE     1     4      0
#>   5:       0        0        0          9999  FALSE     1     5      0
#>  ---                                                                  
#> 317:       0        0        0            14  FALSE     1     3      0
#> 318:       0        0        0            14  FALSE     1     3      1
#> 319:       0        0        1             7   TRUE     0     1      0
#> 320:       0        0        0             7  FALSE     1     2      0
#> 321:       0        0        0             7  FALSE     1     2      1
#>      regime_start time_on_regime eligible0 eligible1        wt       p_n
#>             <int>          <num>     <num>     <num>     <num>     <num>
#>   1:            0              0         1         0 0.7964844 0.4706283
#>   2:            0              1         0         1 0.9274210 0.7174218
#>   3:            0              2         0         1 1.0750073 0.7126312
#>   4:            0              3         0         1 1.0761263 0.7077924
#>   5:            0              4         0         1 0.9234340 0.7029062
#>  ---                                                                    
#> 317:            0              2         0         1 1.1082753 0.5621482
#> 318:            3              3         0         1 0.8898716 0.5563530
#> 319:            0              0         1         0 0.8816779 0.2512156
#> 320:            0              1         0         1 1.0034178 0.5621482
#> 321:            2              2         0         1 0.9959067 0.5563530
#>            p_d       wtS
#>          <num>     <num>
#>   1: 0.5908820 0.7964844
#>   2: 0.7735665 0.9274210
#>   3: 0.6629082 1.0750073
#>   4: 0.6577225 1.0761263
#>   5: 0.7611872 0.9234340
#>  ---                    
#> 317: 0.5072279 1.1082753
#> 318: 0.5014484 0.8898716
#> 319: 0.2849290 0.8816779
#> 320: 0.5602335 1.0034178
#> 321: 0.5545296 0.9959067
show_weight_models(trial_pp)
#> Weight Models for Informative Censoring
#> ---------------------------------------
#> 
#> Weight Models for Treatment Switching
#> -------------------------------------
#> 
#> [[n1]]
#> Model: P(treatment = 1 | previous treatment = 1) for numerator 
#>  
#>  term        estimate    std.error  statistic p.value   
#>  (Intercept)  1.80162178 0.77463133  2.325780 0.02003031
#>  age         -0.02351116 0.01691961 -1.389581 0.16465623
#>  
#>  null.deviance df.null logLik    AIC      BIC      deviance df.residual nobs
#>  188.829       150     -93.43779 190.8756 196.9101 186.8756 149         151 
#>  
#>  path                                  
#>  /tmp/Rtmpq180rT/model_18ef565172bc.rds
#>  
#> [[d1]]
#> Model: P(treatment = 1 | previous treatment = 1) for denominator 
#>  
#>  term        estimate    std.error  statistic  p.value   
#>  (Intercept)  1.55485166 0.81706997  1.9029602 0.05704573
#>  age         -0.02312027 0.01696843 -1.3625460 0.17302562
#>  x1           0.52915871 0.43594855  1.2138100 0.22482028
#>  x3           0.21319587 0.35744378  0.5964459 0.55087740
#>  
#>  null.deviance df.null logLik    AIC      BIC      deviance df.residual nobs
#>  188.829       150     -92.54787 193.0957 205.1649 185.0957 147         151 
#>  
#>  path                                  
#>  /tmp/Rtmpq180rT/model_18ef7ee1c54d.rds
#>  
#> [[n0]]
#> Model: P(treatment = 1 | previous treatment = 0) for numerator 
#>  
#>  term        estimate    std.error  statistic p.value   
#>  (Intercept)  1.09212298 0.60582678  1.802698 0.07143559
#>  age         -0.03360404 0.01439482 -2.334453 0.01957201
#>  
#>  null.deviance df.null logLik    AIC      BIC      deviance df.residual nobs
#>  232.2705      169     -113.2746 230.5492 236.8208 226.5492 168         170 
#>  
#>  path                                  
#>  /tmp/Rtmpq180rT/model_18ef6157fe8d.rds
#>  
#> [[d0]]
#> Model: P(treatment = 1 | previous treatment = 0) for denominator 
#>  
#>  term        estimate    std.error  statistic  p.value   
#>  (Intercept)  1.03084683 0.63052714  1.6348969 0.10207067
#>  age         -0.03633255 0.01472576 -2.4672779 0.01361446
#>  x1           0.64473751 0.32346837  1.9932011 0.04623943
#>  x3          -0.23411026 0.32147930 -0.7282281 0.46647397
#>  
#>  null.deviance df.null logLik  AIC    BIC      deviance df.residual nobs
#>  232.2705      169     -111.03 230.06 242.6032 222.06   166         170 
#>  
#>  path                                  
#>  /tmp/Rtmpq180rT/model_18ef6eff045c.rds
#>  

# get logical column for own processing
i <- weight_model_data_indices(trial_pp, "switch", "d0")

# set column in data
weight_model_data_indices(trial_pp, "switch", "d0", set_col = "sw_d0")
#> Key: <id>
#> Indices: <am_1>, <treatment>
#>         id period treatment    x1           x2    x3        x4   age      age_s
#>      <int>  <int>     <num> <num>        <num> <int>     <num> <num>      <num>
#>   1:     1      0         1     1  1.146148362     0 0.7342030    36 0.08333333
#>   2:     1      1         1     1  0.002200337     0 0.7342030    37 0.16666667
#>   3:     1      2         1     0 -0.481762418     0 0.7342030    38 0.25000000
#>   4:     1      3         1     0  0.007872396     0 0.7342030    39 0.33333333
#>   5:     1      4         1     1  0.216053715     0 0.7342030    40 0.41666667
#>  ---                                                                           
#> 317:    98      2         1     0 -0.735240928     0 0.3174175    66 2.58333333
#> 318:    98      3         0     0 -0.425345479     0 0.3174175    67 2.66666667
#> 319:    99      0         1     1 -0.346377841     1 0.5752681    65 2.50000000
#> 320:    99      1         1     0 -1.106480738     1 0.5752681    66 2.58333333
#> 321:    99      2         0     0  1.650478074     1 0.5752681    67 2.66666667
#>      outcome censored eligible time_of_event  first  am_1  cumA switch
#>        <num>    <int>    <num>         <num> <lgcl> <num> <num>  <num>
#>   1:       0        0        1          9999   TRUE     0     1      0
#>   2:       0        0        0          9999  FALSE     1     2      0
#>   3:       0        0        0          9999  FALSE     1     3      0
#>   4:       0        0        0          9999  FALSE     1     4      0
#>   5:       0        0        0          9999  FALSE     1     5      0
#>  ---                                                                  
#> 317:       0        0        0            14  FALSE     1     3      0
#> 318:       0        0        0            14  FALSE     1     3      1
#> 319:       0        0        1             7   TRUE     0     1      0
#> 320:       0        0        0             7  FALSE     1     2      0
#> 321:       0        0        0             7  FALSE     1     2      1
#>      regime_start time_on_regime eligible0 eligible1        wt       p_n
#>             <int>          <num>     <num>     <num>     <num>     <num>
#>   1:            0              0         1         0 0.7964844 0.4706283
#>   2:            0              1         0         1 0.9274210 0.7174218
#>   3:            0              2         0         1 1.0750073 0.7126312
#>   4:            0              3         0         1 1.0761263 0.7077924
#>   5:            0              4         0         1 0.9234340 0.7029062
#>  ---                                                                    
#> 317:            0              2         0         1 1.1082753 0.5621482
#> 318:            3              3         0         1 0.8898716 0.5563530
#> 319:            0              0         1         0 0.8816779 0.2512156
#> 320:            0              1         0         1 1.0034178 0.5621482
#> 321:            2              2         0         1 0.9959067 0.5563530
#>            p_d       wtS  sw_d0
#>          <num>     <num> <lgcl>
#>   1: 0.5908820 0.7964844   TRUE
#>   2: 0.7735665 0.9274210  FALSE
#>   3: 0.6629082 1.0750073  FALSE
#>   4: 0.6577225 1.0761263  FALSE
#>   5: 0.7611872 0.9234340  FALSE
#>  ---                           
#> 317: 0.5072279 1.1082753  FALSE
#> 318: 0.5014484 0.8898716  FALSE
#> 319: 0.2849290 0.8816779   TRUE
#> 320: 0.5602335 1.0034178  FALSE
#> 321: 0.5545296 0.9959067  FALSE
weight_model_data_indices(trial_pp, "switch", "d1", set_col = "sw_d1")
#> Key: <id>
#> Indices: <am_1>, <treatment>
#>         id period treatment    x1           x2    x3        x4   age      age_s
#>      <int>  <int>     <num> <num>        <num> <int>     <num> <num>      <num>
#>   1:     1      0         1     1  1.146148362     0 0.7342030    36 0.08333333
#>   2:     1      1         1     1  0.002200337     0 0.7342030    37 0.16666667
#>   3:     1      2         1     0 -0.481762418     0 0.7342030    38 0.25000000
#>   4:     1      3         1     0  0.007872396     0 0.7342030    39 0.33333333
#>   5:     1      4         1     1  0.216053715     0 0.7342030    40 0.41666667
#>  ---                                                                           
#> 317:    98      2         1     0 -0.735240928     0 0.3174175    66 2.58333333
#> 318:    98      3         0     0 -0.425345479     0 0.3174175    67 2.66666667
#> 319:    99      0         1     1 -0.346377841     1 0.5752681    65 2.50000000
#> 320:    99      1         1     0 -1.106480738     1 0.5752681    66 2.58333333
#> 321:    99      2         0     0  1.650478074     1 0.5752681    67 2.66666667
#>      outcome censored eligible time_of_event  first  am_1  cumA switch
#>        <num>    <int>    <num>         <num> <lgcl> <num> <num>  <num>
#>   1:       0        0        1          9999   TRUE     0     1      0
#>   2:       0        0        0          9999  FALSE     1     2      0
#>   3:       0        0        0          9999  FALSE     1     3      0
#>   4:       0        0        0          9999  FALSE     1     4      0
#>   5:       0        0        0          9999  FALSE     1     5      0
#>  ---                                                                  
#> 317:       0        0        0            14  FALSE     1     3      0
#> 318:       0        0        0            14  FALSE     1     3      1
#> 319:       0        0        1             7   TRUE     0     1      0
#> 320:       0        0        0             7  FALSE     1     2      0
#> 321:       0        0        0             7  FALSE     1     2      1
#>      regime_start time_on_regime eligible0 eligible1        wt       p_n
#>             <int>          <num>     <num>     <num>     <num>     <num>
#>   1:            0              0         1         0 0.7964844 0.4706283
#>   2:            0              1         0         1 0.9274210 0.7174218
#>   3:            0              2         0         1 1.0750073 0.7126312
#>   4:            0              3         0         1 1.0761263 0.7077924
#>   5:            0              4         0         1 0.9234340 0.7029062
#>  ---                                                                    
#> 317:            0              2         0         1 1.1082753 0.5621482
#> 318:            3              3         0         1 0.8898716 0.5563530
#> 319:            0              0         1         0 0.8816779 0.2512156
#> 320:            0              1         0         1 1.0034178 0.5621482
#> 321:            2              2         0         1 0.9959067 0.5563530
#>            p_d       wtS  sw_d0  sw_d1
#>          <num>     <num> <lgcl> <lgcl>
#>   1: 0.5908820 0.7964844   TRUE  FALSE
#>   2: 0.7735665 0.9274210  FALSE   TRUE
#>   3: 0.6629082 1.0750073  FALSE   TRUE
#>   4: 0.6577225 1.0761263  FALSE   TRUE
#>   5: 0.7611872 0.9234340  FALSE   TRUE
#>  ---                                  
#> 317: 0.5072279 1.1082753  FALSE   TRUE
#> 318: 0.5014484 0.8898716  FALSE   TRUE
#> 319: 0.2849290 0.8816779   TRUE  FALSE
#> 320: 0.5602335 1.0034178  FALSE   TRUE
#> 321: 0.5545296 0.9959067  FALSE   TRUE
ipw_data(trial_pp)
#> Key: <id>
#> Indices: <am_1>, <treatment>
#>         id period treatment    x1           x2    x3        x4   age      age_s
#>      <int>  <int>     <num> <num>        <num> <int>     <num> <num>      <num>
#>   1:     1      0         1     1  1.146148362     0 0.7342030    36 0.08333333
#>   2:     1      1         1     1  0.002200337     0 0.7342030    37 0.16666667
#>   3:     1      2         1     0 -0.481762418     0 0.7342030    38 0.25000000
#>   4:     1      3         1     0  0.007872396     0 0.7342030    39 0.33333333
#>   5:     1      4         1     1  0.216053715     0 0.7342030    40 0.41666667
#>  ---                                                                           
#> 317:    98      2         1     0 -0.735240928     0 0.3174175    66 2.58333333
#> 318:    98      3         0     0 -0.425345479     0 0.3174175    67 2.66666667
#> 319:    99      0         1     1 -0.346377841     1 0.5752681    65 2.50000000
#> 320:    99      1         1     0 -1.106480738     1 0.5752681    66 2.58333333
#> 321:    99      2         0     0  1.650478074     1 0.5752681    67 2.66666667
#>      outcome censored eligible time_of_event  first  am_1  cumA switch
#>        <num>    <int>    <num>         <num> <lgcl> <num> <num>  <num>
#>   1:       0        0        1          9999   TRUE     0     1      0
#>   2:       0        0        0          9999  FALSE     1     2      0
#>   3:       0        0        0          9999  FALSE     1     3      0
#>   4:       0        0        0          9999  FALSE     1     4      0
#>   5:       0        0        0          9999  FALSE     1     5      0
#>  ---                                                                  
#> 317:       0        0        0            14  FALSE     1     3      0
#> 318:       0        0        0            14  FALSE     1     3      1
#> 319:       0        0        1             7   TRUE     0     1      0
#> 320:       0        0        0             7  FALSE     1     2      0
#> 321:       0        0        0             7  FALSE     1     2      1
#>      regime_start time_on_regime eligible0 eligible1        wt       p_n
#>             <int>          <num>     <num>     <num>     <num>     <num>
#>   1:            0              0         1         0 0.7964844 0.4706283
#>   2:            0              1         0         1 0.9274210 0.7174218
#>   3:            0              2         0         1 1.0750073 0.7126312
#>   4:            0              3         0         1 1.0761263 0.7077924
#>   5:            0              4         0         1 0.9234340 0.7029062
#>  ---                                                                    
#> 317:            0              2         0         1 1.1082753 0.5621482
#> 318:            3              3         0         1 0.8898716 0.5563530
#> 319:            0              0         1         0 0.8816779 0.2512156
#> 320:            0              1         0         1 1.0034178 0.5621482
#> 321:            2              2         0         1 0.9959067 0.5563530
#>            p_d       wtS  sw_d0  sw_d1
#>          <num>     <num> <lgcl> <lgcl>
#>   1: 0.5908820 0.7964844   TRUE  FALSE
#>   2: 0.7735665 0.9274210  FALSE   TRUE
#>   3: 0.6629082 1.0750073  FALSE   TRUE
#>   4: 0.6577225 1.0761263  FALSE   TRUE
#>   5: 0.7611872 0.9234340  FALSE   TRUE
#>  ---                                  
#> 317: 0.5072279 1.1082753  FALSE   TRUE
#> 318: 0.5014484 0.8898716  FALSE   TRUE
#> 319: 0.2849290 0.8816779   TRUE  FALSE
#> 320: 0.5602335 1.0034178  FALSE   TRUE
#> 321: 0.5545296 0.9959067  FALSE   TRUE
```
