# weight_func works as expected

    Code
      result <- weight_func(sw_data = data, switch_n_cov = ~1, switch_d_cov = ~ X1 +
        X2, cense = "C", pool_cense = 0, cense_d_cov = ~ X1 + X2 + X3 + X4 + age_s,
      cense_n_cov = ~ X3 + X4, save_weight_models = FALSE, save_dir = save_dir,
      glm_function = "parglm", control = parglm.control(nthreads = 2, method = "FAST"))
    Message <simpleMessage>
      P(treatment = 1 | previous treatment = 0) for denominator
    Output
      
      Call:
      glm(formula = treatment ~ X1 + X2, family = binomial(link = "logit"), 
          data = data, control = list(epsilon = 1e-08, maxit = 25, 
              trace = FALSE, nthreads = 2, block_size = NULL, method = "FAST"), 
          method = parglm::parglm.fit, singular.ok = FALSE)
      
      Deviance Residuals: 
          Min       1Q   Median       3Q      Max  
      -1.5619  -1.0320  -0.8117   1.2276   1.9618  
      
      Coefficients:
                  Estimate Std. Error z value Pr(>|z|)    
      (Intercept) -0.52633    0.05629  -9.351  < 2e-16 ***
      X1           0.35856    0.07807   4.593 4.37e-06 ***
      X2           0.42935    0.04032  10.648  < 2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      (Dispersion parameter for binomial family taken to be 1)
      
          Null deviance: 3870  on 2848  degrees of freedom
      Residual deviance: 3731  on 2846  degrees of freedom
      AIC: 3737
      
      Number of Fisher Scoring iterations: 4
      
    Message <simpleMessage>
      P(treatment = 1 | previous treatment = 0) for numerator
    Output
      
      Call:
      glm(formula = treatment ~ 1, family = binomial(link = "logit"), 
          data = data, control = list(epsilon = 1e-08, maxit = 25, 
              trace = FALSE, nthreads = 2, block_size = NULL, method = "FAST"), 
          method = parglm::parglm.fit, singular.ok = FALSE)
      
      Deviance Residuals: 
         Min      1Q  Median      3Q     Max  
      -1.038  -1.038  -1.038   1.323   1.323  
      
      Coefficients:
                  Estimate Std. Error z value Pr(>|z|)    
      (Intercept)  -0.3366     0.0380  -8.857   <2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      (Dispersion parameter for binomial family taken to be 1)
      
          Null deviance: 3870  on 2848  degrees of freedom
      Residual deviance: 3870  on 2848  degrees of freedom
      AIC: 3872
      
      Number of Fisher Scoring iterations: 4
      
    Message <simpleMessage>
      P(treatment = 1 | previous treatment = 1) for denominator
    Output
      
      Call:
      glm(formula = treatment ~ X1 + X2, family = binomial(link = "logit"), 
          data = data, control = list(epsilon = 1e-08, maxit = 25, 
              trace = FALSE, nthreads = 2, block_size = NULL, method = "FAST"), 
          method = parglm::parglm.fit, singular.ok = FALSE)
      
      Deviance Residuals: 
          Min       1Q   Median       3Q      Max  
      -2.1549  -1.2867   0.7295   0.8722   1.4429  
      
      Coefficients:
                  Estimate Std. Error z value Pr(>|z|)    
      (Intercept)  0.89795    0.05905  15.208  < 2e-16 ***
      X1           0.34311    0.11198   3.064  0.00218 ** 
      X2           0.44843    0.04888   9.174  < 2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      (Dispersion parameter for binomial family taken to be 1)
      
          Null deviance: 2649.6  on 2153  degrees of freedom
      Residual deviance: 2549.5  on 2151  degrees of freedom
      AIC: 2555.5
      
      Number of Fisher Scoring iterations: 4
      
    Message <simpleMessage>
      P(treatment = 1 | previous treatment = 1) for numerator
    Output
      
      Call:
      glm(formula = treatment ~ 1, family = binomial(link = "logit"), 
          data = data, control = list(epsilon = 1e-08, maxit = 25, 
              trace = FALSE, nthreads = 2, block_size = NULL, method = "FAST"), 
          method = parglm::parglm.fit, singular.ok = FALSE)
      
      Deviance Residuals: 
          Min       1Q   Median       3Q      Max  
      -1.5410  -1.5410   0.8531   0.8531   0.8531  
      
      Coefficients:
                  Estimate Std. Error z value Pr(>|z|)    
      (Intercept)   0.8235     0.0468    17.6   <2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      (Dispersion parameter for binomial family taken to be 1)
      
          Null deviance: 2649.6  on 2153  degrees of freedom
      Residual deviance: 2649.6  on 2153  degrees of freedom
      AIC: 2651.6
      
      Number of Fisher Scoring iterations: 4
      
    Message <simpleMessage>
      Model for P(cense = 0 | X, previous treatment = 0) for denominator
    Output
      
      Call:
      glm(formula = 1 - C ~ X1 + X2 + X3 + X4 + age_s, family = binomial(link = "logit"), 
          data = data, control = list(epsilon = 1e-08, maxit = 25, 
              trace = FALSE, nthreads = 2, block_size = NULL, method = "FAST"), 
          method = parglm::parglm.fit, singular.ok = FALSE)
      
      Deviance Residuals: 
          Min       1Q   Median       3Q      Max  
      -3.1031   0.2401   0.4013   0.5996   1.8356  
      
      Coefficients:
                  Estimate Std. Error z value Pr(>|z|)    
      (Intercept)  0.90004    0.09634   9.342  < 2e-16 ***
      X1           0.58887    0.11288   5.217 1.82e-07 ***
      X2          -0.46469    0.05660  -8.210  < 2e-16 ***
      X3           0.32342    0.11178   2.893  0.00381 ** 
      X4          -0.25226    0.05627  -4.483 7.35e-06 ***
      age_s        0.97304    0.06783  14.346  < 2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      (Dispersion parameter for binomial family taken to be 1)
      
          Null deviance: 2465.6  on 2848  degrees of freedom
      Residual deviance: 2105.0  on 2843  degrees of freedom
      AIC: 2117
      
      Number of Fisher Scoring iterations: 5
      
    Message <simpleMessage>
      Model for P(cense = 0 | X, previous treatment = 0) for numerator
    Output
      
      Call:
      glm(formula = 1 - C ~ X3 + X4, family = binomial(link = "logit"), 
          data = data, control = list(epsilon = 1e-08, maxit = 25, 
              trace = FALSE, nthreads = 2, block_size = NULL, method = "FAST"), 
          method = parglm::parglm.fit, singular.ok = FALSE)
      
      Deviance Residuals: 
          Min       1Q   Median       3Q      Max  
      -2.2424   0.5067   0.5557   0.6094   0.8898  
      
      Coefficients:
                  Estimate Std. Error z value Pr(>|z|)    
      (Intercept)  1.51989    0.07547  20.139  < 2e-16 ***
      X3           0.20212    0.10398   1.944   0.0519 .  
      X4          -0.24079    0.05398  -4.461 8.17e-06 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      (Dispersion parameter for binomial family taken to be 1)
      
          Null deviance: 2465.6  on 2848  degrees of freedom
      Residual deviance: 2442.8  on 2846  degrees of freedom
      AIC: 2448.8
      
      Number of Fisher Scoring iterations: 4
      
    Message <simpleMessage>
      Model for P(cense = 0 | X, previous treatment = 1) for denominator
    Output
      
      Call:
      glm(formula = 1 - C ~ X1 + X2 + X3 + X4 + age_s, family = binomial(link = "logit"), 
          data = data, control = list(epsilon = 1e-08, maxit = 25, 
              trace = FALSE, nthreads = 2, block_size = NULL, method = "FAST"), 
          method = parglm::parglm.fit, singular.ok = FALSE)
      
      Deviance Residuals: 
          Min       1Q   Median       3Q      Max  
      -2.9568   0.1792   0.2875   0.4277   1.2206  
      
      Coefficients:
                  Estimate Std. Error z value Pr(>|z|)    
      (Intercept)  1.78647    0.13213  13.521  < 2e-16 ***
      X1           0.33495    0.20307   1.649   0.0991 .  
      X2          -0.59679    0.08589  -6.948  3.7e-12 ***
      X3           0.36771    0.16983   2.165   0.0304 *  
      X4          -0.22223    0.09511  -2.336   0.0195 *  
      age_s        1.14576    0.11569   9.904  < 2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      (Dispersion parameter for binomial family taken to be 1)
      
          Null deviance: 1179.7  on 2153  degrees of freedom
      Residual deviance: 1011.3  on 2148  degrees of freedom
      AIC: 1023.3
      
      Number of Fisher Scoring iterations: 6
      
    Message <simpleMessage>
      Model for P(cense = 0 | X, previous treatment = 1) for numerator
    Output
      
      Call:
      glm(formula = 1 - C ~ X3 + X4, family = binomial(link = "logit"), 
          data = data, control = list(epsilon = 1e-08, maxit = 25, 
              trace = FALSE, nthreads = 2, block_size = NULL, method = "FAST"), 
          method = parglm::parglm.fit, singular.ok = FALSE)
      
      Deviance Residuals: 
          Min       1Q   Median       3Q      Max  
      -2.3923   0.3695   0.3892   0.4273   0.5012  
      
      Coefficients:
                  Estimate Std. Error z value Pr(>|z|)    
      (Intercept)  2.35075    0.11680  20.126   <2e-16 ***
      X3           0.30635    0.16109   1.902   0.0572 .  
      X4          -0.10257    0.09121  -1.125   0.2608    
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      (Dispersion parameter for binomial family taken to be 1)
      
          Null deviance: 1179.7  on 2153  degrees of freedom
      Residual deviance: 1175.0  on 2151  degrees of freedom
      AIC: 1181
      
      Number of Fisher Scoring iterations: 5
      

# weight_func works time on regime

    Code
      for (i in result$switch_models) print(i)
    Output
      P(treatment = 1 | previous treatment = 0) for denominator 
      
                      term estimate std.error statistic   p.value
               (Intercept) -0.09647  0.072673    -1.327 1.844e-01
                        X1  0.37310  0.079695     4.682 2.846e-06
                        X2  0.45602  0.041421    11.009 3.449e-28
            time_on_regime -0.39377  0.060036    -6.559 5.421e-11
       I(time_on_regime^2)  0.02355  0.009802     2.403 1.628e-02
      
       null.deviance df.null logLik  AIC  BIC deviance df.residual nobs
                3870    2848  -1807 3625 3655     3615        2844 2849
      P(treatment = 1 | previous treatment = 0) for numerator 
      
                      term estimate std.error statistic   p.value
               (Intercept)  0.07328  0.059050     1.241 2.146e-01
            time_on_regime -0.37093  0.058468    -6.344 2.236e-10
       I(time_on_regime^2)  0.02278  0.009578     2.378 1.741e-02
      
       null.deviance df.null logLik  AIC  BIC deviance df.residual nobs
                3870    2848  -1882 3770 3788     3764        2846 2849
      P(treatment = 1 | previous treatment = 1) for denominator 
      
                      term estimate std.error statistic   p.value
               (Intercept)  0.44003   0.13985     3.146 1.652e-03
                        X1  0.34909   0.11262     3.100 1.938e-03
                        X2  0.45026   0.04908     9.173 4.581e-20
            time_on_regime  0.27407   0.09689     2.829 4.674e-03
       I(time_on_regime^2) -0.02218   0.01262    -1.757 7.887e-02
      
       null.deviance df.null logLik  AIC  BIC deviance df.residual nobs
                2650    2153  -1265 2540 2569     2530        2149 2154
      P(treatment = 1 | previous treatment = 1) for numerator 
      
                      term estimate std.error statistic  p.value
               (Intercept)  0.37107   0.13286     2.793 0.005223
            time_on_regime  0.27531   0.09461     2.910 0.003615
       I(time_on_regime^2) -0.02303   0.01235    -1.865 0.062171
      
       null.deviance df.null logLik  AIC  BIC deviance df.residual nobs
                2650    2153  -1315 2637 2654     2631        2151 2154

---

    Code
      for (i in result$censor_models) print(i)
    Output
      Model for P(cense = 0 | X, previous treatment = 0) for denominator 
      
              term estimate std.error statistic   p.value
       (Intercept)   0.9000   0.09634     9.342 9.427e-21
                X1   0.5889   0.11288     5.217 1.820e-07
                X2  -0.4647   0.05660    -8.210 2.203e-16
                X3   0.3234   0.11178     2.893 3.811e-03
                X4  -0.2523   0.05627    -4.483 7.351e-06
             age_s   0.9730   0.06783    14.346 1.130e-46
      
       null.deviance df.null logLik  AIC  BIC deviance df.residual nobs
                2466    2848  -1052 2117 2153     2105        2843 2849
      Model for P(cense = 0 | X, previous treatment = 0) for numerator 
      
              term estimate std.error statistic   p.value
       (Intercept)   1.5199   0.07547    20.139 3.386e-90
                X3   0.2021   0.10398     1.944 5.192e-02
                X4  -0.2408   0.05398    -4.461 8.173e-06
      
       null.deviance df.null logLik  AIC  BIC deviance df.residual nobs
                2466    2848  -1221 2449 2467     2443        2846 2849
      Model for P(cense = 0 | X, previous treatment = 1) for denominator 
      
              term estimate std.error statistic   p.value
       (Intercept)   1.7865   0.13213    13.521 1.179e-41
                X1   0.3350   0.20307     1.649 9.906e-02
                X2  -0.5968   0.08589    -6.948 3.697e-12
                X3   0.3677   0.16983     2.165 3.037e-02
                X4  -0.2222   0.09511    -2.336 1.947e-02
             age_s   1.1458   0.11569     9.904 4.017e-23
      
       null.deviance df.null logLik  AIC  BIC deviance df.residual nobs
                1180    2153 -505.7 1023 1057     1011        2148 2154
      Model for P(cense = 0 | X, previous treatment = 1) for numerator 
      
              term estimate std.error statistic   p.value
       (Intercept)   2.3507   0.11680    20.126 4.378e-90
                X3   0.3063   0.16109     1.902 5.721e-02
                X4  -0.1026   0.09121    -1.125 2.608e-01
      
       null.deviance df.null logLik  AIC  BIC deviance df.residual nobs
                1180    2153 -587.5 1181 1198     1175        2151 2154

# weight_func works with pool_cense = 1

    Code
      lapply(result$switch_models, print)
    Output
      P(treatment = 1 | previous treatment = 0) for denominator 
      
              term estimate std.error statistic   p.value
       (Intercept)  -0.5263   0.05629    -9.351 8.713e-21
                X1   0.3586   0.07807     4.593 4.369e-06
                X2   0.4294   0.04032    10.648 1.787e-26
      
       null.deviance df.null logLik  AIC  BIC deviance df.residual nobs
                3870    2848  -1865 3737 3755     3731        2846 2849
      P(treatment = 1 | previous treatment = 0) for numerator 
      
              term estimate std.error statistic   p.value
       (Intercept)  -0.3366     0.038    -8.857 8.201e-19
      
       null.deviance df.null logLik  AIC  BIC deviance df.residual nobs
                3870    2848  -1935 3872 3878     3870        2848 2849
      P(treatment = 1 | previous treatment = 1) for denominator 
      
              term estimate std.error statistic   p.value
       (Intercept)   0.8979   0.05905    15.208 3.149e-52
                X1   0.3431   0.11198     3.064 2.183e-03
                X2   0.4484   0.04888     9.174 4.562e-20
      
       null.deviance df.null logLik  AIC  BIC deviance df.residual nobs
                2650    2153  -1275 2556 2573     2550        2151 2154
      P(treatment = 1 | previous treatment = 1) for numerator 
      
              term estimate std.error statistic   p.value
       (Intercept)   0.8235    0.0468      17.6 2.571e-69
      
       null.deviance df.null logLik  AIC  BIC deviance df.residual nobs
                2650    2153  -1325 2652 2657     2650        2153 2154
      $switch_d0
      NULL
      
      $switch_n0
      NULL
      
      $switch_d1
      NULL
      
      $switch_n1
      NULL
      

---

    Code
      lapply(result$censor_models, print)
    Output
      Model for P(cense = 0 | X) for denominator 
      
              term estimate std.error statistic   p.value
       (Intercept)   1.3102   0.07502    17.465 2.640e-68
                X1   0.3612   0.09673     3.734 1.886e-04
                X2  -0.5369   0.04672   -11.492 1.446e-30
                X3   0.3147   0.09232     3.409 6.530e-04
                X4  -0.1317   0.04615    -2.854 4.320e-03
             age_s   1.0445   0.05846    17.866 2.177e-71
      
       null.deviance df.null logLik  AIC  BIC deviance df.residual nobs
                3718    5002  -1594 3200 3239     3188        4997 5003
      Model for P(cense = 0 | X) for numerator 
      
              term estimate std.error statistic    p.value
       (Intercept)  1.85647   0.06132    30.277 2.290e-201
                X3  0.21535   0.08648     2.490  1.277e-02
                X4 -0.05953   0.04371    -1.362  1.732e-01
      
       null.deviance df.null logLik  AIC  BIC deviance df.residual nobs
                3718    5002  -1855 3716 3735     3710        5000 5003
      $cens_pool_d
      NULL
      
      $cens_pool_n
      NULL
      

