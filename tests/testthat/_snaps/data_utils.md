# weight_func works as expected

    Code
      result <- weight_func(sw_data = data, switch_n_cov = ~1, switch_d_cov = ~ X1 +
        X2, use_switch_weights = TRUE, use_censor_weights = TRUE, cense = "C",
      pool_cense_d = FALSE, pool_cense_n = FALSE, cense_d_cov = ~ X1 + X2 + X3 + X4 +
        age_s, cense_n_cov = ~ X3 + X4, save_weight_models = FALSE, data_dir = save_dir,
      glm_function = "parglm", control = parglm.control(nthreads = 2, method = "FAST"))
    Message
      P(treatment = 1 | previous treatment = 0) for denominator
    Output
      
      Call:
      glm(formula = treatment ~ X1 + X2, family = binomial(link = "logit"), 
          data = data, control = list(epsilon = 1e-08, maxit = 25, 
              trace = FALSE, nthreads = 2, block_size = NULL, method = "FAST"), 
          method = parglm::parglm.fit, singular.ok = FALSE)
      
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
      
    Message
      P(treatment = 1 | previous treatment = 0) for numerator
    Output
      
      Call:
      glm(formula = treatment ~ 1, family = binomial(link = "logit"), 
          data = data, control = list(epsilon = 1e-08, maxit = 25, 
              trace = FALSE, nthreads = 2, block_size = NULL, method = "FAST"), 
          method = parglm::parglm.fit, singular.ok = FALSE)
      
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
      
    Message
      P(treatment = 1 | previous treatment = 1) for denominator
    Output
      
      Call:
      glm(formula = treatment ~ X1 + X2, family = binomial(link = "logit"), 
          data = data, control = list(epsilon = 1e-08, maxit = 25, 
              trace = FALSE, nthreads = 2, block_size = NULL, method = "FAST"), 
          method = parglm::parglm.fit, singular.ok = FALSE)
      
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
      
    Message
      P(treatment = 1 | previous treatment = 1) for numerator
    Output
      
      Call:
      glm(formula = treatment ~ 1, family = binomial(link = "logit"), 
          data = data, control = list(epsilon = 1e-08, maxit = 25, 
              trace = FALSE, nthreads = 2, block_size = NULL, method = "FAST"), 
          method = parglm::parglm.fit, singular.ok = FALSE)
      
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
      
    Message
      Model for P(cense = 0 | X, previous treatment = 0) for denominator
    Output
      
      Call:
      glm(formula = 1 - C ~ X1 + X2 + X3 + X4 + age_s, family = binomial(link = "logit"), 
          data = data, control = list(epsilon = 1e-08, maxit = 25, 
              trace = FALSE, nthreads = 2, block_size = NULL, method = "FAST"), 
          method = parglm::parglm.fit, singular.ok = FALSE)
      
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
      
    Message
      Model for P(cense = 0 | X, previous treatment = 1) for denominator
    Output
      
      Call:
      glm(formula = 1 - C ~ X1 + X2 + X3 + X4 + age_s, family = binomial(link = "logit"), 
          data = data, control = list(epsilon = 1e-08, maxit = 25, 
              trace = FALSE, nthreads = 2, block_size = NULL, method = "FAST"), 
          method = parglm::parglm.fit, singular.ok = FALSE)
      
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
      
    Message
      Model for P(cense = 0 | X, previous treatment = 0) for numerator
    Output
      
      Call:
      glm(formula = 1 - C ~ X3 + X4, family = binomial(link = "logit"), 
          data = data, control = list(epsilon = 1e-08, maxit = 25, 
              trace = FALSE, nthreads = 2, block_size = NULL, method = "FAST"), 
          method = parglm::parglm.fit, singular.ok = FALSE)
      
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
      
    Message
      Model for P(cense = 0 | X, previous treatment = 1) for numerator
    Output
      
      Call:
      glm(formula = 1 - C ~ X3 + X4, family = binomial(link = "logit"), 
          data = data, control = list(epsilon = 1e-08, maxit = 25, 
              trace = FALSE, nthreads = 2, block_size = NULL, method = "FAST"), 
          method = parglm::parglm.fit, singular.ok = FALSE)
      
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
      

# weight_func works with user specified time on regime

    Code
      for (i in result$switch_models) print(i, digits = 4)
    Output
      P(treatment = 1 | previous treatment = 0) for denominator 
      
                 term estimate std.error statistic   p.value
          (Intercept)  -0.1657   0.06642    -2.495 1.261e-02
                   X1   0.3719   0.07957     4.674 2.960e-06
                   X2   0.4548   0.04131    11.008 3.505e-28
       time_on_regime  -0.2686   0.02750    -9.768 1.538e-22
      
       null.deviance df.null logLik  AIC  BIC deviance df.residual nobs
                3870    2848  -1810 3628 3652     3620        2845 2849
      P(treatment = 1 | previous treatment = 0) for numerator 
      
                 term  estimate std.error statistic   p.value
          (Intercept)  0.005973   0.05142    0.1162 9.075e-01
       time_on_regime -0.250221   0.02676   -9.3493 8.819e-21
      
       null.deviance df.null logLik  AIC  BIC deviance df.residual nobs
                3870    2848  -1885 3773 3785     3769        2847 2849
      P(treatment = 1 | previous treatment = 1) for denominator 
      
                 term estimate std.error statistic   p.value
          (Intercept)   0.6281   0.08932     7.032 2.036e-12
                   X1   0.3559   0.11244     3.165 1.550e-03
                   X2   0.4496   0.04903     9.170 4.731e-20
       time_on_regime   0.1130   0.02898     3.901 9.591e-05
      
       null.deviance df.null logLik  AIC  BIC deviance df.residual nobs
                2650    2153  -1267 2541 2564     2533        2150 2154
      P(treatment = 1 | previous treatment = 1) for numerator 
      
                 term estimate std.error statistic   p.value
          (Intercept)   0.5673   0.08024     7.070 1.551e-12
       time_on_regime   0.1086   0.02845     3.818 1.343e-04
      
       null.deviance df.null logLik  AIC  BIC deviance df.residual nobs
                2650    2153  -1317 2638 2650     2634        2152 2154

---

    Code
      for (i in result$censor_models) print(i, digits = 4)

# weight_func works with pool_cense = TRUE

    Code
      lapply(result$switch_models, print, digits = 4)
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
      lapply(result$censor_models, print, digits = 4)
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
      

