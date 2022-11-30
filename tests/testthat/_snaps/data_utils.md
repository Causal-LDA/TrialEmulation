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
      

