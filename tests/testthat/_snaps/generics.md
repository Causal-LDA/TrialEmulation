# summary for data_preparation separate=TRUE

    Code
      summary(object, digits = 3)
    Output
      Expanded Trial Emulation data
      
      Expanded data saved in 10 csv files:
       1: random_temp_dir_path/trial_0.csv
       2: random_temp_dir_path/trial_1.csv
       3: random_temp_dir_path/trial_2.csv
      ---
       8: random_temp_dir_path/trial_7.csv
       9: random_temp_dir_path/trial_8.csv
      10: random_temp_dir_path/trial_9.csv
      
      
      Number of observations in expanded data: 2346 
      First trial period: 0 
      Last trial period: 9 
      
      ------------------------------------------------------------ 
      Weight models
      -------------
      
      Treatment switch models
      -----------------------
      
      switch_models$switch_d0:
       P(treatment = 1 | previous treatment = 0) for denominator 
      
              term estimate std.error statistic  p.value
       (Intercept)   -0.281     0.067      -4.2 2.69e-05
      
      ------------------------------------------------------------ 
      switch_models$switch_n0:
       P(treatment = 1 | previous treatment = 0) for numerator 
      
              term estimate std.error statistic  p.value
       (Intercept)  -0.0303    0.0763    -0.398 6.91e-01
             age_s  -0.5164    0.0728    -7.093 1.32e-12
      
      ------------------------------------------------------------ 
      switch_models$switch_d1:
       P(treatment = 1 | previous treatment = 1) for denominator 
      
              term estimate std.error statistic  p.value
       (Intercept)     0.98    0.0784      12.5 6.51e-36
      
      ------------------------------------------------------------ 
      switch_models$switch_n1:
       P(treatment = 1 | previous treatment = 1) for numerator 
      
              term estimate std.error statistic  p.value
       (Intercept)    1.203    0.0977     12.31 7.55e-35
             age_s   -0.379    0.0874     -4.34 1.44e-05
      
      ------------------------------------------------------------ 
      Censoring models
      ----------------
      
      censor_models$cens_d0:
      Model for P(cense = 0 | X, previous treatment = 0) for denominator 
      
              term estimate std.error statistic  p.value
       (Intercept)     1.69    0.0914      18.5 4.11e-76
      
      ------------------------------------------------------------ 
      censor_models$cens_n0:
      Model for P(cense = 0 | X, previous treatment = 0) for numerator 
      
              term estimate std.error statistic  p.value
       (Intercept)    1.493     0.122     12.22 2.48e-34
                X1    0.409     0.185      2.21 2.68e-02
      
      ------------------------------------------------------------ 
      censor_models$cens_d1:
      Model for P(cense = 0 | X, previous treatment = 1) for denominator 
      
              term estimate std.error statistic  p.value
       (Intercept)      2.4     0.127        19 2.08e-80
      
      ------------------------------------------------------------ 
      censor_models$cens_n1:
      Model for P(cense = 0 | X, previous treatment = 1) for numerator 
      
              term estimate std.error statistic  p.value
       (Intercept)     2.22     0.137     16.19 5.76e-59
                X1     0.93     0.367      2.54 1.12e-02
      
      ------------------------------------------------------------ 

---

    Code
      print(object$censor_models[[1]])
    Output
      Model for P(cense = 0 | X, previous treatment = 0) for denominator 
      
              term estimate std.error statistic   p.value
       (Intercept)    1.687   0.09135     18.46 4.113e-76
      
       null.deviance df.null logLik   AIC   BIC deviance df.residual nobs
               787.8     908 -393.9 789.8 794.6    787.8         908  909
      
      Object saved at "random_temp_dir_path/cense_model_d0.rds"

---

    Code
      print(object$censor_models[[1]], full = FALSE)
    Output
      Model for P(cense = 0 | X, previous treatment = 0) for denominator 
      
              term estimate std.error statistic   p.value
       (Intercept)    1.687   0.09135     18.46 4.113e-76
      

# summary for data_preparation separate=FALSE

    Code
      summary(object, digits = 3)
    Output
      Expanded Trial Emulation data
      
              id for_period followup_time outcome weight treatment X1
         1:    1          0             0       0  1.000         1  1
         2:    1          0             1       0  1.011         1  1
         3:    1          0             2       0  0.994         1  1
        ---                                                          
      9474: 1000          0             7       0  0.821         1  0
      9475: 1000          0             8       0  0.761         1  0
      9476: 1000          0             9       1  0.698         1  0
            assigned_treatment dose
         1:                  1    1
         2:                  1    2
         3:                  1    3
        ---                        
      9474:                  1    8
      9475:                  1    9
      9476:                  1   10
      
      Number of observations in expanded data: 9476 
      First trial period: 0 
      Last trial period: 9 
      
      ------------------------------------------------------------ 
      Weight models
      -------------
      
      Treatment switch models
      -----------------------
      
      switch_models$switch_d0:
       P(treatment = 1 | previous treatment = 0) for denominator 
      
              term estimate std.error statistic  p.value
       (Intercept)   -0.303    0.0378     -8.01 1.17e-15
      
      ------------------------------------------------------------ 
      switch_models$switch_n0:
       P(treatment = 1 | previous treatment = 0) for numerator 
      
              term estimate std.error statistic  p.value
       (Intercept)    -0.11    0.0454     -2.42 1.54e-02
             age_s    -0.32    0.0422     -7.60 3.04e-14
      
      ------------------------------------------------------------ 
      switch_models$switch_d1:
       P(treatment = 1 | previous treatment = 1) for denominator 
      
              term estimate std.error statistic  p.value
       (Intercept)    0.757    0.0454      16.7 2.27e-62
      
      ------------------------------------------------------------ 
      switch_models$switch_n1:
       P(treatment = 1 | previous treatment = 1) for numerator 
      
              term estimate std.error statistic  p.value
       (Intercept)    1.004    0.0624     16.09 3.12e-58
             age_s   -0.341    0.0554     -6.15 7.72e-10
      
      ------------------------------------------------------------ 
      Censoring models
      ----------------
      
      censor_models$cens_d0:
      Model for P(cense = 0 | X, previous treatment = 0) for denominator 
      
              term estimate std.error statistic   p.value
       (Intercept)     1.71    0.0518      32.9 1.41e-237
      
      ------------------------------------------------------------ 
      censor_models$cens_n0:
      Model for P(cense = 0 | X, previous treatment = 0) for numerator 
      
              term estimate std.error statistic   p.value
       (Intercept)    1.558    0.0696     22.39 5.51e-111
                X1    0.314    0.1045      3.01  2.65e-03
      
      ------------------------------------------------------------ 
      censor_models$cens_d1:
      Model for P(cense = 0 | X, previous treatment = 1) for denominator 
      
              term estimate std.error statistic   p.value
       (Intercept)     2.74    0.0888      30.9 2.19e-209
      
      ------------------------------------------------------------ 
      censor_models$cens_n1:
      Model for P(cense = 0 | X, previous treatment = 1) for numerator 
      
              term estimate std.error statistic   p.value
       (Intercept)     2.67     0.100     26.67 9.51e-157
                X1     0.32     0.218      1.47  1.42e-01
      
      ------------------------------------------------------------ 

---

    Code
      print(object$switch_models[[1]])
    Output
      P(treatment = 1 | previous treatment = 0) for denominator 
      
              term estimate std.error statistic   p.value
       (Intercept)  -0.3028   0.03781    -8.007 1.174e-15
      
       null.deviance df.null logLik  AIC  BIC deviance df.residual nobs
                3903    2861  -1951 3905 3911     3903        2861 2862

---

    Code
      print(object$switch_models[[1]], full = FALSE)
    Output
      P(treatment = 1 | previous treatment = 0) for denominator 
      
              term estimate std.error statistic   p.value
       (Intercept)  -0.3028   0.03781    -8.007 1.174e-15
      

---

    Code
      summary(weights(object))
    Output
         Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
       0.1661  0.9386  1.0000  1.0810  1.1354  7.7059 

# summary for initiators

    Code
      summary(object, digits = 3)
    Output
      Trial Emulation Outcome Model
      
      Outcome model formula:
      outcome ~ assigned_treatment + dose + for_period + I(for_period^2) + 
          followup_time + I(followup_time^2) + X1
      
      Coefficent summary (robust):
                    names estimate robust_se    2.5%   97.5%       z p_value
              (Intercept)  -4.1266    0.2437 -4.6043 -3.6489 -16.931  <2e-16
       assigned_treatment   0.3091    0.2788 -0.2373  0.8555   1.109   0.267
                     dose  -0.1922    0.1174 -0.4224  0.0380  -1.636   0.102
               for_period  -0.0668    0.0926 -0.2483  0.1147  -0.721   0.471
          I(for_period^2)  -0.0116    0.0199 -0.0506  0.0273  -0.585   0.558
            followup_time   0.2249    0.1401 -0.0496  0.4995   1.606   0.108
       I(followup_time^2)  -0.0181    0.0169 -0.0513  0.0151  -1.070   0.284
                       X1  -0.1110    0.2291 -0.5600  0.3381  -0.484   0.628
      
      object$model contains the fitted glm model object.
      object$robust$matrix contains the full robust covariance matrix.

---

    Code
      summary(object, digits = 7)
    Output
      Trial Emulation Outcome Model
      
      Outcome model formula:
      outcome ~ assigned_treatment + dose + for_period + I(for_period^2) + 
          followup_time + I(followup_time^2) + X1
      
      Coefficent summary (robust):
                    names    estimate  robust_se        2.5%       97.5%           z
              (Intercept) -4.12662820 0.24372909 -4.60433721 -3.64891919 -16.9312093
       assigned_treatment  0.30913237 0.27877733 -0.23727119  0.85553592   1.1088863
                     dose -0.19218381 0.11744822 -0.42238232  0.03801470  -1.6363280
               for_period -0.06679054 0.09260338 -0.24829317  0.11471209  -0.7212538
          I(for_period^2) -0.01163048 0.01987472 -0.05058494  0.02732398  -0.5851896
            followup_time  0.22492444 0.14006997 -0.04961271  0.49946158   1.6058005
       I(followup_time^2) -0.01812884 0.01693641 -0.05132420  0.01506652  -1.0704062
                       X1 -0.11095373 0.22910223 -0.55999411  0.33808665  -0.4842979
         p_value
       < 2.2e-16
       0.2674792
       0.1017710
       0.4707534
       0.5584203
       0.1083177
       0.2844365
       0.6281745
      
      object$model contains the fitted glm model object.
      object$robust$matrix contains the full robust covariance matrix.

