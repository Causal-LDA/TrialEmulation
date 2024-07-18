# fit_msm works

    Code
      fm_01
    Output
      Trial Sequence Object 
      Estimand: Intention-to-treat 
      Data 
      N: 725 observations from 89 patients 
      Key: <id>
      Index: <am_1>
              id period treatment    x1           x2    x3        x4   age      age_s
           <int>  <int>     <num> <num>        <num> <int>     <num> <num>      <num>
        1:     1      0         1     1  1.146148362     0 0.7342030    36 0.08333333
        2:     1      1         1     1  0.002200337     0 0.7342030    37 0.16666667
       ---                                                                           
      724:    99      6         1     1 -0.033762356     1 0.5752681    71 3.00000000
      725:    99      7         0     0 -1.340496520     1 0.5752681    72 3.08333333
           outcome censored eligible time_of_event  first  am_1  cumA switch
             <num>    <int>    <num>         <num> <lgcl> <num> <num>  <num>
        1:       0        0        1          9999   TRUE     0     1      0
        2:       0        0        0          9999  FALSE     1     2      0
       ---                                                                  
      724:       0        0        0             7  FALSE     1     4      0
      725:       1        0        0             7  FALSE     1     4      1
           regime_start time_on_regime eligible0 eligible1        wt      pC_n
                  <int>          <num>     <num>     <num>     <num>     <num>
        1:            0              0         1         0 1.1087474 0.9205042
        2:            0              1         0         1 0.9991778 0.9523103
       ---                                                                    
      724:            5              1         0         1 0.9908385 0.9443712
      725:            7              2         0         1 0.9861780 0.9402561
                pC_d       wtC
               <num>     <num>
        1: 0.8302199 1.1087474
        2: 0.9530939 0.9991778
       ---                    
      724: 0.9531031 0.9908385
      725: 0.9534345 0.9861780
       
      IPW for informative censoring: 
       - Numerator formula: 1 - censored ~ x1 + x2 + x3 
       - Denominator formula: 1 - censored ~ x2 
      Model fitter type: te_stats_glm_logit 
      View weight model summaries with show_weight_models() 
       
      Outcome model: 
      TE Outcome Model Object 
      Formula: outcome ~ assigned_treatment + x1 + x2 + followup_time + I(followup_time^2) + trial_period + I(trial_period^2) 
      Treatment_var: assigned_treatment 
      Adjustment_vars: x1 x2 
       
      Model Summary: 
       
       term               estimate std.error statistic p.value conf.low conf.high
       (Intercept)        -5.868   0.771      -7.61    2.7e-14 -7.38    -4.3572  
       assigned_treatment  1.630   0.520       3.13    1.7e-03  0.61     2.6489  
       x1                 -0.322   0.561      -0.57    5.7e-01 -1.42     0.7772  
       x2                  0.222   0.409       0.54    5.9e-01 -0.58     1.0223  
       followup_time       0.343   0.240       1.43    1.5e-01 -0.13     0.8139  
       I(followup_time^2) -0.022   0.014      -1.50    1.3e-01 -0.05     0.0066  
       trial_period        6.876   0.962       7.15    8.6e-13  4.99     8.7605  
       I(trial_period^2)  -7.395   0.535     -13.82    1.9e-43 -8.44    -6.3461  
       
       null.deviance df.null logLik AIC BIC deviance df.residual nobs
       155           1557    -69.2  154 197 132      1550        1558

