# show works for te_weights_spec objects

    Code
      show_weight_models(object)

---

    Code
      show(object@censor_weights)
    Output
       - Numerator formula: 1 - censored ~ age + x4 
       - Denominator formula: 1 - censored ~ age + x2 + x4 
      Model fitter type: te_stats_glm_logit 
      Weight models not fitted 

---

    Code
      show(object_w_weights)
    Output
      Trial Sequence Object 
      Estimand: Per-protocol 
      Data 
      N: 321 observations from 89 patients 
      Key: <id>
      Indices: <am_1>, <treatment>
              id period treatment    x1           x2    x3        x4   age      age_s
           <int>  <int>     <num> <num>        <num> <int>     <num> <num>      <num>
        1:     1      0         1     1  1.146148362     0 0.7342030    36 0.08333333
        2:     1      1         1     1  0.002200337     0 0.7342030    37 0.16666667
       ---                                                                           
      320:    99      1         1     0 -1.106480738     1 0.5752681    66 2.58333333
      321:    99      2         0     0  1.650478074     1 0.5752681    67 2.66666667
           outcome censored eligible time_of_event  first  am_1  cumA switch
             <num>    <int>    <num>         <num> <lgcl> <num> <num>  <num>
        1:       0        0        1          9999   TRUE     0     1      0
        2:       0        0        0          9999  FALSE     1     2      0
       ---                                                                  
      320:       0        0        0             7  FALSE     1     2      0
      321:       0        0        0             7  FALSE     1     2      1
           regime_start time_on_regime eligible0 eligible1        wt       p_n
                  <int>          <num>     <num>     <num>     <num>     <num>
        1:            0              0         1         0 1.8629733 0.7181453
        2:            0              1         0         1 0.8873368 0.7861863
       ---                                                                    
      320:            0              1         0         1 1.1119420 0.6832182
      321:            2              2         0         1 1.6082501 0.6801701
                 p_d       wtS      pC_n      pC_d       wtC
               <num>     <num>     <num>     <num>     <num>
        1: 0.7431677 0.9663301 0.8154994 0.4230021 1.9278851
        2: 0.8017946 0.9805333 0.8303162 0.9175238 0.9049533
       ---                                                  
      320: 0.6145486 1.1117399 0.9896890 0.9895092 1.0001818
      321: 0.7997821 1.5974091 0.9906770 0.9839990 1.0067866
       
      IPW for informative censoring: 
       - Numerator formula: 1 - censored ~ age + x4 
       - Denominator formula: 1 - censored ~ age + x2 + x4 
      Model fitter type: te_stats_glm_logit 
      View weight model summaries with show_weight_models() 
       
      IPW for treatment switch censoring: 
       - Numerator formula: treatment ~ age + x4 
       - Denominator formula: treatment ~ age + x2 + x4 
      Model fitter type: te_stats_glm_logit 
      View weight model summaries with show_weight_models() 
       
      TE Outcome Model Object 
      Formula: new("formula", .S3Class = "formula", structure(list(), class = "formula", .Environment = <environment>)) 
      Treatment_var:  
      Adjustment_vars:  
       
      An object of class "te_outcome_fitted"
      Slot "model":
      list()
      
      Slot "summary":
      list()
      

---

    Code
      show_weight_models(object_w_weights)
    Output
      Model: P(censor_event = 0 | X) for numerator 
       
       term        estimate   std.error  statistic p.value     
       (Intercept) -2.0539683 0.71502953 -2.872564 4.071553e-03
       age          0.1017218 0.01953751  5.206488 1.924486e-07
       x4          -0.1659871 0.16180629 -1.025839 3.049677e-01
       
       null.deviance df.null logLik    AIC      BIC     deviance df.residual nobs
       256.5508      320     -108.7563 223.5127 234.827 217.5127 318         321 
       
       path                
       /tempdir/model_n.rds
       
      Model: P(censor_event = 0 | X, previous treatment = 0) for denominator 
       
       term        estimate   std.error  statistic p.value     
       (Intercept) -2.9993953 0.96683551 -3.102281 1.920357e-03
       age          0.1163969 0.02718057  4.282359 1.849224e-05
       x2          -1.0232012 0.29805533 -3.432924 5.971105e-04
       x4          -0.4475813 0.22040610 -2.030712 4.228420e-02
       
       null.deviance df.null logLik    AIC      BIC      deviance df.residual nobs
       172.8729      169     -61.88922 131.7784 144.3216 123.7784 166         170 
       
       path                 
       /tempdir/model_d0.rds
       
      Model: P(censor_event = 0 | X, previous treatment = 1) for denominator 
       
       term        estimate    std.error  statistic  p.value   
       (Intercept)  0.20802433 1.52244361  0.1366384 0.89131658
       age          0.06527658 0.03837472  1.7010309 0.08893719
       x2          -0.17882403 0.38088489 -0.4694963 0.63871496
       x4          -0.29105768 0.36381453 -0.8000166 0.42370117
       
       null.deviance df.null logLik   AIC     BIC      deviance df.residual nobs
       68.21358      150     -31.9729 71.9458 84.01492 63.9458  147         151 
       
       path                 
       /tempdir/model_d1.rds
       
      Model: P(treatment = 1 | previous treatment = 1) for numerator 
       
       term        estimate    std.error statistic  p.value     
       (Intercept)  1.23927187 0.7983788  1.5522354 0.1206059128
       age         -0.01404746 0.0173406 -0.8100905 0.4178881792
       x4           0.79347625 0.2162862  3.6686408 0.0002438434
       
       null.deviance df.null logLik    AIC      BIC      deviance df.residual nobs
       188.829       150     -85.39205 176.7841 185.8359 170.7841 148         151 
       
       path                 
       /tempdir/model_n1.rds
       
      Model: P(treatment = 1 | previous treatment = 1) for denominator 
       
       term        estimate    std.error  statistic  p.value     
       (Intercept)  1.36823919 0.81213243  1.6847489 0.0920370351
       age         -0.01484948 0.01760642 -0.8434126 0.3989977540
       x2           0.33852867 0.19500928  1.7359619 0.0825705753
       x4           0.78724133 0.21455503  3.6691814 0.0002433283
       
       null.deviance df.null logLik    AIC     BIC      deviance df.residual nobs
       188.829       150     -83.82451 175.649 187.7181 167.649  147         151 
       
       path                 
       /tempdir/model_d1.rds
       
      Model: P(treatment = 1 | previous treatment = 0) for numerator 
       
       term        estimate    std.error  statistic  p.value     
       (Intercept) 0.084860976 0.67437905 0.12583572 8.998620e-01
       age         0.001012695 0.01625994 0.06228156 9.503386e-01
       x4          1.108633792 0.21415037 5.17689418 2.256101e-07
       
       null.deviance df.null logLik    AIC      BIC      deviance df.residual nobs
       232.2705      169     -94.30944 194.6189 204.0263 188.6189 167         170 
       
       path                 
       /tempdir/model_n0.rds
       
      Model: P(treatment = 1 | previous treatment = 0) for denominator 
       
       term        estimate     std.error  statistic  p.value     
       (Intercept) 0.1214193693 0.67770501 0.17916257 8.578101e-01
       age         0.0002235908 0.01632757 0.01369406 9.890741e-01
       x2          0.1068088770 0.19102028 0.55914942 5.760597e-01
       x4          1.1040693183 0.21338578 5.17405287 2.290700e-07
       
       null.deviance df.null logLik    AIC      BIC      deviance df.residual nobs
       232.2705      169     -94.15216 196.3043 208.8475 188.3043 166         170 
       
       path                 
       /tempdir/model_d0.rds
       

---

    Code
      show(object_w_weights@switch_weights@fitted$n1)
    Output
      Model: P(treatment = 1 | previous treatment = 1) for numerator 
       
       term        estimate    std.error statistic  p.value     
       (Intercept)  1.23927187 0.7983788  1.5522354 0.1206059128
       age         -0.01404746 0.0173406 -0.8100905 0.4178881792
       x4           0.79347625 0.2162862  3.6686408 0.0002438434
       
       null.deviance df.null logLik    AIC      BIC      deviance df.residual nobs
       188.829       150     -85.39205 176.7841 185.8359 170.7841 148         151 
       
       path                 
       /tempdir/model_n1.rds
       

