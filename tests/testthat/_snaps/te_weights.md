# show works for te_weights_spec objects

    Code
      show_weight_models(object)
    Output
      Weight Models for Informative Censoring
      ---------------------------------------
      
      Weight Models for Treatment Switching
      -------------------------------------
      

---

    Code
      show(object@censor_weights)
    Output
       - Numerator formula: 1 - censored ~ age + x4 
       - Denominator formula: 1 - censored ~ age + x2 + x4 
       - Numerator model is pooled across treatment arms. Denominator model is not pooled. 
       - Model fitter type: te_stats_glm_logit 
       - Weight models not fitted. Use calculate_weights() 

---

    Code
      show(object_w_weights)
    Output
      Trial Sequence Object 
      Estimand: Per-protocol 
       
      Data: 
       - N: 321 observations from 89 patients 
              id period treatment    x1           x2    x3        x4   age      age_s
           <int>  <int>     <num> <num>        <num> <int>     <num> <num>      <num>
        1:     1      0         1     1  1.146148362     0 0.7342030    36 0.08333333
        2:     1      1         1     1  0.002200337     0 0.7342030    37 0.16666667
       ---                                                                           
      320:    99      1         1     0 -1.106480738     1 0.5752681    66 2.58333333
      321:    99      2         0     0  1.650478074     1 0.5752681    67 2.66666667
           outcome censored eligible time_on_regime        wt       wtS       wtC
             <num>    <int>    <num>          <num>     <num>     <num>     <num>
        1:       0        0        1              0 1.8629733 0.9663301 1.9278851
        2:       0        0        0              1 0.8873368 0.9805333 0.9049533
       ---                                                                       
      320:       0        0        0              1 1.1119420 1.1117399 1.0001818
      321:       0        0        0              2 1.6082501 1.5974091 1.0067866
       
      IPW for informative censoring: 
       - Numerator formula: 1 - censored ~ age + x4 
       - Denominator formula: 1 - censored ~ age + x2 + x4 
       - Numerator model is pooled across treatment arms. Denominator model is not pooled. 
       - Model fitter type: te_stats_glm_logit 
       - View weight model summaries with show_weight_models() 
       
      IPW for treatment switch censoring: 
       - Numerator formula: treatment ~ age + x4 
       - Denominator formula: treatment ~ age + x2 + x4 
       - Model fitter type: te_stats_glm_logit 
       - View weight model summaries with show_weight_models() 
       
      Sequence of Trials Data: 
      - Use set_expansion_options() and expand_trials() to construct the sequence of trials dataset. 
       
      Outcome model: 
       - Outcome model not specified. Use set_outcome_model() 

---

    Code
      show_weight_models(object_w_weights)
    Output
      Weight Models for Informative Censoring
      ---------------------------------------
      
      [[n]]
      Model: P(censor_event = 0 | X) for numerator 
       
       term        estimate   std.error  statistic p.value     
       (Intercept) -2.0539683 0.71502953 -2.872564 4.071553e-03
       age          0.1017218 0.01953751  5.206488 1.924486e-07
       x4          -0.1659871 0.16180629 -1.025839 3.049677e-01
       
       null.deviance df.null logLik    AIC      BIC     deviance df.residual nobs
       256.5508      320     -108.7563 223.5127 234.827 217.5127 318         321 
       
       path                
       /tempdir/model_n.rds
       
      [[d0]]
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
       
      [[d1]]
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
       
      Weight Models for Treatment Switching
      -------------------------------------
      
      [[n1]]
      Model: P(treatment = 1 | previous treatment = 1) for numerator 
       
       term        estimate    std.error statistic  p.value     
       (Intercept)  1.23927187 0.7983788  1.5522354 0.1206059128
       age         -0.01404746 0.0173406 -0.8100905 0.4178881792
       x4           0.79347625 0.2162862  3.6686408 0.0002438434
       
       null.deviance df.null logLik    AIC      BIC      deviance df.residual nobs
       188.829       150     -85.39205 176.7841 185.8359 170.7841 148         151 
       
       path                 
       /tempdir/model_n1.rds
       
      [[d1]]
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
       
      [[n0]]
      Model: P(treatment = 1 | previous treatment = 0) for numerator 
       
       term        estimate    std.error  statistic  p.value     
       (Intercept) 0.084860976 0.67437905 0.12583572 8.998620e-01
       age         0.001012695 0.01625994 0.06228156 9.503386e-01
       x4          1.108633792 0.21415037 5.17689418 2.256101e-07
       
       null.deviance df.null logLik    AIC      BIC      deviance df.residual nobs
       232.2705      169     -94.30944 194.6189 204.0263 188.6189 167         170 
       
       path                 
       /tempdir/model_n0.rds
       
      [[d0]]
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
       

# weight_model_data_indices works

    Code
      data.frame(values = rle_result$values, lengths = rle_result$lengths)
    Output
          values lengths
      1     TRUE       1
      2    FALSE       5
      3     TRUE       2
      4    FALSE       2
      5     TRUE       1
      6    FALSE       4
      7     TRUE       5
      8    FALSE       4
      9     TRUE       1
      10   FALSE       7
      11    TRUE       1
      12   FALSE       8
      13    TRUE       1
      14   FALSE       1
      15    TRUE       1
      16   FALSE       5
      17    TRUE       9
      18   FALSE       1
      19    TRUE       3
      20   FALSE       1
      21    TRUE      10
      22   FALSE       1
      23    TRUE       4
      24   FALSE       2
      25    TRUE       1
      26   FALSE       1
      27    TRUE       1
      28   FALSE       4
      29    TRUE       6
      30   FALSE       1
      31    TRUE       1
      32   FALSE       2
      33    TRUE       1
      34   FALSE       2
      35    TRUE       5
      36   FALSE       2
      37    TRUE       2
      38   FALSE       2
      39    TRUE       6
      40   FALSE       1
      41    TRUE       1
      42   FALSE       5
      43    TRUE       1
      44   FALSE       1
      45    TRUE       6
      46   FALSE       1
      47    TRUE       1
      48   FALSE      17
      49    TRUE       2
      50   FALSE       3
      51    TRUE       4
      52   FALSE       1
      53    TRUE       2
      54   FALSE       2
      55    TRUE       1
      56   FALSE       1
      57    TRUE       2
      58   FALSE       1
      59    TRUE      18
      60   FALSE       1
      61    TRUE       1
      62   FALSE       2
      63    TRUE       6
      64   FALSE       7
      65    TRUE       3
      66   FALSE       3
      67    TRUE       3
      68   FALSE       3
      69    TRUE       1
      70   FALSE       1
      71    TRUE       2
      72   FALSE       2
      73    TRUE       3
      74   FALSE       3
      75    TRUE       1
      76   FALSE       3
      77    TRUE       1
      78   FALSE       1
      79    TRUE       3
      80   FALSE       2
      81    TRUE       2
      82   FALSE       2
      83    TRUE       1
      84   FALSE       2
      85    TRUE      11
      86   FALSE       2
      87    TRUE       2
      88   FALSE       5
      89    TRUE       3
      90   FALSE       2
      91    TRUE       6
      92   FALSE       1
      93    TRUE       1
      94   FALSE       3
      95    TRUE       1
      96   FALSE       2
      97    TRUE       5
      98   FALSE       7
      99    TRUE       4
      100  FALSE       1
      101   TRUE       5
      102  FALSE       1
      103   TRUE       2
      104  FALSE       2
      105   TRUE       2
      106  FALSE       3
      107   TRUE       1
      108  FALSE       2

