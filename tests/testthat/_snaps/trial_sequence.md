# show works for trial_sequence_PP with nothing set

    Code
      show(trial_sequence("PP"))
    Output
      Trial Sequence Object 
      Estimand: Per-protocol 
      Data 
      N: 0 observations from 0 patients 
      data frame with 0 columns and 0 rows
       
      IPW for informative censoring: 
       - No weight model specified 
       
      IPW for treatment switch censoring: 
       - No weight model specified 
       
      Outcome model: 
       - Outcome model not specified. Use set_outcome_model()

# show works for trial_sequence_ITT with nothing set

    Code
      show(trial_sequence("ITT"))
    Output
      Trial Sequence Object 
      Estimand: Intention-to-treat 
      Data 
      N: 0 observations from 0 patients 
      data frame with 0 columns and 0 rows
       
      IPW for informative censoring: 
       - No weight model specified 
       
      Outcome model: 
       - Outcome model not specified. Use set_outcome_model()

# show works for trial_sequence_AT with nothing set

    Code
      show(trial_sequence("AT"))
    Output
      Trial Sequence Object 
      Estimand: As treated 
      Data 
      N: 0 observations from 0 patients 
      data frame with 0 columns and 0 rows
       
      IPW for informative censoring: 
       - No weight model specified 
       
      IPW for treatment switch censoring: 
       - No weight model specified 
       
      Outcome model: 
       - Outcome model not specified. Use set_outcome_model()

# show works for trial_sequence_PP with data and outcome_model set

    Code
      show(result)
    Output
      Trial Sequence Object 
      Estimand: Per-protocol 
      Data 
      N: 321 observations from 89 patients 
      Key: <id>
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
           regime_start time_on_regime eligible0 eligible1
                  <int>          <num>     <num>     <num>
        1:            0              0         1         0
        2:            0              1         0         1
       ---                                                
      320:            0              1         0         1
      321:            2              2         0         1
       
      IPW for informative censoring: 
       - No weight model specified 
       
      IPW for treatment switch censoring: 
       - No weight model specified 
       
      Outcome model: 
      TE Outcome Model Object 
      Formula: outcome ~ assigned_treatment + followup_time + I(followup_time^2) + trial_period + I(trial_period^2) 
      Treatment_var: assigned_treatment 
      Adjustment_vars:  
       
      Use fit_msm() to fit the outcome model 

