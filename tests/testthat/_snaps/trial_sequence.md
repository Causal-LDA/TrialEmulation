# show works for trial_sequence_PP with nothing set

    Code
      show(trial_sequence("PP"))
    Output
      Trial Sequence Object 
      Estimand: Per-protocol 
       
      Observational Data: 
       - No observational data has been set. Use set_data() 
       
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
       
      Observational Data: 
       - No observational data has been set. Use set_data() 
       
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
       
      Observational Data: 
       - No observational data has been set. Use set_data() 
       
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
       
      Observational Data: 
       - N: 321 observations from 89 patients 
              id period treatment    x1           x2    x3        x4   age      age_s
           <int>  <int>     <num> <num>        <num> <int>     <num> <num>      <num>
        1:     1      0         1     1  1.146148362     0 0.7342030    36 0.08333333
        2:     1      1         1     1  0.002200337     0 0.7342030    37 0.16666667
       ---                                                                           
      320:    99      1         1     0 -1.106480738     1 0.5752681    66 2.58333333
      321:    99      2         0     0  1.650478074     1 0.5752681    67 2.66666667
           outcome censored eligible time_on_regime
             <num>    <int>    <num>          <num>
        1:       0        0        1              0
        2:       0        0        0              1
       ---                                         
      320:       0        0        0              1
      321:       0        0        0              2
       
      IPW for informative censoring: 
       - No weight model specified 
       
      IPW for treatment switch censoring: 
       - No weight model specified 
       
      Sequence of Trials Data: 
      - Use set_expansion_options() and expand_trials() to construct the sequence of trials dataset. 
       
      Outcome model: 
      - Formula: outcome ~ assigned_treatment + followup_time + I(followup_time^2) + trial_period + I(trial_period^2) 
      - Treatment variable: assigned_treatment 
      - Adjustment variables:  
      - Model fitter type: te_stats_glm_logit 
       
      Use fit_msm() to fit the outcome model 
       

