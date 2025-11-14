# Example of a fitted marginal structural model object

A small example object from
[trial_msm](https://causal-lda.github.io/TrialEmulation/reference/trial_msm.md)
used in examples. It is created with the following code:

## Usage

``` r
te_model_ex
```

## Format

An object of class `TE_msm` of length 3.

## Details

    te_model_ex <- trial_msm(
     data = data_subset,
     outcome_cov = c("catvarA", "nvarA"),
     last_followup = 40,
     model_var = "assigned_treatment",
     include_followup_time = ~followup_time,
     include_trial_period = ~trial_period,
     use_sample_weights = FALSE,
     quiet = TRUE,
     glm_function = "glm"
     )

## See also

[te_data_ex](https://causal-lda.github.io/TrialEmulation/reference/te_data_ex.md)
