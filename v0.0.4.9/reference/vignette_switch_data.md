# Example of expanded longitudinal data for sequential trial emulation

This is the expanded dataset created in the
[`vignette("Getting-Started")`](https://causal-lda.github.io/TrialEmulation/articles/Getting-Started.md)
known as `switch_data`.

## Usage

``` r
vignette_switch_data
```

## Format

A data frame with 1939053 rows and 7 variables:

- id:

  patient identifier

- trial_period:

  trial start time period

- followup_time:

  follow up time within trial

- outcome:

  indicator for outcome in this period, 1=event occurred, 0=no event

- treatment:

  indicator for receiving treatment in this period, 1=treatment,
  0=non-treatment

- assigned_treatment:

  indicator for assigned treatment at baseline of the trial,
  1=treatment, 0=non-treatment

- weight:

  weights for use with model fitting

- catvarA:

  A categorical variable relating to treatment and the outcome

- catvarB:

  A categorical variable relating to treatment and the outcome

- catvarC:

  A categorical variable relating to treatment and the outcome

- nvarA:

  A numerical variable relating to treatment and the outcome

- nvarB:

  A numerical variable relating to treatment and the outcome

- nvarC:

  A numerical variable relating to treatment and the outcome
