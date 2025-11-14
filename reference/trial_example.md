# Example of longitudinal data for sequential trial emulation

A dataset containing the treatment, outcomes and other attributes of 503
patients for sequential trial emulation. See
[`vignette("Getting-Started")`](https://causal-lda.github.io/TrialEmulation/articles/Getting-Started.md).

## Usage

``` r
trial_example
```

## Format

A data frame with 48400 rows and 11 variables:

- id:

  patient identifier

- eligible:

  eligible for trial start in this period, 1=yes, 0=no

- period:

  time period

- outcome:

  indicator for outcome in this period, 1=event occurred, 0=no event

- treatment:

  indicator for receiving treatment in this period, 1=treatment, 0=no
  treatment

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
