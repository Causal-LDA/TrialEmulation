# Example of longitudinal data for sequential trial emulation containing censoring

This data contains data from 89 patients followed for up to 19 periods.

## Usage

``` r
data_censored
```

## Format

A data frame with 725 rows and 12 variables:

- id:

  patient identifier

- period:

  time period

- treatment:

  indicator for receiving treatment in this period, 1=treatment,
  0=non-treatment

- x1:

  A time-varying categorical variable relating to treatment and the
  outcome

- x2:

  A time-varying numeric variable relating to treatment and the outcome

- x3:

  A fixed categorical variable relating to treatment and the outcome

- x4:

  A fixed categorical variable relating to treatment and the outcome

- age:

  patient age in years

- age_s:

  patient age

- outcome:

  indicator for outcome in this period, 1=event occurred, 0=no event

- censored:

  indicator for patient being censored in this period, 1=censored, 0=not
  censored

- eligible:

  indicator for eligibility for trial start in this period, 1=yes, 0=no
