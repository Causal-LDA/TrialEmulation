# Example of a prepared data object

A small example object from
[data_preparation](https://causal-lda.github.io/TrialEmulation/reference/data_preparation.md)
used in examples. It is created with the following code:

## Usage

``` r
te_data_ex
```

## Format

An object of class `TE_data_prep_dt` (inherits from `TE_data_prep`) of
length 6.

## Details

    dat <- trial_example[trial_example$id < 200, ]

    te_data_ex <- data_preparation(
    data = dat,
     outcome_cov = c("nvarA", "catvarA"),
     first_period = 260,
     last_period = 280
    )

## See also

[te_model_ex](https://causal-lda.github.io/TrialEmulation/reference/te_model_ex.md)
