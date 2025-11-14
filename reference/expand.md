# Expand Function

This function performs the data expansion for a given dataset

## Usage

``` r
expand(
  sw_data,
  outcomeCov_var,
  where_var,
  use_censor,
  maxperiod,
  minperiod,
  keeplist
)
```

## Arguments

- sw_data:

  datatable to expand

- outcomeCov_var:

  A list of individual baseline variables used in final model

- where_var:

  Variables used in where conditions used in subsetting the data used in
  final analysis (where_case), the variables not included in the final
  model

- use_censor:

  Use censoring for per-protocol analysis - censor person-times once a
  person-trial stops taking the initial treatment value

- maxperiod:

  Maximum period

- minperiod:

  Minimum period

- keeplist:

  A list contains names of variables used in final model
