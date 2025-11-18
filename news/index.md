# Changelog

## TrialEmulation 0.0.4.9

- Fix bugs in testing discovered due to updated `testthat`. Thanks
  [@hadley](https://github.com/hadley)
- Update links

## TrialEmulation 0.0.4.5

CRAN release: 2025-06-13

- Update tests for `duckdb` 1.3.0 changes to sampling.
- Require R \>= 4.1 for native pipe support.

## TrialEmulation 0.0.4.2

CRAN release: 2025-02-21

- Update tests for `duckdb` 1.2.0 changes to sampling.

## TrialEmulation 0.0.4.0

CRAN release: 2024-11-29

- Add new extensible S4 class based interface.
  - Step-by-step functions to construct sequence of target trial
    analysis. See the “New Interface”” vignette.
  - Possibility to add alternative data storage and model fitting
    functionality. See the “Extending TrialEmulation” vignette.
  - Use `duckdb` to store expanded data for efficient reading and
    sampling of large data.
  - Use `parsnip` models for weight model fitting (experimental, not
    recommended).
  - This interface is still “experimental” and may change in future
  - Thanks
    [@darkgoldenrod-cherry](https://github.com/darkgoldenrod-cherry)

## TrialEmulation 0.0.3.9

CRAN release: 2024-09-09

- Improved documentation

- Add `estimand_type` argument

- Fix partial matching in data.table calls. Thanks
  [@joshhwuu](https://github.com/joshhwuu)

- Additional internal refactoring

## TrialEmulation 0.0.3.2

CRAN release: 2023-08-25

- Initial release to CRAN

## TrialEmulation 0.0.2.x

- Further feature development and code improvement

- Implemented case-control sampling

- Implemented
  [`predict()`](https://causal-lda.github.io/TrialEmulation/reference/predict_marginal.md)
  method

- Package tests and CI pipeline on github

## TrialEmulation 0.0.1

- Initial development by Roonak Rezvani as internship project

- Data preparation and model fitting functions for sequence of target
  trials

- Support for large data using `biglm`, `data.table` and chunked
  processing
