# TrialEmulation 0.0.3.33

* Add new extensible S4 class based interface.
  - Step-by-step functions to construct sequence of target trial analysis. See the New Interface vignette.
  - Use `duckdb` to store expanded data for efficient reading and sampling of large data.
  - Possibility to add alternative data storage and model fitting functionality. See the Extending TrialEmulation 
   vignette.
  - This interface is still "experimental" and may change in future
  - Thanks @darkgoldenrod-cherry

* Improved documentation

* Add `estimand_type` argument

* Fix partial matching in data.table calls. Thanks @joshhwuu

# TrialEmulation 0.0.3.2

* Initial release to CRAN

# TrialEmulation 0.0.2.x

* Further feature development and code improvement

* Implemented case-control sampling

* Implemented `predict()` method

* Package tests and CI pipeline on github

# TrialEmulation 0.0.1

* Initial development by Roonak Rezvani as internship project

* Data preparation and model fitting functions for sequence of target trials

* Support for large data using `biglm`, `data.table` and chunked processing
