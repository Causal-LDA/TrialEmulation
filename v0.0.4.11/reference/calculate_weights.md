# Calculate Inverse Probability of Censoring Weights

**\[experimental\]**

## Usage

``` r
calculate_weights(object, ...)

# S4 method for class 'trial_sequence_ITT'
calculate_weights(object, quiet = FALSE)

# S4 method for class 'trial_sequence_AT'
calculate_weights(object, quiet = FALSE)

# S4 method for class 'trial_sequence_PP'
calculate_weights(object, quiet = FALSE)
```

## Arguments

- object:

  A
  [trial_sequence](https://causal-lda.github.io/TrialEmulation/reference/trial_sequence.md)
  object

- ...:

  Other arguments used by methods.

- quiet:

  Prints model summaries is `TRUE`.

## Value

A
[trial_sequence](https://causal-lda.github.io/TrialEmulation/reference/trial_sequence.md)
object with updated `censor_weights` and/or `switch_weights` slots

## Examples

``` r
save_dir <- file.path(tempdir(), "switch_models")
ts <- trial_sequence("PP") |>
  set_data(
    data = data_censored,
    id = "id",
    period = "period",
    treatment = "treatment",
    outcome = "outcome",
    eligible = "eligible"
  ) |>
  set_switch_weight_model(
    numerator = ~ age + x1 + x3,
    denominator = ~age,
    model_fitter = stats_glm_logit(save_path = save_dir)
  ) |>
  calculate_weights()
```
