# Fitted Outcome Model Object

Fitted Outcome Model Object

## Slots

- `formula`:

  `formula` object for the model fitting

- `adjustment_vars`:

  character. Adjustment variables

- `treatment_var`:

  Variable used for treatment

- `stabilised_weights_terms`:

  formula. Adjustment terms from numerator models of stabilised weights.
  These must be included in the outcome model.

- `adjustment_terms`:

  formula. User specified terms to include in the outcome model

- `treatment_terms`:

  formula. Estimand defined treatment term

- `followup_time_terms`:

  formula. Terms to model follow up time within an emulated trial

- `trial_period_terms`:

  formula. Terms to model start time ("trial_period") of an emulated
  trial

- `model_fitter`:

  Model fitter object

- `fitted`:

  list. Saves the model objects
