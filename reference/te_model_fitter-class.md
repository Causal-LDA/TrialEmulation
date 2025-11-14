# Outcome Model Fitter Class

This is a virtual class which other outcome model fitter classes should
inherit from. Objects of these class exist to define how the outcome
models are fit. They are used for the dispatch of the internal methods
[fit_outcome_model](https://causal-lda.github.io/TrialEmulation/reference/fit_outcome_model.md),
[fit_weights_model](https://causal-lda.github.io/TrialEmulation/reference/fit_weights_model.md)
and
[predict](https://causal-lda.github.io/TrialEmulation/reference/predict_marginal.md).

## See also

Other model_fitter:
[`parsnip_model()`](https://causal-lda.github.io/TrialEmulation/reference/parsnip_model.md),
[`stats_glm_logit()`](https://causal-lda.github.io/TrialEmulation/reference/stats_glm_logit.md)
