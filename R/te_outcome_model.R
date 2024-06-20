setClass("te_outcome_model",
  slots = c(
    formula = "formula",
    treatment_var = "character",
    adjustment_vars = "character",
    model_fitter = "te_model_fitter"
  )
)
