setClass("te_outcome_model",
  slots = c(
    formula = "formula",
    treatment_var = "character"
  )
)


setClass("te_expansion",
  slots = c(
    censor_at_switch = "logical"
  )
)




setClass("te_expanded_datastore")
# eg data.table, sqlite, csv





# setClass("trial_sequence",
#          slots = c(
#            data = "te_data",
#            estimand = "te_estimand",
#            switch_weight = "te_weight",
#            censor_weight = "te_weight",
#            expansion = "te_expansion",
#            outcome_model = "te_outcome_model"
#            expanded_data = "te_expanded_data",
#          )
# )
