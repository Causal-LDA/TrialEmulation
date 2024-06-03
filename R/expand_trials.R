expand_trials_trial_seq <- function(object) {
  data <- object@data@data
  first_period <- object@expansion@first_period
  last_period <- object@expansion@last_period
  chunk_size <- object@expansion@chunk_size
  censor_at_switch <- object@expansion@censor_at_switch

  outcome_adj_vars <- unique(object@outcome_model@adjustment_vars)
  keeplist <- unique(c(
    "id", "trial_period", "followup_time", "outcome", "weight", "treatment",
    outcome_adj_vars, object@outcome_model@treatment_var
  ))

  all_ids <- unique(data$id)
  ids_split <- if (chunk_size == 0) {
    list(all_ids)
  } else if (chunk_size > 0) {
    split(all_ids, ceiling(seq_along(all_ids) / chunk_size))
  }
  for (ids in ids_split) {
    switch_data <- expand(
      sw_data = data[list(ids), ],
      outcomeCov_var = outcome_adj_vars,
      where_var = NA,
      use_censor = censor_at_switch,
      minperiod = first_period,
      maxperiod = last_period,
      keeplist = keeplist
    )
    object@expansion@datastore <- save_expanded_data(object@expansion@datastore, switch_data)
  }
  object
}
