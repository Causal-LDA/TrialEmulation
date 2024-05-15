expand_trials_trial_seq <- function(object, censor_at_switch, keeplist) {
  data <- object@data@data
  first_period <- object@expansion@first_period
  last_period <- object@expansion@last_period
  chunk_size <- object@expansion@chunk_size

  outcomeCov_var <- object@data@expand_variables
  keeplist <- c(
    "id", "trial_period", "followup_time", "outcome", "weight", "treatment",
    keeplist, outcomeCov_var
  )
  keeplist <- keeplist[!is.na(keeplist)]
  # TODO also need model var. Ultimately better to have the outcome model specified before expansion

  all_ids <- unique(data$id)
  ids_split <- split(all_ids, ceiling(seq_along(all_ids) / chunk_size))
  for (ids in ids_split) {
    switch_data <- expand(
      sw_data = data[list(ids), ],
      outcomeCov_var = outcomeCov_var,
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
