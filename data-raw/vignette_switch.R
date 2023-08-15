prep_data <- data_preparation(
  data = trial_example,
  id = "id",
  period = "period",
  eligible = "eligible",
  treatment = "treatment",
  outcome = "outcome",
  model_var = "assigned_treatment",
  outcome_cov = ~ catvarA + catvarB + catvarC + nvarA + nvarB + nvarC,
  save_weight_models = FALSE,
  use_censor = FALSE,
  use_weight = FALSE,
  switch_n_cov = ~ nvarA + nvarB,
  quiet = TRUE
)
vignette_switch_data <- as.data.frame(prep_data$data)
# similar to usethis::use_data
if (FALSE) {
  # only run this if you're sure!
  save(vignette_switch_data, file = "data/vignette_switch_data.rda", compress = "bzip2")
}
