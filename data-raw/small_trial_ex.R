dat <- trial_example[trial_example$id < 200, ]

te_data_ex <- data_preparation(
  data = dat,
  outcome_cov = c("nvarA", "catvarA"),
  first_period = 260,
  last_period = 280
)

te_model_ex <- trial_msm(
  data = data_subset,
  outcome_cov = c("catvarA", "nvarA"),
  last_followup = 40,
  model_var = "assigned_treatment",
  include_followup_time = ~followup_time,
  include_trial_period = ~trial_period,
  use_sample_weights = FALSE,
  quiet = TRUE,
  glm_function = "glm"
)

# similar to usethis::use_data
if (FALSE) {
  # only run this if you're sure!
  save(te_data_ex, file = "data/te_data_ex.rda", compress = "bzip2")
  save(te_model_ex, file = "data/te_model_ex.rda", compress = "xz")
}
