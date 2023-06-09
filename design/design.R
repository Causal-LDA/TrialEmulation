

trial_emulation_settings <- function(
    data_columns = list(outcome, cens, ...),
    switch_weight_models = switch_weight_models(),
    censor_weight_models = censor_weight_models(),
    outcome_model = outcome_model(),
    sampling = case_control_sampling(),
    expansion_settings =  expansion_settings(),
    ...
) {
 list(...)
}

settings_check <- function(settings, data) {

}

summary(settings)

prepare_data <- function(
    data,
    settings_object
) {
  data <- select_data_cols(data, settings_object)
  data <- data_manipulation(data)
  data
}

calculate_weights <- function(
    data,
    settings_object
) {
  weight_func(data, settings)
}

set_weights <- function(data, weights_object) {
  data$weights <- weights(weights_object)
}

expand_trial_sequence <- function(data, settings) {
  data_extension(data, settings)
}

sample_from_trials <- function(trial_sequence, settings) {
  case_control_sampling_trials(trial_sequence, settings)
}

fit_pooled_logistic <- function(data, settings) {
  weights <- limit_weights(data, settings)
  model.full <- fit_glm(data, weights, settings)
  robust_model <- robust_calculation(model.full, data[["id"]])
  list(model.full, robust_model)
}

marginal_effects <- function(model, settings) {
  predict(model)
}
