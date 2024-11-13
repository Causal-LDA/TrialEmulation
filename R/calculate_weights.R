calculate_weights_trial_seq <- function(object, quiet, switch_weights, censor_weights) {
  wt <- wtS <- wtC <- NULL

  set(object@data@data, j = "wt", value = 1)

  if (switch_weights) {
    if (!inherits(object@switch_weights, "te_weights_unset")) {
      object <- calculate_switch_weights(object)
      object@data@data[, wt := wt * wtS]
    }
  }
  if (censor_weights) {
    if (!inherits(object@censor_weights, "te_weights_unset")) {
      object <- calculate_censor_weights(object)
      object@data@data[, wt := wt * wtC]
    }
  }

  object
}



calculate_switch_weights <- function(object) {
  am_1 <- eligible_wts_0 <- eligible_wts_1 <- p_d <- p_n <- treatment <- wtS <- NULL

  # Switch from Treatment = 1
  data_1_expr <- if ("eligible_wts_1" %in% colnames(object@data@data)) {
    expression(am_1 == 1 & eligible_wts_1 == 1)
  } else {
    expression(am_1 == 1)
  }
  model_1_index <- object@data@data[eval(data_1_expr), which = TRUE]

  object@switch_weights@fitted[["n1"]] <- fit_weights_model(
    object = object@switch_weights@model_fitter,
    data = object@data@data[model_1_index, ],
    formula = object@switch_weights@numerator,
    label = "P(treatment = 1 | previous treatment = 1) for numerator"
  )
  set(object@data@data, i = model_1_index, j = "p_n", value = object@switch_weights@fitted[["n1"]]@fitted)
  object@switch_weights@data_subset_expr[["n1"]] <- data_1_expr

  object@switch_weights@fitted[["d1"]] <- fit_weights_model(
    object = object@switch_weights@model_fitter,
    data = object@data@data[model_1_index, ],
    formula = object@switch_weights@denominator,
    label = "P(treatment = 1 | previous treatment = 1) for denominator"
  )
  set(object@data@data, i = model_1_index, j = "p_d", value = object@switch_weights@fitted[["d1"]]@fitted)
  object@switch_weights@data_subset_expr[["d1"]] <- data_1_expr
  rm(model_1_index)


  # Switch from Treatment = 0
  data_0_expr <- if ("eligible_wts_1" %in% colnames(object@data@data)) {
    expression(am_1 == 0 & eligible_wts_1 == 0)
  } else {
    expression(am_1 == 0)
  }
  model_0_index <- object@data@data[eval(data_0_expr), which = TRUE]

  object@switch_weights@fitted[["n0"]] <- fit_weights_model(
    object = object@switch_weights@model_fitter,
    data = object@data@data[model_0_index, ],
    formula = object@switch_weights@numerator,
    label = "P(treatment = 1 | previous treatment = 0) for numerator"
  )
  set(object@data@data, i = model_0_index, j = "p_n", value = object@switch_weights@fitted[["n0"]]@fitted)
  object@switch_weights@data_subset_expr[["n0"]] <- data_0_expr

  object@switch_weights@fitted[["d0"]] <- fit_weights_model(
    object = object@switch_weights@model_fitter,
    data = object@data@data[model_0_index, ],
    formula = object@switch_weights@denominator,
    label = "P(treatment = 1 | previous treatment = 0) for denominator"
  )
  set(object@data@data, i = model_0_index, j = "p_d", value = object@switch_weights@fitted[["d0"]]@fitted)
  object@switch_weights@data_subset_expr[["d0"]] <- data_0_expr
  rm(model_0_index)


  # Combine weights
  if (any(c("eligible_wts_0", "eligible_wts_1") %in% colnames(object@data@data))) {
    object@data@data[
      (eligible_wts_0 == 1 | eligible_wts_1 == 1) & treatment == 0,
      wtS := (1.0 - p_n) / (1.0 - p_d)
    ]
    object@data@data[
      (eligible_wts_0 == 1 | eligible_wts_1 == 1) & treatment == 1,
      wtS := p_n / p_d
    ]
  } else {
    object@data@data[treatment == 0, wtS := (1.0 - p_n) / (1.0 - p_d)]
    object@data@data[treatment == 1, wtS := p_n / p_d]
  }

  object@data@data[is.na(wtS), wtS := 1]

  object
}


# Calculate censor weights ---------
calculate_censor_weights <- function(object) {
  am_1 <- pC_d <- pC_n <- wtC <- NULL

  if (!object@censor_weights@pool_numerator || !object@censor_weights@pool_denominator) {
    data_0_expr <- expression(am_1 == 0)
    data_1_expr <- expression(am_1 == 1)
    elig_0_index <- object@data@data[eval(data_0_expr), which = TRUE]
    elig_1_index <- object@data@data[eval(data_1_expr), which = TRUE]
  }
  data_pool_expr <- expression(TRUE)

  if (object@censor_weights@pool_numerator) {
    object@censor_weights@fitted[["n"]] <- fit_weights_model(
      object = object@censor_weights@model_fitter,
      data = object@data@data,
      formula = object@censor_weights@numerator,
      label = "P(censor_event = 0 | X) for numerator"
    )
    set(object@data@data, j = "pC_n", value = object@censor_weights@fitted[["n"]]@fitted)
    object@censor_weights@data_subset_expr[["n"]] <- data_pool_expr
  } else {
    object@censor_weights@fitted[["n0"]] <- fit_weights_model(
      object = object@censor_weights@model_fitter,
      data = object@data@data[elig_0_index, ],
      formula = object@censor_weights@numerator,
      label = "P(censor_event = 0 | X, previous treatment = 0) for numerator"
    )
    set(object@data@data, i = elig_0_index, j = "pC_n", value = object@censor_weights@fitted[["n0"]]@fitted)
    object@censor_weights@data_subset_expr[["n0"]] <- data_0_expr

    object@censor_weights@fitted[["n1"]] <- fit_weights_model(
      object = object@censor_weights@model_fitter,
      data = object@data@data[elig_1_index, ],
      formula = object@censor_weights@numerator,
      label = "P(censor_event = 0 | X, previous treatment = 1) for numerator"
    )
    set(object@data@data, i = elig_1_index, j = "pC_n", value = object@censor_weights@fitted[["n1"]]@fitted)
    object@censor_weights@data_subset_expr[["n1"]] <- data_1_expr
  }

  if (object@censor_weights@pool_denominator) {
    object@censor_weights@fitted[["d"]] <- fit_weights_model(
      object = object@censor_weights@model_fitter,
      data = object@data@data,
      formula = object@censor_weights@denominator,
      label = "P(censor_event = 0 | X) for denominator"
    )
    set(object@data@data, j = "pC_d", value = object@censor_weights@fitted[["d"]]@fitted)
    object@censor_weights@data_subset_expr[["d"]] <- data_pool_expr
  } else {
    object@censor_weights@fitted[["d0"]] <- fit_weights_model(
      object = object@censor_weights@model_fitter,
      data = object@data@data[elig_0_index, ],
      formula = object@censor_weights@denominator,
      label = "P(censor_event = 0 | X, previous treatment = 0) for denominator"
    )
    set(object@data@data, i = elig_0_index, j = "pC_d", value = object@censor_weights@fitted[["d0"]]@fitted)
    object@censor_weights@data_subset_expr[["d0"]] <- data_0_expr

    object@censor_weights@fitted[["d1"]] <- fit_weights_model(
      object = object@censor_weights@model_fitter,
      data = object@data@data[elig_1_index, ],
      formula = object@censor_weights@denominator,
      label = "P(censor_event = 0 | X, previous treatment = 1) for denominator"
    )
    set(object@data@data, i = elig_1_index, j = "pC_d", value = object@censor_weights@fitted[["d1"]]@fitted)
    object@censor_weights@data_subset_expr[["d1"]] <- data_1_expr
  }

  object@data@data[is.na(pC_d), pC_d := 1]
  object@data@data[is.na(pC_n), pC_n := 1]
  object@data@data[, wtC := pC_n / pC_d]

  object
}
