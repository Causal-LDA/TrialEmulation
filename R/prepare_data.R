#' Prepare Data for Trial Emulation
#'
#' @param data A data frame
#' @param setup A `TE_setup` object from [trial_emulation_setup()]
#'
#' @return A `TE_data` object containing a data frame `data`
#' @export
prepare_data <- function(data,
                         setup) {
  check_setup(setup)
  data_setup <- get_data_setup(setup)

  TE_data <- select_data_cols(
    data,
    id = data_setup$id,
    period = data_setup$period,
    treatment = data_setup$treatment,
    outcome = data_setup$outcome,
    eligible = data_setup$eligible,
    eligible_wts_0 = data_setup$eligible_wts_0,
    eligible_wts_1 = data_setup$eligible_wts_1,
    formula_vars = data_setup$formula_vars,
    cense = data_setup$cense,
    where_var = data_setup$where_var
  )

  TE_data <- data_manipulation(TE_data, use_censor = data_setup$use_censor)

  result <- list(data = TE_data, data_setup = data_setup)
  class(result) <- "TE_data"
  result
}
