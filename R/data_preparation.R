#' Prepare data for the sequence of emulated target trials
#'
#' `r lifecycle::badge('stable')`
#'
#' This function  expands observational data in the person-time format (i.e., the  `long' format) to emulate a sequence
#' of target trials and also estimates the inverse probability of treatment and censoring weights as required.
#'
#' @inheritParams initiators
#' @param chunk_size Number of individuals whose data to  be processed in one chunk when `separate_files = TRUE`
#' @param separate_files Save expanded data in separate CSV files for each trial.
#' @param data_dir Directory to save model objects when `save_weight_models=TRUE` and expanded data as separate CSV
#'   files names as `trial_i.csv`s if `separate_files = TRUE`. If the specified directory does not exist it will be
#'   created. If the directory already contains trial files, an error will occur, other files may be overwritten.
#' @export
#'
#' @details The arguments `chunk_size` and `separate_files` allow for processing of large datasets that would not fit in
#' memory once expanded. When `separate_files = TRUE`, the input data are processed in chunks of individuals and saved
#' into separate files for each emulated trial. These separate files can be sampled by case-control sampling to create
#' a reduced dataset for the modelling.
#'
#' @returns An object of class `TE_data_prep`, which can either be sampled from ([case_control_sampling_trials]) or
#'   directly used in a model ([trial_msm]). It contains the elements
#' \describe{
#'   \item{data}{the expanded dataset for all emulated trials. If `separate_files = FALSE`, it is  a `data.table`; if
#'   `separate_files = TRUE`, it is a character vector with the file path of the expanded data as CSV files.}
#'   \item{min_period}{index for the first trial in the expanded data}
#'   \item{max_period}{index for the last trial in the expanded data}
#'   \item{N}{the total number of observations in the expanded data}
#'   \item{data_template}{a zero-row `data.frame`  with the columns and attributes of the expanded data}
#'   \item{switch_models}{a list of summaries of the models fitted for inverse probability of treatment weights,
#'   if `estimand_type` is `"PP"` or `"As-Treated"`}
#'   \item{censor_models}{a list of summaries of the models fitted for inverse probability of censoring weights,
#'   if `use_censor_weights=TRUE`}
#'  \item{args}{a list contain the parameters used to prepare the data and fit the weight models}
#'   }
#'
data_preparation <- function(data,
                             id = "id",
                             period = "period",
                             treatment = "treatment",
                             outcome = "outcome",
                             eligible = "eligible",
                             model_var = NULL,
                             outcome_cov = ~1,
                             estimand_type = c("ITT", "PP", "As-Treated"),
                             switch_n_cov = ~1,
                             switch_d_cov = ~1,
                             first_period = NA,
                             last_period = NA,
                             use_censor_weights = FALSE,
                             cense = NA,
                             pool_cense = c("none", "both", "numerator"),
                             cense_d_cov = ~1,
                             cense_n_cov = ~1,
                             eligible_wts_0 = NA,
                             eligible_wts_1 = NA,
                             where_var = NULL,
                             data_dir,
                             save_weight_models = FALSE,
                             glm_function = "glm",
                             chunk_size = 500,
                             separate_files = FALSE,
                             quiet = FALSE,
                             ...) {
  data <- as.data.table(data)
  # Check data_preparation arguments
  args <- check_args_data_preparation(
    data = data,
    id = id,
    period = period,
    treatment = treatment,
    outcome = outcome,
    eligible = eligible,
    model_var = model_var,
    outcome_cov = outcome_cov,
    estimand_type = estimand_type,
    switch_n_cov = switch_n_cov,
    switch_d_cov = switch_d_cov,
    first_period = first_period,
    last_period = last_period,
    use_censor_weights = use_censor_weights,
    cense = cense,
    pool_cense = pool_cense,
    cense_d_cov = cense_d_cov,
    cense_n_cov = cense_n_cov,
    eligible_wts_0 = eligible_wts_0,
    eligible_wts_1 = eligible_wts_1,
    where_var = where_var,
    data_dir = data_dir,
    save_weight_models = save_weight_models,
    glm_function = glm_function,
    chunk_size = chunk_size,
    separate_files = separate_files,
    quiet = quiet,
    ...
  )

  args <- check_estimand_data_prep(args)
  data <- select_data_cols(data, args)

  quiet_msg(quiet, "Starting data manipulation")
  data <- data_manipulation(data, use_censor = args$censor_at_switch)

  if (args$use_censor_weights || args$use_switch_weights) {
    weight_result <- do.call(
      "weight_func",
      args = c(list(sw_data = data), args_for_fun(args, "weight_func"), list(...))
    )
    data <- weight_result$data
  } else {
    set(data, j = "wt", value = 1)
  }

  quiet_msg(quiet, "Starting data extension")
  result <- do.call("data_extension", args = c(list(data = data), args_for_fun(args, "data_extension")))

  quiet_msg(quiet, "Summary of extended data:")
  quiet_msg(quiet, paste0("Number of observations: ", result$N))
  quiet_line(quiet)

  result$switch_models <- if (args$use_switch_weights) weight_result$switch_models else NULL
  result$censor_models <- if (args$use_censor_weights) weight_result$censor_models else NULL
  result$parameters <- args

  class(result) <- c(ifelse(args$separate_files, "TE_data_prep_sep", "TE_data_prep_dt"), "TE_data_prep")
  return(result)
}


#' Check for valid data_dir and create if necessary
#'
#' @param data_dir Directory to check
#' @noRd
check_data_dir <- function(data_dir) {
  if (test_directory_exists(data_dir)) {
    if (length(list.files(data_dir, pattern = "trial_.*csv"))) {
      stop("trial_*.csv files already exist in ", data_dir, ". Remove them or specify a different `data_dir`.")
    }
    if (length(list.files(data_dir, ".*model.*rds"))) {
      warning(data_dir, " contains model rds files. These may be overwritten.")
    }
  } else {
    if (!dir.create(data_dir)) {
      stop(data_dir, " could not be created.")
    }
  }
  data_dir
}

#' Check arguments to data_preparation and convert types
#'
#' @noRd
check_args_data_preparation <- function(data,
                                        id,
                                        period,
                                        treatment,
                                        outcome,
                                        eligible,
                                        model_var,
                                        outcome_cov,
                                        estimand_type,
                                        switch_n_cov,
                                        switch_d_cov,
                                        first_period,
                                        last_period,
                                        use_censor_weights,
                                        cense,
                                        pool_cense,
                                        cense_d_cov,
                                        cense_n_cov,
                                        eligible_wts_0,
                                        eligible_wts_1,
                                        where_var,
                                        data_dir,
                                        save_weight_models,
                                        glm_function,
                                        chunk_size,
                                        separate_files,
                                        quiet,
                                        ...) {
  arg_checks <- makeAssertCollection()
  assert_flag(use_censor_weights, add = arg_checks)
  assert_flag(save_weight_models, add = arg_checks)
  assert_flag(separate_files, add = arg_checks)
  assert_flag(quiet, add = arg_checks)
  assert_names(colnames(data), must.include = c(id, period, treatment, outcome, eligible), add = arg_checks)
  assert_multi_class(outcome_cov, classes = c("formula", "character"), add = arg_checks)
  assert_multi_class(switch_n_cov, classes = c("formula", "character"), add = arg_checks)
  assert_multi_class(switch_d_cov, classes = c("formula", "character"), add = arg_checks)
  assert_multi_class(cense_d_cov, classes = c("formula", "character"), add = arg_checks)
  assert_multi_class(cense_n_cov, classes = c("formula", "character"), add = arg_checks)
  assert_integerish(first_period, lower = 0, all.missing = TRUE, len = 1, add = arg_checks)
  assert_integerish(last_period, lower = 0, all.missing = TRUE, len = 1, add = arg_checks)
  assert_choice(estimand_type, choices = c("ITT", "PP", "As-Treated"), add = arg_checks)
  if (use_censor_weights) assert_string(cense, add = arg_checks)
  reportAssertions(arg_checks)

  if ("use_weight" %in% ...names()) {
    stop(
      "Argument `use_weight` is no longer supported. Use `estimand_type` to control weighting behaviour.",
      call. = FALSE
    )
  }

  data_dir <- if (isTRUE(separate_files || save_weight_models)) check_data_dir(data_dir) else NA

  outcome_cov <- as_formula(outcome_cov)
  switch_n_cov <- as_formula(switch_n_cov)
  switch_d_cov <- as_formula(switch_d_cov)
  cense_d_cov <- as_formula(cense_d_cov)
  cense_n_cov <- as_formula(cense_n_cov)
  keeplist <- c(
    "id", "trial_period", "followup_time", "outcome", "weight", "treatment",
    where_var, all.vars(outcome_cov)
  )

  args <- list(
    id = id,
    period = period,
    treatment = treatment,
    outcome = outcome,
    eligible = eligible,
    model_var = model_var,
    outcome_cov = outcome_cov,
    estimand_type = estimand_type,
    switch_n_cov = switch_n_cov,
    switch_d_cov = switch_d_cov,
    first_period = first_period,
    last_period = last_period,
    use_censor_weights = use_censor_weights,
    cense = cense,
    pool_cense = pool_cense,
    cense_d_cov = cense_d_cov,
    cense_n_cov = cense_n_cov,
    eligible_wts_0 = eligible_wts_0,
    eligible_wts_1 = eligible_wts_1,
    where_var = where_var,
    data_dir = data_dir,
    save_weight_models = save_weight_models,
    glm_function = glm_function,
    chunk_size = chunk_size,
    separate_files = separate_files,
    quiet = quiet,
    keeplist = keeplist
  )
}

check_estimand_data_prep <- function(args) {
  estimand_type <- args$estimand_type
  model_var <- args$model_var
  use_censor_weights <- args$use_censor_weights
  pool_cense <- args$pool_cense

  if (estimand_type == "ITT") {
    model_var <- ~assigned_treatment
    censor_at_switch <- FALSE
    use_switch_weights <- FALSE
    if (use_censor_weights) {
      if (all(pool_cense == c("none", "both", "numerator"))) {
        message("Setting pool_cense to default \"numerator\" for estimand_type = \"ITT\"")
        pool_cense <- "numerator"
      }
      assert_choice(pool_cense, c("both", "numerator"))
    }
  } else if (estimand_type == "PP") {
    model_var <- ~assigned_treatment
    censor_at_switch <- TRUE
    use_switch_weights <- TRUE
    if (use_censor_weights) {
      if (all(pool_cense == c("none", "both", "numerator"))) {
        message("Setting pool_cense to default \"none\" for estimand_type = \"PP\"")
        pool_cense <- "none"
      }
      assert_choice(pool_cense, c("none", "both", "numerator"))
    }
  } else if (estimand_type == "As-Treated") {
    model_var <- if (is.null(model_var)) {
      ~dose
    } else {
      as_formula(model_var)
    }
    censor_at_switch <- FALSE
    use_switch_weights <- TRUE
    if (use_censor_weights) {
      if (all(pool_cense == c("none", "both", "numerator"))) {
        message("Setting pool_cense to default \"none\" for estimand_type = \"As-Treated\"")
        pool_cense <- "none"
      }
      assert_choice(pool_cense, c("none", "both", "numerator"))
    }
  }

  args$pool_cense_n <- pool_cense %in% c("both", "numerator")
  args$pool_cense_d <- pool_cense == "both"
  args$model_var <- model_var
  args$censor_at_switch <- censor_at_switch
  args$use_switch_weights <- use_switch_weights
  args$keeplist <- unique(c(args$keeplist, all.vars(args$model_var)))

  args
}


args_for_fun <- function(args, fun) {
  args[intersect(names(args), names(formals(fun)))]
}
