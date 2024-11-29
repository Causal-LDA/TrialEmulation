#' @include te_datastore.R generics.R
NULL


#' @title te_datastore_csv, functions and methods
#'
#' @slot path path to csv files.
#' @slot files names of csv files.
#' @slot template data.frame template.
#'
#' @return A 'te_datastore_csv' object.
#' @keywords internal
#'
setClass(
  "te_datastore_csv",
  contains = "te_datastore",
  slots = c(
    path = "character",
    files = "data.frame",
    template = "data.frame"
  ),
  prototype = list(
    files = data.frame(file = character(0L), period = numeric(0L))
  )
)

setValidity("te_datastore_csv", function(object) {
  msg <- character(0L)

  if (!(check1 <- check_data_frame(object@files, ncols = 2))) msg <- c(msg, check1)
  if (!(check2 <- check_names(colnames(object@files), identical.to = c("file", "period")))) msg <- c(msg, check2)

  check3 <- NULL
  for (n in seq_along(object@files$file)) {
    check3[n] <- grepl(
      x = object@files$file[n],
      pattern = paste0("trial_", object@files$period[n], ".csv")
    )
  }
  if (!all(check3)) {
    msg <- c(msg, "@files$file and @files$period don't match")
  }
  if (length(msg)) msg else TRUE
})

# Show
setMethod(
  "show",
  c(object = "te_datastore_csv"),
  function(object) {
    catn("A TE Datastore CSV object")
    catn("N:", object@N, "observations")
    catn("Periods:", object@files$period)
    catn("Path:", object@path)
    catn("Columns:", paste0(colnames(object@template), collapse = ", "))
  }
)


#' Save expanded data as CSV
#'
#' `r lifecycle::badge('experimental')`
#' @param path Directory to save CSV files in. Must be empty.
#' @family save_to
#' @return A [te_datastore_csv-class] object.
#' @export
#' @examples
#' csv_dir <- file.path(tempdir(), "expanded_trials_csv")
#' dir.create(csv_dir)
#' csv_datastore <- save_to_csv(path = csv_dir)
#'
#' trial_to_expand <- trial_sequence("ITT") |>
#'   set_data(data = data_censored) |>
#'   set_expansion_options(output = csv_datastore, chunk_size = 500)
#'
#' # Delete directory after use
#' unlink(csv_dir)
#'
save_to_csv <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path)
  } else {
    if (length(dir(path))) {
      stop(path, " must be empty")
    }
  }
  new("te_datastore_csv", path = path, N = 0L)
}


#' @rdname te_datastore_csv-class
#' @inherit save_expanded_data
setMethod(
  f = "save_expanded_data",
  signature = "te_datastore_csv",
  definition = function(object, data) {
    trial_period <- NULL
    data_dir <- object@path
    assert_directory_exists(data_dir)
    periods <- unique(data[["trial_period"]])
    for (p in periods) {
      file_p <- file.path(data_dir, paste0("trial_", p, ".csv"))
      fwrite(data[data$trial_period == p, ], file = file_p, append = TRUE)
    }
    object@N <- object@N + nrow(data)
    object@files <- data.frame(
      "file" = file.path(data_dir, paste0("trial_", periods, ".csv")),
      "period" = periods
    )
    if (!ncol(object@template)) object@template <- data[0, ]
    validObject(object)
    object
  }
)


#' @rdname te_datastore_csv-class
#' @inherit read_expanded_data
setMethod(
  f = "read_expanded_data",
  signature = "te_datastore_csv",
  definition = function(object, period, subset_condition) {
    checkmate::assert_integerish(period, null.ok = TRUE, any.missing = FALSE, lower = 0)
    if (use_subset <- !is.null(subset_condition)) {
      subset_expr <- str2lang(subset_condition)
    }

    files <- if (is.null(period)) {
      object@files$file
    } else {
      object@files[object@files$period %in% period, ]$file
    }
    data_table <- data.table::rbindlist(lapply(files, data.table::fread))
    if (use_subset) {
      data_table <- data_table[eval(subset_expr)]
    }
    data_table
  }
)


#' @rdname te_datastore_csv-class
#' @include trial_sequence.R
setMethod(
  f = "sample_expanded_data",
  signature = "te_datastore_csv",
  definition = function(object, p_control, period, subset_condition = NULL, seed) {
    old_seed <- globalenv()$.Random.seed
    on.exit(suspendInterrupts(set_random_seed(old_seed)))
    set.seed(seed)

    all_periods <- object@files$period

    if (is.null(period)) {
      periods <- all_periods
    } else if (all(period %in% all_periods)) {
      periods <- period
    } else {
      periods <- period[period %in% all_periods]
      warning(
        "The following periods don't exist in the data and were omitted: ",
        toString(period[!(period %in% all_periods)])
      )
    }

    rbindlist(
      lapply(periods, function(p) {
        dt <- read_expanded_data(object, period = p, subset_condition)
        dt_sample <- dt[, do_sampling(.SD, p_control = p_control), by = "followup_time"]
        setcolorder(dt_sample, colnames(dt))
        dt_sample
      })
    )
  }
)
