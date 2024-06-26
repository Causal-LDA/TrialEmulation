#' @includes te_datastore.R generics.R
NULL


# save to csv -------
setClass(
  "te_datastore_csv",
  contains = "te_datastore",
  slots = c(
    path = "character",
    files = "character",
    template = "data.frame"
  )
)


#' Save expanded data as CSV
#' @param path Directory to save CSV files in. Must be empty.
#' @family save_to
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


#' @rdname save_expanded_data
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
    object@files <- file.path(data_dir, paste0("trial_", periods, ".csv"))
    if (!ncol(object@template)) object@template <- data[0, ]
    object
  }
)


#' @rdname read_expanded_data
setMethod(
  f = "read_expanded_data",
  signature = "te_datastore_csv",
  definition = function(object, period, subset_condition) {
    checkmate::assert_integerish(period, null.ok = TRUE, any.missing = FALSE, lower = 0)
    if (use_subset <- !is.null(subset_condition)) {
      subset_expr <- str2lang(subset_condition)
    }
    all_files <- object@files
    files <- if (is.null(period)) {
      all_files
    } else {
      grep(x = all_files, pattern = paste0("trial_", period, ".csv", collapse = "|"), value = TRUE)
    }
    data_table <- data.table::rbindlist(lapply(files, data.table::fread))
    if (use_subset) {
      data_table <- data_table[eval(subset_expr)]
    }
    data_table
  }
)
