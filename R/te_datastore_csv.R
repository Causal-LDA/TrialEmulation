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
    files = "character",
    template = "data.frame"
  )
)


#' Save expanded data as CSV
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
    object@files <- file.path(data_dir, paste0("trial_", periods, ".csv"))
    if (!ncol(object@template)) object@template <- data[0, ]
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


#' @rdname te_datastore_csv-class
#' @include trial_sequence.R
setMethod(
  f = "sample_expanded_data",
  signature = "te_datastore_csv",
  definition = function(object, p_control, period, subset_condition = NULL, seed) {
    old_seed <- globalenv()$.Random.seed
    on.exit(suspendInterrupts(set_random_seed(old_seed)))
    set.seed(seed)

    if (is.null(period)) {
      for (n in seq_len(length(object@files))) {
        period[n] <- substr(object@files[n], nchar(object@path) + 8, nchar(object@files)[n] - 4)
      }
    period <- as.numeric(period)
    }

    i <- 0
    data <- list()
    for (p in period) {
      i <- i + 1
      data[[i]] <- read_expanded_data(object, period = p, subset_condition = subset_condition)
      data[[i]] <- lapply(
        split(data[[i]], data[[i]]$followup_time, drop = TRUE),
        do_sampling,
        p_control = p_control
      )
    }

    data_table <- data.table::rbindlist(lapply(data, data.table::rbindlist))
    data_table
  }
)
