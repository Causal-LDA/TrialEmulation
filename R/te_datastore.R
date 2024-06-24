#' @include generics.R




#' @title te_datastore
#' @description
#' This is the parent class for classes which define how the expanded trial data should be stored.
#' To define a new storage type, a new class should be defined which inherits from `te_datastore`. In addition, methods
#' [save_expanded_data] and `read_expanded_data` need to be defined for the new class.
#' @name te_datastore-class
#' @slot N The number of observations in this data. Initially 0.
#'
#' @return A 'te_datastore' object
#' @export
setClass("te_datastore",
  slots = c(N = "integer"),
  prototype = list(N = 0L)
)

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




# save to data.table -------
setClass(
  "te_datastore_datatable",
  contains = "te_datastore",
  slots = c(
    data = "data.table"
  )
)


#' Save expanded data as a `data.table`
#' @family save_to
#' @export
#' @examples
#' trial_to_expand <- trial_sequence("ITT") |>
#'   set_data(data = data_censored) |>
#'   set_expansion_options(output = save_to_datatable(), chunk_size = 500)
save_to_datatable <- function() {
  new("te_datastore_datatable", data = data.table(), N = 0L)
}



# save to duckdb -------
#' @importClassesFrom duckdb duckdb_connection
setClass(
  "te_datastore_duckdb",
  contains = "te_datastore",
  slots = c(
    path = "character",
    table = "character",
    con = "duckdb_connection"
  )
)

#' Save expanded data to `DuckDB`
#' @param path Directory to save `DuckDB` database file in.
#' @family save_to
#' @export
#' @examples
#' if (require(duckdb)) {
#'   duckdb_dir <- file.path(tempdir(), "expanded_trials_duckdb")
#'
#'   trial_to_expand <- trial_sequence("ITT") |>
#'     set_data(data = data_censored) |>
#'     set_expansion_options(output = save_to_duckdb(path = duckdb_dir), chunk_size = 500)
#'
#'   # Delete directory after use
#'   unlink(duckdb_dir)
#' }
#'
save_to_duckdb <- function(path) {
  if (!requireNamespace("duckdb")) stop("duckdb package is required but not installed.")
  if (!dir.exists(path)) {
    dir.create(path)
  }
  file_path <- tempfile(pattern = "expanded_data_", tmpdir = path, fileext = ".duckdb")
  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = file_path, read_only = FALSE)
  new("te_datastore_duckdb", path = file_path, N = 0L, con = con)
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

#' @rdname save_expanded_data
setMethod(
  f = "save_expanded_data",
  signature = "te_datastore_datatable",
  definition = function(object, data) {
    object@data <- rbind(object@data, data)
    object@N <- nrow(object@data)
    object
  }
)


#' @rdname save_expanded_data
setMethod(
  f = "save_expanded_data",
  signature = "te_datastore_duckdb",
  definition = function(object, data) {
    assert_file_exists(object@path)

    if (!duckdb::dbExistsTable(conn = object@con, name = "trial_data")) {
      duckdb::dbWriteTable(conn = object@con, name = "trial_data", value = data)
    } else {
      duckdb::dbAppendTable(conn = object@con, name = "trial_data", value = data)
    }

    object@N <- object@N + nrow(data)

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


#' @rdname read_expanded_data
setMethod(
  f = "read_expanded_data",
  signature = "te_datastore_datatable",
  definition = function(object, period, subset_condition) {
    checkmate::assert_integerish(period, null.ok = TRUE, any.missing = FALSE, lower = 0)
    if (use_subset <- !is.null(subset_condition)) {
      subset_expr <- str2lang(subset_condition)
    }
    p <- period
    data_table <- if (is.null(period)) {
      object@data
    } else {
      object@data[period %in% p, ]
    }
    if (use_subset) {
      data_table <- data_table[eval(subset_expr)]
    }
    data_table
  }
)


#' @rdname read_expanded_data
setMethod(
  f = "read_expanded_data",
  signature = "te_datastore_duckdb",
  definition = function(object, period, subset_condition) {
    checkmate::assert_integerish(period, null.ok = TRUE, any.missing = FALSE, lower = 0)
    if (use_subset <- !is.null(subset_condition)) {
      subset_expr <- str2lang(subset_condition)
    }
    query <- if (is.null(period)) {
      "SELECT * FROM trial_data"
    } else {
      paste0("SELECT * FROM trial_data WHERE trial_period IN (", paste0(period, collapse = ", "), ")")
    }
    data_table <- data.table::as.data.table(DBI::dbGetQuery(conn = object@con, statement = query))
    if (use_subset) {
      data_table <- data_table[eval(subset_expr)]
    }
    data_table
  }
)
