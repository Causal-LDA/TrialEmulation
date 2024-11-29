#' @include te_datastore.R generics.R
NULL


#' @title te_datastore_duckdb, functions and methods
#'
#' @slot path Path to the duckdb file containing the data.
#' @slot table .
#' @slot con S4 object of class duckdb_connection.
#'
#' @return A 'te_datastore_duckdb' object.
#' @importClassesFrom duckdb duckdb_connection
#' @keywords internal
#'
setClass(
  "te_datastore_duckdb",
  contains = "te_datastore",
  slots = c(
    path = "character",
    table = "character",
    con = "duckdb_connection"
  )
)

# Show
setMethod(
  "show",
  c(object = "te_datastore_duckdb"),
  function(object) {
    catn("A TE Datastore DuckDB object")
    catn("N:", object@N, "observations")
    catn("Path:", object@path)
  }
)


#' Save expanded data to `DuckDB`
#'
#' `r lifecycle::badge('experimental')`
#' @param path Directory to save `DuckDB` database file in.
#' @family save_to
#' @return A [te_datastore_duckdb-class] object.
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
  new("te_datastore_duckdb", path = file_path, N = 0L, table = "trial_data", con = con)
}


#' @rdname te_datastore_duckdb-class
#' @inherit save_expanded_data
setMethod(
  f = "save_expanded_data",
  signature = "te_datastore_duckdb",
  definition = function(object, data) {
    assert_file_exists(object@path)
    if (!duckdb::dbExistsTable(conn = object@con, name = object@table)) {
      duckdb::dbWriteTable(conn = object@con, name = object@table, value = data)
    } else {
      duckdb::dbAppendTable(conn = object@con, name = object@table, value = data)
    }

    object@N <- object@N + nrow(data)

    object
  }
)


#' @rdname te_datastore_duckdb-class
#' @inherit read_expanded_data
setMethod(
  f = "read_expanded_data",
  signature = "te_datastore_duckdb",
  definition = function(object, period, subset_condition) {
    checkmate::assert_integerish(period, null.ok = TRUE, any.missing = FALSE, lower = 0)
    if (use_subset <- !is.null(subset_condition)) {
      subset_expr <- translate_to_sql(subset_condition)
    }
    q_p1 <- paste0("SELECT * FROM ", object@table)
    q_p2 <- if (!is.null(period) | use_subset) " WHERE" else ""
    q_period <- if (!is.null(period)) paste0(" trial_period IN (", paste0(period, collapse = ", "), ")") else ""
    q_p3 <- if (!is.null(period) & use_subset) " AND" else ""
    q_subset <- if (use_subset) paste0(" (", subset_expr, ")") else ""

    query <- paste0(q_p1, q_p2, q_period, q_p3, q_subset)

    data_table <- data.table::as.data.table(DBI::dbGetQuery(conn = object@con, statement = query))

    data_table
  }
)


#' @rdname te_datastore_duckdb-class
#' @inherit sample_expanded_data
setMethod(
  f = "sample_expanded_data",
  signature = "te_datastore_duckdb",
  definition = function(object, p_control, period, subset_condition, seed) {
    if (use_subset <- !is.null(subset_condition)) {
      subset_expr <- translate_to_sql(subset_condition)
    }
    q_p1 <- paste0("SELECT * FROM (SELECT * FROM ", object@table, " WHERE outcome = 0 ")
    q_p2 <- paste0("UNION SELECT * FROM ", object@table, " WHERE outcome = 1 ")
    if (is.null(seed)) {
      q_sample <- paste0("USING SAMPLE ", p_control * 100, " PERCENT (bernoulli) ")
    } else {
      q_sample <- paste0("USING SAMPLE ", p_control * 100, " PERCENT (bernoulli, ", seed, ") ")
    }
    q_period <- if (!is.null(period)) paste0("AND trial_period IN (", paste0(period, collapse = ", "), ") ") else ""
    q_subset <- if (use_subset) paste0("AND (", subset_expr, ")") else ""

    query <- paste0(q_p1, q_period, q_subset, ") ", q_sample, q_p2, q_period, q_subset)

    data <- DBI::dbGetQuery(conn = object@con, statement = query)
    data["sample_weight"] <- ifelse(data$outcome == 1, 1, 1 / p_control)
    data_table <- data.table::as.data.table(data)
    data_table
  }
)


#' Translate subset_condition to SQL syntax
#'
#' @param string subset_condition as a string
#'
#' @return a string
#'
#' @noRd
translate_to_sql <- function(string) {
  tryCatch(
    {
      replacement <- c("\\|" = "OR", "&" = "AND", "==" = "=", "%in%" = "IN", "^c\\(" = "\\(")

      vec <- strsplit(string, " ")[[1]]

      if (length(grep(":", vec)) != 0) {
        vec <- translate_num_vec(vec)
      }

      for (i in seq_len(length(replacement))) {
        vec <- gsub(pattern = names(replacement)[i], replacement = replacement[i], vec)
      }
      string <- paste0(vec, collapse = " ")
      string
    },
    error = function(e) {
      stop("Error translating subset_condition to SQL. See ?sample_controls for me information.\n",
        as.character(e),
        call. = FALSE
      )
    }
  )
}


#' Translate numerical vectors to SQL syntax
#'
#' @param vec a vector obtained by splitting subset_condition string from sample_controls
#'
#' @return a vector
#'
#' @noRd
translate_num_vec <- function(vec) {
  for (i in grep(":", vec)) {
    vec[i] <- paste0("(", paste0(
      seq(
        strsplit(vec[i], ":")[[1]][1],
        strsplit(vec[i], ":")[[1]][2]
      ),
      collapse = ", "
    ), ")")
  }
  vec
}
