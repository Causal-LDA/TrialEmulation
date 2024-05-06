setClass("te_outcome_model",
  slots = c(
    formula = "formula",
    treatment_var = "character"
  )
)

# Specification for saving expanded data
# - allows setting of parameters in constructors
# - allows dispatch of method for saving expanded records
setClass("te_datastore",
  slots = c(empty = "logical"),
  prototype = list(empty = TRUE)
)

setClass(
  "te_datastore_csv",
  contains = "te_datastore",
  slots = c(
    path = "character",
    files = "character",
    template = "data.frame",
    N = "integer"
  )
)

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

save_to_data.table <- function(...) {
  new("te_datastore")
}


# duckdb
#' @importClassesFrom duckdb duckdb_connection
setClass(
  "te_datastore_duckdb",
  contains = "te_datastore",
  slots = c(
    path = "character",
    table = "character",
    con = "duckdb_connection",
    N = "integer"
  )
)

save_to_duckdb <- function(path) {
  if (!requireNamespace("duckdb")) stop("duckdb package is required but not installed.")
  if (!dir.exists(path)) {
    dir.create(path)
  }
  file_path <- tempfile(pattern = "expanded_data_", tmpdir = path, fileext = ".duckdb")
  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = file_path, read_only = FALSE)
  new("te_datastore_duckdb", path = file_path, N = 0L, con = con)
}


setClass("te_expansion",
  slots = c(
    chunks = "numeric",
    datastore = "te_datastore",
    censor_at_switch = "logical",
    first_period = "integer",
    last_period = "integer"
  )
)

setGeneric("save_expanded_data", function(object, data) standardGeneric("save_expanded_data"))

setMethod(
  f = "save_expanded_data",
  signature = "te_datastore_csv",
  definition = function(object, data) {
    data_dir <- object@path
    assert_directory_exists(data_dir)
    periods <- unique(data[["trial_period"]])
    for (p in periods) {
      file_p <- file.path(data_dir, paste0("trial_", p, ".csv"))
      fwrite(data[trial_period == p, ], file_p, append = TRUE)
    }
    object@N <- object@N + nrow(data)
    object@files <- file.path(data_dir, paste0("trial_", periods, ".csv"))
    if (!ncol(object@template)) object@template <- data[0, ]
    object
  }
)

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
