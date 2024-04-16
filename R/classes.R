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
    files = "character"
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
  new("te_datastore_csv", path = path)
}

save_to_data.table <- function(...) {
  new("te_datastore")
}
save_to_duckdb <- function(...) {
  new("te_datastore")
}



setClass("te_expansion",
  slots = c(
    chunks = "numeric",
    datastore = "te_datastore",
    censor_at_switch = "logical"
  )
)
