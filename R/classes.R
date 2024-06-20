#' @include te_datastore.R

setClass(
  "te_expansion",
  slots = c(
    chunk_size = "numeric",
    datastore = "te_datastore",
    censor_at_switch = "logical",
    first_period = "numeric",
    last_period = "numeric"
  )
)

# Dummy class for unset te_expansion
setClass("te_expansion_unset", contains = "te_expansion")
