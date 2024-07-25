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

# Show
setMethod(
  "show",
  c(object = "te_expansion"),
  function(object) {
    catn("Expansion:")
    catn("Chunk size:", object@chunk_size)
    catn("Censor at switch:", object@censor_at_switch)
    catn("First period:", object@first_period, "| Last period:", object@last_period)
    catn("")
    show(object@datastore)
  }
)


# Dummy class for unset te_expansion
setClass("te_expansion_unset", contains = "te_expansion")

# Show
setMethod(
  "show",
  c(object = "te_expansion_unset"),
  function(object) {
    catn("No expanded data, use expand_trials()")
  }
)
