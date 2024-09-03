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
    catn("Sequence of Trials Data:")
    catn("- Chunk size:", object@chunk_size)
    catn("- Censor at switch:", object@censor_at_switch)
    catn("- First period:", object@first_period, "| Last period:", object@last_period)
    if (object@datastore@N > 0) {
      catn("")
      show(object@datastore)
    } else {
      catn("- Use expand_trials() to construct sequence of trials dataset.")
    }
  }
)


# Dummy class for unset te_expansion
setClass("te_expansion_unset", contains = "te_expansion")

# Show
setMethod(
  "show",
  c(object = "te_expansion_unset"),
  function(object) {
    catn("Sequence of Trials Data:")
    catn("- Use set_expansion_options() and expand_trials() to construct the sequence of trials dataset.")
  }
)
