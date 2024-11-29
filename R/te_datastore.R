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


# save to data.table -------
setClass(
  "te_datastore_datatable",
  contains = "te_datastore",
  slots = c(
    data = "data.table"
  )
)

# Show
setMethod(
  "show",
  c(object = "te_datastore_datatable"),
  function(object) {
    catn("A TE Datastore Datatable object")
    catn("N:", object@N, "observations")
    print(object@data, nrows = 4, topn = 2)
  }
)


#' Save expanded data as a `data.table`
#'
#' `r lifecycle::badge('experimental')`
#' @family save_to
#' @export
#' @examples
#' trial_to_expand <- trial_sequence("ITT") |>
#'   set_data(data = data_censored) |>
#'   set_expansion_options(output = save_to_datatable(), chunk_size = 500)
save_to_datatable <- function() {
  new("te_datastore_datatable", data = data.table(), N = 0L)
}


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


#' @rdname read_expanded_data
setMethod(
  f = "read_expanded_data",
  signature = "te_datastore_datatable",
  definition = function(object, period, subset_condition) {
    trial_period <- NULL
    checkmate::assert_integerish(period, null.ok = TRUE, any.missing = FALSE, lower = 0)

    data_table <- if (is.null(period)) {
      object@data
    } else {
      object@data[trial_period %in% period, ]
    }

    if (!is.null(subset_condition)) {
      subset_expr <- str2lang(subset_condition)
      data_table <- data_table[eval(subset_expr)]
    }
    data_table
  }
)
