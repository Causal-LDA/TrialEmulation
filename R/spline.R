#' Add splines to expanded data
#'
#' Add natural spline basis columns to the expanded data for period and follow up time.
#'
#' @param data_path Path to expanded data csv file which has columns `for_period` and `followup_time`.
#' @param out_path Path to write data csv with added splines. Defaults to overwriting data_path.
#' @param period_spline Named list of arguments for `ns` for periods spline (column `for_period`)
#' @param followup_spline Named list of arguments for `ns` for follow up time (column `follow_time`)
#'
#' @return Returns a list containing the the first rows of the `ns` objects.
#' Writes the data with the spline basis columns to `out_path`.
#' @export
#' @import assertthat
#'
add_splines <- function(data_path, out_path = data_path, period_spline, followup_spline){

  assert_that(is.readable(data_path))
  assert_that(is.string(out_path))

  attempt_period <- attempt_followup <- FALSE

  attempt_period <- if (!missing(period_spline)){
    if (any(!is.na(period_spline))){
      assert_that(is.list(period_spline), msg = "period_spline is not a list of arguments for ns()")
      TRUE
    }
  }

  attempt_followup <- if (!missing(followup_spline)){
    if (any(!is.na(followup_spline))){
      assert_that(is.list(followup_spline), msg = "followup_spline is not a list of arguments for ns()")
      TRUE
    }
  }

  # Check columns exist in data
  data_head <- fread(data_path, header=TRUE, sep = ",", nrows = 10)

  if(isTRUE(attempt_period)) {
    assert_that("for_period" %in% colnames(data_head),
                msg = "Spline specified for period but data does not have for_period column.")
  }

  if(isTRUE(attempt_followup)) {
    assert_that("followup_time" %in% colnames(data_head),
                msg = "Spline specified for follow-up time but data does not have followup_time column.")
  }
  rm(data_head)

  return_splines <- list(for_period = NA, followup_time = NA)

  # Create splines for the followup_time and for_period as required.
    if(isTRUE(attempt_period) | isTRUE(attempt_followup)){

    data <- fread(data_path, header = TRUE, sep = ",")
    if(isTRUE(attempt_period)){
      temp <- do.call("ns", c(x = list(data[["for_period"]]), period_spline))
      for(i in 1:ncol(temp)){
        data[, paste0("period_base_", i)] <- temp[, i]
      }
      return_splines$for_period <- temp[1:10, ]
      mostattributes(return_splines$for_period) <- attributes(temp)
    }

    if(isTRUE(attempt_followup)){
      temp <- do.call("ns", c(x = list(data[["followup_time"]]), followup_spline))
      for(i in 1:ncol(temp)){
        data[, paste0("followup_base_", i)] <- temp[, i]
      }
      return_splines$followup_time <- temp[1:10, ]
      mostattributes(return_splines$followup_time) <- attributes(temp)
    }
    fwrite(data, file = out_path, row.names = FALSE)

    return_splines
  }
}



#' Add splines to expanded data in memory
#'
#' Add natural spline basis columns to the expanded data for period and follow up time.
#'
#' @param data A `data.table` containing the expanded data.
#' @param period_spline Named list of arguments for `ns` for periods spline (column `for_period`)
#' @param followup_spline Named list of arguments for `ns` for follow up time (column `follow_time`)
#'
#' @return Returns a data.table with the splines columns added.
#' @export
#' @import assertthat
#'
add_splines_df <- function(data, period_spline, followup_spline){

  data <- as.data.table(data)
  assert_that(is.data.table(data))

  attempt_period <- attempt_followup <- FALSE

  attempt_period <- if (!missing(period_spline)){
    if (any(!is.na(period_spline))){
      assert_that(is.list(period_spline), msg = "period_spline is not a list of arguments for ns()")
      TRUE
    }
  }

  attempt_followup <- if (!missing(followup_spline)){
    if (any(!is.na(followup_spline))){
      assert_that(is.list(followup_spline), msg = "followup_spline is not a list of arguments for ns()")
      TRUE
    }
  }

  # Check columns exist in data
  if(isTRUE(attempt_period)) {
    assert_that("for_period" %in% colnames(data),
                msg = "Spline specified for period but data does not have for_period column.")
  }

  if(isTRUE(attempt_followup)) {
    assert_that("followup_time" %in% colnames(data),
                msg = "Spline specified for follow-up time but data does not have followup_time column.")
  }

  # Create splines for the followup_time and for_period as required.
  if(isTRUE(attempt_period) | isTRUE(attempt_followup)){

    if(isTRUE(attempt_period)){
      temp <- do.call("ns", c(x = list(data[["for_period"]]), period_spline))
      for(i in 1:ncol(temp)){
        data[, paste0("period_base_", i)] <- temp[, i]
      }
    }

    if(isTRUE(attempt_followup)){
      temp <- do.call("ns", c(x = list(data[["followup_time"]]), followup_spline))
      for(i in 1:ncol(temp)){
        data[, paste0("followup_base_", i)] <- temp[, i]
      }
    }
  }
  data
}
