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

# CCS: First param data_path is now switch_data. Return type changed.
# out_path was set to data_path as default
add_splines <- function(switch_data, out_path = NA, period_spline, followup_spline){

  # CCS:
  #assert_that(is.readable(data_path))
  #assert_that(is.string(out_path))

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
  # CCS: Just use full data.table here
  #data_head <- fread(data_path, header=TRUE, sep = ",", nrows = 10)

  if(isTRUE(attempt_period)) {
    assert_that("for_period" %in% colnames(switch_data), #CCS
                msg = "Spline specified for period but data does not have for_period column.")
  }

  if(isTRUE(attempt_followup)) {
    assert_that("followup_time" %in% colnames(switch_data), #CCS
                msg = "Spline specified for follow-up time but data does not have followup_time column.")
  }
  #rm(data_head)

  return_splines <- list(for_period = NA, followup_time = NA)

  # Create splines for the followup_time and for_period as required.
    if(isTRUE(attempt_period) | isTRUE(attempt_followup)){

      # CCS: data renamed switch_data
    #data <- fread(data_path, header = TRUE, sep = ",")
    if(isTRUE(attempt_period)){
      temp <- do.call("ns", c(x = list(switch_data[["for_period"]]), period_spline))
      for(i in 1:ncol(temp)){
        switch_data[, paste0("period_base_", i)] <- temp[, i]
      }
      return_splines$for_period <- temp[1:10, ]
      mostattributes(return_splines$for_period) <- attributes(temp)
    }

    if(isTRUE(attempt_followup)){
      temp <- do.call("ns", c(x = list(switch_data[["followup_time"]]), followup_spline))
      for(i in 1:ncol(temp)){
        switch_data[, paste0("followup_base_", i)] <- temp[, i]
      }
      return_splines$followup_time <- temp[1:10, ]
      mostattributes(return_splines$followup_time) <- attributes(temp)
    }
      # CCS
      #fwrite(data, file = out_path, row.names = FALSE)

      # CCS: The current return value of return_splines is ignored.
      # So don't both returning this. Instead, return switch_data.
      #return_splines
      
    }
  ## CCS
  switch_data
}
