#' Add splines to expanded data
#'
#' Add natural spline basis columns to the expanded data for period and follow up time.
#'
#' @param data_path Path to expanded data csv file which has columns `for_period` and `followup_time`.
#' @param period_spline Named list of arguments for `ns` for periods spline (column `for_period`)
#' @param followup_spline Named list of arguments for `ns` for follow up time (column `follow_time`)
#'
#' @return Overwrites data_path with new csv with spline basis columns.
#' @export
#'
add_splines <- function(data_path, period_spline, followup_spline){

  assert_that(is.readable(data_path))

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

  # Process data if we need to
  if(isTRUE(attempt_period) | isTRUE(attempt_followup)){
    data <- fread(data_path, header = TRUE, sep = ",")

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
    fwrite(data, data_path, row.names=FALSE)
  }
}
