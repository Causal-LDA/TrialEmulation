#' Add splines to expanded data
#'
#' Add natural spline basis columns to the expanded data for period and follow up time.
#'
#' @param data_path Path to expanded data csv file
#' @param period_spline Named list of arguments for `ns` for periods spline
#' @param followup_spline Named list of arguments for `ns` for follow up time
#'
#' @return Overwrites data_path with new csv with spline basis columns.
#' @export
#'
add_splines <- function(data_path, period_spline, followup_spline){

  if(!file.exists(data_path)) stop(paste0("Specified file does not exist: ", data_path))

  data <- fread(data_path, header = TRUE, sep = ",")

  if(any(!is.na(period_spline))){
    temp <- do.call("ns", c(x = list(data[, for_period]),
                           period_spline))
    for(i in 1:ncol(temp)){
      data[, paste0("period_base_", i)] <- temp[, i]
    }
  }
  if(any(!is.na(followup_spline))){
    temp <- do.call("ns", c(x = list(data[, followup_time]),
                           followup_spline))
    for(i in 1:ncol(temp)){
      data[, paste0("followup_base_", i)] <- temp[, i]
    }
  }

  fwrite(data, data_path, row.names=FALSE)
}
