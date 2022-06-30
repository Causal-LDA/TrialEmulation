#' P99 Weights Function
#'
#' This function truncate the weights of data at the 1st and 99th percentile
#' @param switch_data The data.table with weight column

p99_weight <- function(switch_data){
  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  weight <- NULL

  p99 = quantile(switch_data[, weight],
                 prob=c(0.99, 0.1),
                 type = 1)

  len = nrow(switch_data)
  switch_data[weight > p99[1], weight := p99[1]]
  switch_data[weight < p99[2], weight := p99[2]]
  return(switch_data)
}

#' Limit Weights Function
#'
#' This function truncate the weights using user defined limits
#' @param switch_data The data.table contains weight column
#' @param lower_limit The user defined minimum possible weight
#' @param upper_limit The user defined maximum possible weight

limit_weight <- function(switch_data, lower_limit, upper_limit){

  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  weight <- NULL

  len = nrow(switch_data)
  switch_data[weight > upper_limit, weight := upper_limit]
  switch_data[weight < lower_limit, weight := lower_limit]
  return(switch_data)
}

#' Weight Logistic Regression Function
#'
#' This function get the information needed for performing Logistic Regression in the weight calculation process using parglm
#'
#' @param data data
#' @param formula model formula for `parglm`
#' @param class_var categorical variables to be converted to factors

weight_lr <- function(data, formula, class_var){

  if(!missing(class_var) & any(!is.na(class_var))){
    class_var <- class_var[!is.na(class_var)]

    if(!(is.list(class_var) | is.character(class_var))) stop("outcomeClass is not a list or character vector")
    #class_var given as a character vector, convert to list
    if(is.character(class_var)) class_Var <- as.list(class_var)

    #process the list
    for(i in seq_along(class_var)){
      # variable name provided only
      if(is.character(class_var[[i]])) {
        this_var <- class_var[[i]]
        set(data, j = this_var, value = as.factor(data[[this_var]]))
      }

      # named list provided
      if(is.list(class_var[[i]])){
        this_var <- names(class_var[i])
        set(data, j = this_var,
            value = do.call("factor", c(x = list(data[[this_var]]), class_var[[this_var]])))
      }
    }
  }

  model = parglm::parglm(as.formula(formula), data=data,
                         family = binomial(link = "logit"),
                         control = parglm::parglm.control(nthreads = 4, method='FAST'))
  return(model)
}



#' Logistic Regression Function
#'
#' This function get the information needed for performing Logistic Regression for final model
#' @param l A list contains the data with categorical feature determind if needed and logistic regression formula

lr <- function(l){
  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  weight <- id <- NULL

  d = l[[1]]
  regf = l[[2]]

  model = parglm::parglm(as.formula(regf), data=d,
                 weights=d[, weight],
                 family = binomial(link = "logit"),
                 control = parglm::parglm.control(nthreads = 4, method='FAST'))

  out = robust_calculation(model, d[, id])

  return(list(model=model,
              output=out))
}


#' Expand Function
#'
#' This function performs the data expansion for a given dataset
#' @param sw_data datatable to expand
#' @param outcomeCov_var A list of individual baseline variables used in final model
#' @param where_var Variables used in where conditions used in subsetting the data used in final analysis (where_case), the variables not included in the final model
#' @param use_censor Use censoring for per-protocol analysis - censor person-times once a person-trial stops taking the initial treatment value
#' @param followup_spline The parameters for spline model for followup time when choose "spline" in the include_followup_time_case
#' @param period_spline The parameters for spline model for for_period when choose "spline" in the include_expansion_time_case
#' @param maxperiod Maximum period
#' @param minperiod Minimum period
#' @param lag_p_nosw when 1 this will set the first weight to be 1 and use p_nosw_d and p_nosw_n at followup-time (t-1) for calculating the weights at followup-time t - can be set to 0 which will increase the maximum and variance of weights (Defaults to 1)
#' @param keeplist A list contains names of variables used in final model
#' @param data_dir Direction to save data
#' @param separate_files Write to one file or one per trial (default FALSE)
#' @importFrom splines ns
#' @import data.table

# CCS: Have changed return type of this function
expand <- function(sw_data,
                   outcomeCov_var, where_var, use_censor,
                   followup_spline=NA,
                   period_spline=NA,
                   maxperiod, minperiod,
                   lag_p_nosw, keeplist, data_dir, separate_files=FALSE){

  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  id <- period <- wtprod <- elgcount <- treat <- dosesum <- eligible <- treatment <- treatment_new <-
    weight0 <- wt <- cumA <- init <- init_shift <- period_new <- cumA_new <- switch_new <-
    outcome_new <- outcome <- time_of_event <- for_period <- index <- for_period2 <-
    followup_time <- followup_time2 <- dose <- dose2 <- weight <- case <- NULL

  temp_data = data.table(id = sw_data[, id],
                         period = sw_data[, period],
                         switch = sw_data[, switch])
  temp_data[, wtprod := 1.0, by=id][, elgcount := 0.0, by=id][, expand := 0.0, by=id]
  temp_data[, treat := 0.0, by=id][, dosesum := 0.0, by=id]
  temp_data[(sw_data[, eligible] == 1 & !is.na(sw_data[, treatment])),
            expand := 1, by=id]
  sw_data[first == TRUE, weight0 := 1.0]
  sw_data[, weight0 := cumprod(wt), by=id]
  temp_data[, wtprod := sw_data[, weight0]]
  temp_data[, treat := sw_data[, treatment]]
  temp_data[, dosesum := sw_data[, cumA]]
  temp_data[, elgcount := sw_data[, eligible]]
  temp_data[sw_data[, eligible] == 1, init := sw_data[eligible == 1, treatment]]
  temp_data[, init_shift := shift(sw_data[, treatment])]
  temp_data[sw_data[, eligible] == 0, init := init_shift, by=id]
  temp_data[, init_shift := NULL]
  if(any(!is.na(outcomeCov_var))){
    tryCatch({
      suppressWarnings(temp_data[, eval(outcomeCov_var) := sw_data[, outcomeCov_var, with=FALSE]])
    })
  }
  if(any(!is.na(where_var))){
    temp_data[, eval(where_var) := sw_data[, where_var, with=FALSE]]
  }

  switch_data = data.table(id = sw_data[, id])
  switch_data = switch_data[rep(1:.N, sw_data[, period]+1)]
  switch_data[, period_new := sw_data[rep(1:.N, period+1), period]]
  switch_data[, cumA_new := sw_data[rep(1:.N, period+1), cumA]]
  switch_data[, treatment_new := shift(sw_data[rep(1:.N, period+1), treatment])]
  switch_data[1, "treatment_new"] = sw_data[1, treatment]
  if(use_censor == 1){
    switch_data[, switch_new := sw_data[rep(1:.N, period+1), switch]]
  }else{
    switch_data[, switch_new := 0]
  }
  switch_data[, outcome_new := sw_data[rep(1:.N, period+1), outcome]]
  switch_data[, time_of_event := sw_data[rep(1:.N, period+1), time_of_event]]
  switch_data[, weight0 := sw_data[rep(1:.N, period+1), weight0]]
  switch_data[, for_period := for_period_func(sw_data)]
  switch_data[, index := 1:nrow(switch_data)]
  switch_data = switch_data[temp_data, on = list(id=id, for_period=period)]
  setorder(switch_data, index)
  if("for_period2" %in% keeplist){
    switch_data[, for_period2 := for_period ** 2]
  }
  # if(any(!is.na(period_spline))){
  #   if(knots %in% period_spline){
  #     for_period = switch_data[, for_period]
  #     temp = do.call("ns", c(x = list(for_period), period_spline))
  #     for(i in 1:ncol(temp)){
  #       switch_data[, paste0("period_base_", i)] = temp[, i]
  #     }
  #   }else{
  #
  #   }
  # }
  switch_data[, followup_time := period_new - for_period]
  if("followup_time2" %in% keeplist){
    switch_data[, followup_time2 := followup_time **2]
  }
  # if(!is.na(followup_spline)){
  #   if(knots %in% followup_spline){
  #     followup_time = switch_data[, followup_time]
  #     temp = do.call("ns", c(x = list(followup_time), followup_spline))
  #     for(i in 1:ncol(temp)){
  #       switch_data[, paste0("followup_base_", i)] = temp[, i]
  #     }
  #   }
  # }
  if(use_censor == 0){
    switch_data[, dose := cumA_new - dosesum + treat]
    switch_data[, dose2 := dose ** 2]
  }else{
    switch_data[, treatment := init]
  }
  switch_data[expand == 1, expand := expand_func(.SD, maxperiod, minperiod), by=id]

  if(lag_p_nosw == 1){
    switch_data[, weight := (weight0/wtprod)]
  }else{
    switch_data[ for_period == minperiod, weight := weight0]
    wtprod_shift = shift(switch_data[, wtprod])
    switch_data[ for_period != 0, weight := (weight0/wtprod_shift)]
  }
  switch_data[, case := 0]
  if(use_censor == 0){
    switch_data[(time_of_event == period_new & outcome_new == 1), case := 1]
  }else{
    switch_data[switch_new == 1, case := as.numeric(NA)]
    switch_data[(switch_new == 0 &
                   time_of_event == period_new & outcome_new == 1), case := 1]
  }

  setnames(switch_data, c("case"), c("outcome"))
  setnames(switch_data, c("init"), c("assigned_treatment"))
  setnames(switch_data, c("treatment_new"), c("treatment"))
  switch_data = switch_data[expand == 1]
  switch_data = switch_data[, keeplist, with=FALSE]

  # CCS: This writes switch_data. Instead, return the switch_data object
  #if(!separate_files){
  #  fwrite(switch_data, file.path(data_dir, "switch_data.csv"), append=TRUE, row.names=FALSE)
  #} else if(separate_files) {
  #  for(p in unique(switch_data[,"for_period"])[[1]]){
  #    fwrite(switch_data[for_period==p,], file.path(data_dir, paste0("trial_",p,".csv")), append=TRUE, row.names=FALSE)
  #  }
  #}

  # CCS
  #N <- nrow(switch_data)

  #rm(temp_data, switch_data)

  #gc()

  #N
  switch_data
}


#' Expand Switch Function
#'
#' This function performs the data expansion for a chunk of data
#' @param id_num Vector of Ids
#' @param data_address Address for data read with bigmemory
#' @param outcomeCov_var A list of individual baseline variables used in final model
#' @param where_var Variables used in where conditions used in subsetting the data used in final analysis (where_case), the variables not included in the final model
#' @param use_censor Use censoring for per-protocol analysis - censor person-times once a person-trial stops taking the initial treatment value
#' @param followup_spline The spline model for followup time when choose "spline" in the include_followup_time_case
#' @param period_spline The spline model for for_period when choose "spline" in the include_expansion_time_case
#' @param maxperiod Maximum period
#' @param minperiod Minimum period
#' @param lag_p_nosw when 1 this will set the first weight to be 1 and use p_nosw_d and p_nosw_n at followup-time (t-1) for calculating the weights at followup-time t - can be set to 0 which will increase the maximum and variance of weights (Defaults to 1)
#' @param keeplist A list contains names of variables used in final model
#' @param data_dir Direction to save data
#' @param separate_files Write to one file or one per trial (default FALSE)
#' @import data.table

# CCS: This is called from within mclapply. We can ignore for now.
expand_switch <- function(id_num, data_address,
                          outcomeCov_var, where_var,
                          use_censor, followup_spline, period_spline,
                          maxperiod, minperiod, lag_p_nosw,
                          keeplist, data_dir, separate_files=FALSE){

  if(bigmemory::is.big.matrix(data_address)){
    sw_data <- as.data.table(
      data_address[bigmemory::mwhich(data_address,
                                     cols = rep("id",length(id_num)),
                                     vals = as.list(id_num),
                                     comps = c('eq'),
                                     op = "OR"),]
    )
  } else {
    sw_data <- data_address[list(id_num),]
  }

  N <- expand(sw_data, outcomeCov_var, where_var, use_censor,
              followup_spline, period_spline, maxperiod, minperiod,
         lag_p_nosw, keeplist, data_dir, separate_files)
  # CCS
  #rm(sw_data)
  #gc()
  N
}
