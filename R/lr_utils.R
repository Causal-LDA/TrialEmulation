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
#' @param l A list contains the data, logistic regression formula and name of the categorical variables

weight_lr <- function(l){
  d = l[[1]]
  regf = l[[2]]
  class_var = l[[3]]

  if(any(!is.na(class_var))){
    for(i in 1:length(class_var)){
      x = factor(d[[eval(class_var[i])]])
      x = relevel(x, ref="1")
      d[, c(eval(class_var[i])) := NULL]
      d[, eval(class_var[i]) := x]
    }
  }

  model = parglm::parglm(as.formula(regf), data=d,
                         family = binomial(link = "logit"),
                         control = parglm::parglm.control(nthreads = 4, method='FAST'))
  return(model)
}

#' Case Control Sampling Function
#'
#' This function apply case control sampling on the extended data
#' @param period_num Period id
#' @param data_address A data.table which is the extended version of input data
#' @param n_control Number of controls in the case control sampling Defaults to 5
#' @param numCores Number of cores for parallel programming

case_control_func <- function(period_num, data_address, n_control=5,
                              data_dir="~/rds/hpc-work/",
                              numCores=NA){
  case_util <- function(data, n_control=5){
    ### cases occurred at each period and follow-up visit
    casedatajk<-data[data$outcome==1, ]
    ### controls (still under follow-up and events haven't occurred) at each period and follow-up visit
    controldatajk<-data[data$outcome==0, ]
    dataall = NULL
    ncase<-dim(casedatajk)[1]  ## number of cases
    ncontrol<-dim(controldatajk)[1]  ## number of potential controls
    if(ncase>0){
      #cluster = 1  ## sampling cluster index
      controlselect<-controldatajk[sample(1:ncontrol, n_control*ncase),]  ## sample 5 controls for each case without replacement
      #casedatajk$strata<-cluster*100000+1:ncase    ## create index for each sampled case-control strata
      #controlselect$strata<-cluster*100000+rep(1:ncase,each=5) ## create index for each sampled case-control strata
      dataall<-rbind(casedatajk, controlselect) ## append sampled data
    }
    return(dataall)
  }
  d_period = data_address[bigmemory::mwhich(data_address, c("for_period"), c(period_num), c('eq')),]
  d_period = as.data.table(d_period)
  d = split(d_period, d_period$followup_time)

  if(numCores == 1) {
    # cl <- makeCluster(numCores)
    # clusterExport(cl, "n_control", envir=environment())
    # m = parLapply(cl, d, case_util, n_control)
    # stopCluster(cl)
    m = lapply(d, case_util, n_control)
  } else {
    m = mclapply(d, case_util, n_control=n_control,
                 mc.cores=numCores)
  }

  new_d = rbindlist(m)
  fwrite(new_d, paste0(data_dir, "temp_data.csv"), append=TRUE, row.names=FALSE)
  rm(d_period, d, m, new_d)
  gc()
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

#' Robust Calculation Funstion
#'
#' This function performs the calculation with Robust Standard Errors
#' @param model The Logistic Regression model
#' @param data_id Values of column id of the data (data[, id])

robust_calculation <- function(model, data_id){
  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  lb <- estimate <- robust_se <- ub <- z <- p_value <- name <- std <- NULL

  est_temp = model$coefficients
  v = sandwich::vcovCL(model, cluster = data_id,
             type = NULL, sandwich = TRUE, fix = FALSE)
  #v = cluster.vcov(model.full, cluster=temp_data[, id], parallel=4)
  se = sqrt(diag(v))
  print("-------------------------------------------------------")
  print("Robust standard error:")
  print(se)
  avg = rbind(est_temp, se)
  output = t(avg)
  output = data.table(output)
  names(output)[names(output) == "est_temp"] <- "estimate"
  names(output)[names(output) == "se"] <- "std"
  output[, name := names(model$coefficients)]

  output[, lb := estimate - (1.96 * std)]
  output[, ub := estimate + (1.96 * std)]
  output[, z := estimate/std]
  output[, p_value := format.pval(2*(1-pnorm(abs(z))), eps=0.001)]
  return(output)
}

#' Expand Function
#'
#' This function performs the data expansion for a given dataset
#' @param sw_data datatable to expand
#' @param outcomeCov_var A list of individual baseline variables used in final model
#' @param where_var Variables used in where conditions used in subsetting the data used in final analysis (where_case), the variables not included in the final model
#' @param use_censor Use censoring for per-protocol analysis - censor person-times once a person-trial stops taking the initial treatment value
#' @param maxperiod Maximum period
#' @param minperiod Minimum period
#' @param lag_p_nosw when 1 this will set the first weight to be 1 and use p_nosw_d and p_nosw_n at followup-time (t-1) for calculating the weights at followup-time t - can be set to 0 which will increase the maximum and variance of weights (Defaults to 1)
#' @param keeplist A list contains names of variables used in final model
#' @param data_dir Direction to save data
#' @import data.table

expand <- function(sw_data,
                   outcomeCov_var, where_var,
                   use_censor, maxperiod, minperiod,
                   lag_p_nosw, keeplist, data_dir){

  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  id <- period <- wtprod <- elgcount <- treat <- dosesum <- eligible <- treatment <-
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
  switch_data[, for_period2 := for_period ** 2]
  switch_data[, followup_time := period_new - for_period]
  switch_data[, followup_time2 := followup_time **2]
  if(use_censor == 0){
    switch_data[, dose := cumA_new - dosesum + treat]
    switch_data[, dose2 := dose ** 2]
  }else{
    switch_data[, treatment := init]
  }

  switch_data[expand == 1, expand := expand_func(.SD, (maxperiod-minperiod)+1, minperiod), by=id]

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
  switch_data = switch_data[expand == 1]
  switch_data = switch_data[, keeplist, with=FALSE]
  fwrite(switch_data, paste0(data_dir, "switch_data.csv"), append=TRUE, row.names=FALSE)
  rm(temp_data, switch_data)
  gc()
}


#' Expand Switch Function
#'
#' This function performs the data expansion for a chunk of data
#' @param id_num Id number
#' @param data_address Address for data read with bigmemory
#' @param outcomeCov_var A list of individual baseline variables used in final model
#' @param where_var Variables used in where conditions used in subsetting the data used in final analysis (where_case), the variables not included in the final model
#' @param use_censor Use censoring for per-protocol analysis - censor person-times once a person-trial stops taking the initial treatment value
#' @param maxperiod Maximum period
#' @param minperiod Minimum period
#' @param lag_p_nosw when 1 this will set the first weight to be 1 and use p_nosw_d and p_nosw_n at followup-time (t-1) for calculating the weights at followup-time t - can be set to 0 which will increase the maximum and variance of weights (Defaults to 1)
#' @param keeplist A list contains names of variables used in final model
#' @param data_dir Direction to save data
#' @import data.table

expand_switch <- function(id_num, data_address,
                          outcomeCov_var, where_var,
                          use_censor, maxperiod, minperiod,
                          lag_p_nosw, keeplist, data_dir){
  d = data_address[bigmemory::mwhich(data_address, c("id"), c(id_num), c('eq')),]
  if(is.null(nrow(d))){
    sw_data = as.data.table(t(d))
  }else{
    sw_data = as.data.table(d)
  }
  expand(sw_data, outcomeCov_var, where_var, use_censor, maxperiod, minperiod,
         lag_p_nosw, keeplist, data_dir)
  rm(sw_data, d)
  gc()
}
