#' Data Manipulation Function
#'
#' This function takes the data and all the variables and do the extension preprocess and weight calculation
#' @param data_address Address for data read with bigmemory
#' @param data_path Path of the csv file
#' @param keeplist A list contains names of variables used in final model
#' @param treatment Name of the data column for treatment feature Defaults to treatment
#' @param id Name of the data column for id feature Defaults to id
#' @param period Name of the data column for period feature Defaults to period
#' @param outcome Name of the data column for outcome feature Defaults to outcome
#' @param eligible Indicator of whether or not an observation is eligible to be expanded about Defaults to eligible
#' @param outcomeCov_var A list of individual baseline variables used in final model
#' @param cov_switchn List of covariates to be used in logistic model for switching probabilities for numerator model
#' @param model_switchn List of models (functions) to use the covariates from cov_switchn
#' @param class_switchn Class variables used in logistic model for nominator model
#' @param cov_switchd List of covariates to be used in logistic model for switching probabilities for denominator model
#' @param model_switchd List of models (functions) to use the covariates from cov_switchd
#' @param class_switchd Class variables used in logistic model for denominator model
#' @param first_period First period value to start expanding about
#' @param last_period Last period value to expand about
#' @param use_weight Use weights in analysis. If 0 then no weights will be calculated
#' @param use_censor Use censoring for per-protocol analysis - censor person-times once a person-trial stops taking the initial treatment value
#' @param check_missing Check for missing values in final model when use_censor=1 (Not added yet!)
#' @param cense Censoring variable
#' @param pool_cense Pool the numerator and denominator models (0: split models by previous treatment Am1 = 0 and Am1 = 1 as in treatment models and 1: pool all observations together into a single numerator and denominator model) Defaults to 0
#' @param cov_censed List of covariates to be used in logistic model for censoring weights in denominator model
#' @param model_censed List of models (functions) to use the covariates from cov_censed
#' @param class_censed Class variables used in censoring logistic regression in denominator model
#' @param cov_censen List of covariates to be used in logistic model for censoring weights in numerator model
#' @param model_censen List of models (functions) to use the covariates from cov_censen
#' @param class_censen Class variables used in censoring logistic regression in numerator model
#' @param include_regime_length If defined as 1 a new variable (time_on_regime) is added to dataset - This variable stores the duration of time that the patient has been on the current treatment value
#' @param eligible_wts_0 Eligibility criteria used in weights for model condition Am1 = 0
#' @param eligible_wts_1 Eligibility criteria used in weights for model condition Am1 = 1
#' @param lag_p_nosw when 1 this will set the first weight to be 1 and use p_nosw_d and p_nosw_n at followup-time (t-1) for calculating the weights at followup-time t - can be set to 0 which will increase the maximum and variance of weights (Defaults to 1)
#' @param where_var Variables used in where conditions used in subsetting the data used in final analysis (where_case), the variables not included in the final model
#' @param data_dir Direction to save data
#' @param numCores Number of cores to be used for fitting weights (passed to `weight_func`)
#' data_manipulation()

data_manipulation <- function(data_address, data_path, keeplist,
                              treatment="treatment", id="id",
                              period="period", outcome="outcome",
                              eligible="eligible", outcomeCov_var=NA,
                              cov_switchn=NA, model_switchn=NA,
                              class_switchn=NA, cov_switchd=NA,
                              model_switchd=NA, class_switchd=NA,
                              first_period=NA, last_period=NA,
                              use_weight=0, use_censor=0, check_missing=0,
                              cense=NA, pool_cense=0, cov_censed=NA,
                              model_censed=NA, class_censed=NA,
                              cov_censen=NA, model_censen=NA, class_censen=NA,
                              include_regime_length=0,
                              eligible_wts_0=NA, eligible_wts_1=NA,
                              lag_p_nosw=1, where_var=NA,
                              data_dir="~/rds/hpc-work/",
                              numCores=NA){

  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  time_of_event <- am_1 <- cumA <- regime_start <- time_on_regime <- time_on_regime2 <-
    regime_start_shift <- started0 <- started1 <- stop0 <- stop1 <- eligible0_sw <-
    eligible1_sw <- delete <- eligible0 <- eligible1 <- wt <- NULL


  datatable = read_data(data_address, data_path, NA, id,
                        period, treatment, outcome, eligible,
                        eligible_wts_0, eligible_wts_1,
                        outcomeCov_var,
                        cov_switchn, cov_switchd,
                        cov_censed, cov_censen, cense, where_var)
  len = nrow(datatable)
  len_id = length(unique(datatable[, id]))

  temp_data <- copy(datatable)
  temp_data <- datatable[, .SD[.N], by=id]
  temp_data[, time_of_event := 9999]
  temp_data[(!is.na(outcome) & outcome == 1),
            time_of_event := as.double(period)]
  temp_data = temp_data[, list(id, time_of_event)]
  datatable = datatable[temp_data, on="id"]

  temp_data = datatable[, first:=!duplicated(datatable[, id])]
  temp_data = temp_data[, am_1 := c(NA, treatment[-.N])]
  temp_data[first == TRUE, cumA := 0]
  temp_data[first == TRUE, am_1 := 0]
  temp_data[first == TRUE, switch := 0]
  temp_data[first == TRUE, regime_start := period]
  temp_data[first == TRUE, time_on_regime := 0]
  temp_data[first == TRUE, time_on_regime2 := 0]

  temp_data[(first == FALSE & am_1 != treatment), switch := 1 ]
  temp_data[(first == FALSE & am_1 == treatment), switch := 0 ]

  temp_data[(first == FALSE & switch == 1), regime_start := period]
  temp_data[, regime_start_shift := regime_start[1], list(cumsum(!is.na(regime_start)))]
  temp_data[(first == FALSE & switch == 0), regime_start := regime_start_shift]

  temp_data[, regime_start_shift := shift(regime_start)]
  temp_data[first == FALSE, time_on_regime := period -
              as.double(regime_start_shift)]
  temp_data[first == FALSE, time_on_regime2 := time_on_regime ** 2]

  temp_data[first == TRUE, cumA := cumA + treatment]
  temp_data[first == FALSE, cumA := treatment]
  temp_data[, cumA := cumsum(cumA), by=id]
  temp_data[, regime_start_shift := NULL]

  datatable = copy(temp_data)

  sw_data <- copy(datatable)
  if(use_censor == 1){
    sw_data[, started0 := as.numeric(NA)]
    sw_data[, started1 := as.numeric(NA)]
    sw_data[, stop0 := as.numeric(NA)]
    sw_data[, stop1 := as.numeric(NA)]
    sw_data[, eligible0_sw := as.numeric(NA)]
    sw_data[, eligible1_sw := as.numeric(NA)]
    sw_data[, delete := NA]
    sw_data = censor_func(sw_data)
    sw_data = sw_data[delete == FALSE]
  }

  sw_data[, eligible0 := 0]
  sw_data[, eligible1 := 0]
  sw_data[am_1 == 0, eligible0 := 1]
  sw_data[am_1 == 1, eligible1 := 1]

  # for weight = 1
  if(use_weight == 1){
    sw_data = weight_func(sw_data, cov_switchn, model_switchn, class_switchn,
                          cov_switchd, model_switchd, class_switchd,
                          eligible_wts_0, eligible_wts_1,
                          cense, pool_cense, cov_censed,
                          model_censed, class_censed, cov_censen,
                          model_censen, class_censen, include_regime_length,
                          numCores)
  }else if(use_weight == 0){
    sw_data[, wt := 1]
  }

  fwrite(sw_data, file.path(data_dir, "sw_data.csv"))
  rm(datatable, temp_data, sw_data)
  gc()
}

#' Data Extension in Parallel Function
#'
#' This function takes the data and all the variables and expand it using parallel computing
#' @param data_address Address for data read with bigmemory
#' @param keeplist A list contains names of variables used in final model
#' @param outcomeCov_var A list of individual baseline variables used in final model
#' @param first_period First period value to start expanding about
#' @param last_period Last period value to expand about
#' @param use_censor Use censoring for per-protocol analysis - censor person-times once a person-trial stops taking the initial treatment value
#' @param lag_p_nosw when 1 this will set the first weight to be 1 and use p_nosw_d and p_nosw_n at followup-time (t-1) for calculating the weights at followup-time t - can be set to 0 which will increase the maximum and variance of weights (Defaults to 1)
#' @param where_var Variables used in where conditions used in subsetting the data used in final analysis (where_case), the variables not included in the final model
#' @param data_dir Direction to save data
#' @param numCores Number of cores for parallel programming
#' @param chunk_size Number of ids to expand in each chunk
#' @param separate_files Write to one file or one per trial (default FALSE)
#' data_extension_parallel()

data_extension_parallel <- function(data_address, keeplist, outcomeCov_var=NA,
                           first_period=NA, last_period=NA,
                           use_censor=0,
                           lag_p_nosw=1, where_var=NA,
                           data_dir="~/rds/hpc-work/",
                           numCores=NA,
                           chunk_size=200,
                           separate_files=FALSE){
  max_id = max(data_address[, "id"])
  maxperiod = max(data_address[, "period"])
  minperiod = min(data_address[, "period"])

  if(is.na(first_period)){
    first_period = minperiod
  }
  if(is.na(last_period)){
    last_period = maxperiod
  }
  range = (maxperiod - minperiod) + 1

  all_ids <- unique(data_address[, "id"])
  j <- split(all_ids, ceiling(seq_along(all_ids)/chunk_size))

  N <- mclapply(j, expand_switch, data_address=data_address,
                outcomeCov_var=outcomeCov_var, where_var=where_var,
                use_censor=use_censor, maxperiod=maxperiod, minperiod=minperiod,
                lag_p_nosw=lag_p_nosw, keeplist=keeplist, data_dir=data_dir,
                mc.cores=numCores, separate_files=separate_files)


  gc()

  return(list(min_period = minperiod,
              max_period = maxperiod,
              range =  range,
              N = sum(unlist(as.numeric(N)))))
}

#' Data Extension Function
#'
#' This function takes the data and all the variables and expand it
#' @param data_path data path
#' @param keeplist A list contains names of variables used in final model
#' @param outcomeCov_var A list of individual baseline variables used in final model
#' @param first_period First period value to start expanding about
#' @param last_period Last period value to expand about
#' @param use_censor Use censoring for per-protocol analysis - censor person-times once a person-trial stops taking the initial treatment value
#' @param lag_p_nosw when 1 this will set the first weight to be 1 and use p_nosw_d and p_nosw_n at followup-time (t-1) for calculating the weights at followup-time t - can be set to 0 which will increase the maximum and variance of weights (Defaults to 1)
#' @param where_var Variables used in where conditions used in subsetting the data used in final analysis (where_case), the variables not included in the final model
#' @param data_dir Direction to save data
#' @param separate_files Write to one file or one per trial (default FALSE)
#' data_extension()

data_extension <- function(data_path, keeplist, outcomeCov_var=NA,
                           first_period=NA, last_period=NA,
                           use_censor=0,
                           lag_p_nosw=1, where_var=NA,
                           data_dir="~/rds/hpc-work/", separate_files=FALSE){

  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  id <- period <- NULL

  sw_data = fread(data_path, header = TRUE, sep = ",")
  max_id = max(sw_data[, id])
  maxperiod = max(sw_data[, period])
  minperiod = min(sw_data[, period])

  if(is.na(first_period)){
    first_period = minperiod
  }
  if(is.na(last_period)){
    last_period = maxperiod
  }
  range = (maxperiod - minperiod) + 1

  N <- expand(sw_data, outcomeCov_var, where_var, use_censor, maxperiod, minperiod,
         lag_p_nosw, keeplist, data_dir, separate_files)

  rm(sw_data)
  gc()
  return(list(min_period = minperiod,
              max_period = maxperiod,
              range =  range,
              N = N))
}
