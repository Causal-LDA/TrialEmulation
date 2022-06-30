#' Data modelling Function
#'
#' This function do the modelling.
#' @param outcomeCov_var List of individual baseline variables used in final model
#' @param outcomeCov List of functions of baseline covariates used in final model
#' @param outcomeClass Categorical variables used in the final model
#' @param model_var List of Variables of interest to be used in final model
#' @param first_followup First follow-up period
#' @param last_followup Last follow-up period
#' @param use_weight Use weights in analysis. If 0 then no weights will be calculated
#' @param run_unweighted_analysis Run the final model with no weights when use_weights = 1
#' @param run_weighted_analysis Run the final model with original weights
#' @param run_p99_analysis Run the final model with truncating the weights at the 1st and 99th percentile
#' @param run_user_limits_analysis Run the final model with truncating the weights using user defined limits
#' @param lower_weight Use lower weight as minimum possible weight
#' @param upper_weight Use upper weight as maximum possible weight
#' @param use_censor Use censoring for per-protocol analysis - censor person-times once a person-trial stops taking the initial treatment value
#' @param check_missing Check for missing values in final model when use_censor=1 (Not added yet!)
#' @param include_followup_time_case The model to include follow up time in outcome model. This has 3 options c("none","linear","quadratic","spline"). The data should contain the corresponding columns: "followup_time","followup_time2" or "followup_base".
#' @param include_expansion_time_case The model to include for_period in outcome model. This has 3 options c("none","linear","quadratic","spline").  The data should contain the corresponding columns: "for_period","for_period2" or "period_base".
#' @param where_case List of where conditions used in subsetting the data used in final analysis
#' @param run_base_model Run the model with no conditions Defaults to 1
#' @param absolutePath Direction to where the data for modelling is saved
#' @param numCores Number of cores for parallel programming (default value is maximum cores and parallel programming)
#' @param glm_function Which glm function to use for the final model from `stats` or `parglm` packages
#' @param use_sample_weights Set model weights to `sample_weight * weight`.
#' @param quiet Don't print progress messages.
#' data_modelling()
#'
#' @details The class variables parameters (`outcomeClass`,`class_switchn`,
#'  `class_switchd`,`class_censen`,`class_censed`) can be given as a character
#'  vector which will construct factors using `as.factor` or as a named list
#'  with the arguments for factor e.g.
#'  `list(risk_cat=list(levels = c(1,2,3,0), age_cat=list(levels=c(1,2,3),labels=c("50-60","60-70","70+")`
#'
#' @export
#' @importFrom splines ns
#' @importFrom stats as.formula binomial pnorm quantile relevel
#' @importFrom utils write.csv

# CCS: Add param switch_data. AbsolutePath is now NA
data_modelling <- function(outcomeCov_var = NA,
                           outcomeCov = NA,
                           outcomeClass = NA,
                           model_var = NA,
                           first_followup = NA,
                           last_followup = NA,
                           use_weight = 0,
                           run_unweighted_analysis = 0,
                           run_weighted_analysis = 1,
                           run_p99_analysis = 0,
                           run_user_limits_analysis = 0,
                           lower_weight = NA,
                           upper_weight = NA,
                           use_censor = 0,
                           check_missing = 0,
                           include_followup_time_case = c("linear","quadratic","spline"),
                           include_expansion_time_case = c("linear","quadratic","spline"),
                           where_case = NA,
                           run_base_model = 1,
                           absolutePath,
                           numCores = NA,
                           glm_function = c('parglm','glm'),
                           use_sample_weights = TRUE,
                           quiet = FALSE,
                           switch_data
) {
  assert_flag(quiet)

  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  weight <- sample_weight <- NULL


  glm_function <- match.arg(glm_function)


  # if there are any limits on the follow up
  if(!is.na(first_followup) | !is.na(last_followup)){
    #make sure that the other is well defined
    if(is.na(first_followup)) first_followup <- 0
    if(is.na(last_followup)) last_followup <- Inf

    # CCS: This branch not currently used by our code
    
    data = tryCatch({
      suppressWarnings(bigmemory::read.big.matrix(absolutePath, header=TRUE, shared=FALSE, type="double"))
    })
    #subset the data
    temp_data = data[bigmemory::mwhich(data, c("followup_time", "followup_time"), c(first_followup, last_followup), c('ge', 'le'), 'AND'), ]
    temp_data = as.data.table(temp_data)
    # CCS
    #rm(data)
    #gc()
  } else {
    #read data directly as data.table
    # CCS: No need for the read
    #temp_data <- fread(absolutePath)
  }

  # CCS: switch_data was read in as temp_data. Perform rename throughout rest of function

  # Process class variables into factors
  if(any(!is.na(outcomeClass))){
    if(!(is.list(outcomeClass) | is.character(outcomeClass))) stop("outcomeClass is not a list or character vector")
    #class_var given as a character vector, convert to list
    if(is.character(outcomeClass)) outcomeClass <- as.list(outcomeClass)

    #process the list
    for(i in seq_along(outcomeClass)){
      # variable name provided only
      if(is.character(outcomeClass[[i]])) {
        this_var <- outcomeClass[[i]]
        set(switch_data, j = this_var, value = as.factor(switch_data[[this_var]]))
      }
      # named list provided
      if(is.list(outcomeClass[[i]])){
        this_var <- names(outcomeClass[i])
        set(switch_data, j = this_var,
            value = do.call("factor", c(x = list(switch_data[[this_var]]), outcomeClass[[this_var]])))
      }
    }
  }


  # adjust weights if necessary
  if (use_sample_weights){
    if (!"sample_weight" %in% colnames(switch_data)) {
      warning("'sample_weight' column not found in data. Using sample weights = 1.")
      switch_data[, weight := weight]
    } else {
      switch_data[, weight := weight * sample_weight]
    }

  }


  vars <- c()
  if(any(!is.na(model_var))){
    # if the model_var is not empty, we use the information provided by user
    vars <- c(vars, model_var)
  }else{
    # if the model_var is empty, we provide the needed variables based on analysis type
    if(use_censor == 0){
      if(use_weight == 0){
        # for ITT analysis
        vars <- c(vars, "assigned_treatment")
      }else{
        # for as treated analysis
        vars <- c(vars, c("dose", "dose2"))
      }
    }else{
      # for per-protocol analysis
      vars <- c(vars, "assigned_treatment")
    }
  }
  if(any(!is.na(include_expansion_time_case))){
    if("linear" %in% include_expansion_time_case){
      vars <- c(vars, "for_period")
    }
    if("quadratic" %in% include_expansion_time_case){
      vars <- c(vars, "for_period2")
    }
    if("spline" %in% include_expansion_time_case){
      idx <- grepl("period_base", colnames(switch_data))
      if(any(idx)){
        vars <- c(vars, colnames(switch_data)[idx])
      } else if(!any(idx)) {
        warning("Splines specified for period but not 'period_base' columns found (include_expansion_time_case).")
      }
    }
  }else{
    message("No trial period term included in outcome model (include_expansion_time_case).")
  }
  if(any(!is.na(include_followup_time_case))){
    if("linear" %in% include_followup_time_case){
      vars <- c(vars, "followup_time")
    }
    if("quadratic" %in% include_followup_time_case){
      vars <- c(vars, "followup_time2")
    }
    if("spline" %in% include_followup_time_case){
      idx <- grepl("followup_base", colnames(switch_data))
      if(any(idx)){
        vars <- c(vars, colnames(switch_data)[idx])
      } else if(!any(idx)) {
        warning("Splines specified for follow-up but not 'followup_base' columns found (include_followup_time_case).")
      }
    }
  }else{
    message("No follow-up time term included in outcome model (include_followup_time_case).")
  }
  if(any(!is.na(outcomeCov))){
    vars <- c(vars, outcomeCov)
  }
  regform <- paste(
    paste("outcome", "~"),
    paste(
      paste(vars, collapse="+"),
      sep="+"
    )
  )

  if(any(!is.na(where_case))){
    timing = system.time({
      d = list()
      for(i in 1:length(where_case)){
        d[[i]] = list(switch_data[eval(parse(text = where_case[i]))], regform)
      }
      if(numCores == 1) {
        #cl <- makeCluster(numCores)
        #m = parLapply(cl, d, lr)
        m = lapply(d, lr)
        #stopCluster(cl)
      } else {
        m = mclapply(d, lr, mc.cores=numCores)
      }

      for(i in 1:length(where_case)){
        h_quiet_print(quiet, paste("Analysis with", where_case[i], sep=" "))
        h_quiet_print(quiet, summary(m[i]$model))
        h_quiet_print(quiet, paste("Analysis with", where_case[i], "using robust variance", sep=" "))
        h_quiet_print(quiet, m[i]$output)
      }
    })
    h_quiet_print(quiet, "------------------------------------")
    h_quiet_print(quiet, "Processing time of modeling for where case analysis in total parallel:")
    h_quiet_print(quiet, timing)
  }

  if(run_base_model == 1){
    if(use_weight == 1){
      if(run_p99_analysis == 1){
        switch_data = p99_weight(switch_data)
      }else if(run_user_limits_analysis == 1){
        switch_data = limit_weight(switch_data, lower_weight, upper_weight)
      }else if(run_unweighted_analysis == 1){
        switch_data[, weight] = 1
      }
    }

    timing = system.time({
      if(glm_function == "parglm"){
        model.full = parglm::parglm(as.formula(regform), data=switch_data,
                                    weights=switch_data[, weight],
                                    family=binomial(link = "logit"),
                                    control=parglm::parglm.control(nthreads = 4, method='FAST'))
      } else if (glm_function == "glm"){
        model.full = stats::glm(as.formula(regform), data=switch_data,
                                weights=switch_data[, weight],
                                family=binomial(link = "logit"))
      }
    })
    h_quiet_print(quiet, "Base Analysis")
    h_quiet_print(quiet, summary(model.full))
    h_quiet_print(quiet, "-------------------------------------------------")
    h_quiet_print(quiet, "Processing time of modeling for base analysis:")
    h_quiet_print(quiet, timing)

    h_quiet_print(quiet, "Base Analysis with robust variance")
    timing = system.time({
      h_quiet_print(quiet, "-------------------------------------------------------")
      h_quiet_print(quiet, "Robust standard error:")
      robust_model = robust_calculation(model.full, switch_data[["id"]])
    })
    h_quiet_print(quiet, robust_model)
    h_quiet_print(quiet, "----------------------------------------------")
    h_quiet_print(quiet, "Processing time of getting the output and sandwich with reduced switch data:")
    h_quiet_print(quiet, timing)
  }

  return(list(model = model.full, robust = robust_model, switch_data = switch_data))
}
