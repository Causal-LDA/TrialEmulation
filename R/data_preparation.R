#' Data preparation Function
#'
#' This function prepare the data for modelling.
#' @param data_path The path to csv file
#' @param id Name of the data column for id feature Defaults to id
#' @param period Name of the data column for period feature Defaults to period
#' @param treatment Name of the data column for treatment feature Defaults to treatment
#' @param outcome Name of the data column for outcome feature Defaults to outcome
#' @param eligible Indicator of whether or not an observation is eligible to be expanded about Defaults to eligible
#' @param outcomeCov_var List of individual baseline variables used in final model
#' @param outcomeCov List of functions of baseline covariates used in final model
#' @param outcomeClass Categorical variables used in the final model
#' @param model_var List of Variables of interest to be used in final model
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
#' @param include_followup_time_case The model to include follow up time in outcome model. This has 3 options c("linear","quadratic","spline").
#' @param include_expansion_time_case The model to include for_period in outcome model. This has 3 options c("linear","quadratic","spline")
#' @param followup_spline The parameters for spline model for followup time when choose "spline" in the include_followup_time_case (ex. list(df=2))
#' @param period_spline The parameters for spline model for for_period when choose "spline" in the include_expansion_time_case (ex. list(df=3))
#' @param include_regime_length If defined as 1 a new variable (time_on_regime) is added to dataset - This variable stores the duration of time that the patient has been on the current treatment value
#' @param eligible_wts_0 Eligibility criteria used in weights for model condition Am1 = 0
#' @param eligible_wts_1 Eligibility criteria used in weights for model condition Am1 = 1
#' @param lag_p_nosw When 1 this will set the first weight to be 1 and use p_nosw_d and p_nosw_n at followup-time (t-1) for calculating the weights at followup-time t - can be set to 0 which will increase the maximum and variance of weights (Defaults to 1)
#' @param where_var List of variables used in where conditions used in subsetting the data used in final analysis (where_case), the variables not included in the final model
#' @param data_dir Direction to save data
#' @param numCores Number of cores for parallel programming (default value is maximum cores and parallel programming)
#' @param chunk_expansion Do the expansion in chunks (and in parallel if numCores > 1). Turn this off if you have enough memory to expand the whole dataset at once. (default TRUE)
#' @param chunk_size Number of ids to process at once for the chunk expansion (default 500). Larger chunk_sizes may be faster but require more memory.
#' @param separate_files Write to one file or one per trial (default FALSE)
#' @param quiet Don't print progress messages.
#' data_preparation()
#' @export
#'
#' @details The class variables paramers (`outcomeClass`,`class_switchn`,`class_switchd`,`class_censen`,`class_censed`)
#' can be given as a character vector which will construct factors using `as.factor` or as a named list with the arguments for factor
#' eg `list(risk_cat=list(levels = c(1,2,3,0), age_cat=list(levels=c(1,2,3),labels=c("50-60","60-70","70+")`


# CCS: data_path is now data.frame
data_preparation <- function(data_path,
                             id = "id",
                             period = "period",
                             treatment = "treatment",
                             outcome = "outcome",
                             eligible = "eligible",
                             outcomeCov_var = NA,
                             outcomeCov = NA,
                             outcomeClass = NA,
                             model_var = NA,
                             cov_switchn = NA,
                             model_switchn = NA,
                             class_switchn = NA,
                             cov_switchd = NA,
                             model_switchd = NA,
                             class_switchd = NA,
                             first_period = NA,
                             last_period = NA,
                             use_weight = 0,
                             use_censor = 0,
                             check_missing = 0,
                             cense = NA,
                             pool_cense = 0,
                             cov_censed = NA,
                             model_censed = NA,
                             class_censed = NA,
                             cov_censen = NA,
                             model_censen = NA,
                             class_censen = NA,
                             include_followup_time_case = "linear",
                             include_expansion_time_case = "linear",
                             followup_spline = NA,
                             period_spline = NA,
                             include_regime_length = 0,
                             eligible_wts_0 = NA,
                             eligible_wts_1 = NA,
                             lag_p_nosw = 1,
                             where_var = NA,
                             data_dir,
                             numCores = NA,
                             chunk_expansion = TRUE,
                             chunk_size = 500,
                             separate_files = FALSE,
                             quiet = FALSE) {
  assert_flag(quiet)

  # outcomeCov_var needs to have outcomeCov
  if(any(!is.na(outcomeCov_var)) & any(is.na(outcomeCov))){
    outcomeCov_var = NA
  }
  # outcomeClass needs to be with outcomeCov and outcomeCov_var
  if(any(!is.na(outcomeClass))){
    if(any(is.na(outcomeCov))){
      outcomeClass = NA
      outcomeCov_var = NA
    }
    if(any(is.na(outcomeCov_var))){
      outcomeClass = NA
      outcomeCov = NA
    }
  }

  # cov_switchn needs to have model_switchn
  if(any(!is.na(cov_switchn)) & any(is.na(model_switchn))){
    cov_switchn = NA
  }
  # class_switchn needs to be with cov_switchn and model_switchn
  if(any(!is.na(class_switchn))){
    if(any(is.na(model_switchn))){
      class_switchn = NA
      cov_switchn = NA
    }
    if(any(is.na(cov_switchn))){
      class_switchn = NA
      model_switchn = NA
    }
  }

  # cov_switchd needs to have model_switchd
  if(any(!is.na(cov_switchd)) & any(is.na(model_switchd))){
    cov_switchd = NA
  }
  # class_switchd needs to be with cov_switchd and model_switchd
  if(any(!is.na(class_switchd))){
    if(any(is.na(model_switchd))){
      class_switchd = NA
      cov_switchd = NA
    }
    if(any(is.na(cov_switchd))){
      class_switchd = NA
      model_switchd = NA
    }
  }

  # cov_censed needs to have model_censed
  if(any(!is.na(cov_censed)) & any(is.na(model_censed))){
    cov_censed = NA
  }
  # class_censed needs to be with cov_censed and model_censed
  if(any(!is.na(class_censed))){
    if(any(is.na(model_censed))){
      class_censed = NA
      cov_censed = NA
    }
    if(any(is.na(cov_censed))){
      class_censed = NA
      model_censed = NA
    }
  }

  # cov_censen needs to have model_censen
  if(any(!is.na(cov_censen)) & any(is.na(model_censen))){
    cov_censen = NA
  }
  # class_censen needs to be with cov_censen and model_censen
  if(any(!is.na(class_censen))){
    if(any(is.na(model_censen))){
      class_censen = NA
      cov_censen = NA
    }
    if(any(is.na(cov_censen))){
      class_censen = NA
      model_censen = NA
    }
  }

  if(is.na(numCores)){
    numCores = max(1, detectCores()/2)
  }

  if(.Platform$OS.type == "windows"){
    numCores = 1
  }


  # CCS
  #if(!file.exists(data_path)) stop(paste0("'data_path' file not found: ", data_path))
  #absolutePath <- normalizePath(data_path)

  # data = tryCatch({
  #   suppressWarnings(out <- bigmemory::read.big.matrix(absolutePath, header = TRUE, type="double"))
  # })

  keeplist <- c("id", "for_period", "followup_time", "outcome",
                "weight", "treatment")
  if("quadratic" %in% include_expansion_time_case){
    keeplist <- c(keeplist, "for_period2")
  }
  # if("spline" %in% include_expansion_time_case){
  #   if(df %in% period_spline){
  #     n = period_spline$df
  #   }else{
  #     n = 1
  #   }
  #   for(i in 1:n){
  #     keeplist <- c(keeplist, paste0("period_base_", i))
  #   }
  # }
  if("quadratic" %in% include_followup_time_case){
    keeplist <- c(keeplist, "followup_time2")
  }
  # if("spline" %in% include_followup_time_case){
  #   if(df %in% followup_spline){
  #     n = followup_spline$df
  #   }else{
  #     n = 1
  #   }
  #   for(i in 1:n){
  #     keeplist <- c(keeplist, paste0("followup_base_", i))
  #   }
  # }
  if(any(!is.na(outcomeCov_var))){
    keeplist <- c(keeplist, outcomeCov_var)
  }
  if(any(!is.na(where_var))){
    keeplist <- c(keeplist, where_var)
  }
  if(any(!is.na(model_var))){
    # if the model_var is not empty, we use the information provided by user
    keeplist <- c(keeplist, model_var)
  }else{
    # if the model_var is empty, we provide the needed variables based on analysis type
    if(use_censor == 0){
      if(use_weight == 0){
        # for ITT analysis
        keeplist <- c(keeplist, "assigned_treatment")
      }else{
        # for as treated analysis
        keeplist <- c(keeplist, c("dose", "dose2"))
      }
    }else{
      # for per-protocol analysis
      keeplist <- c(keeplist, "assigned_treatment")
    }
  }
  if(! "assigned_treatment" %in% keeplist){
    keeplist <- c(keeplist, "assigned_treatment")
  }

  h_quiet_print(quiet, "Start data manipulation")
  timing = system.time({
    # CCS: Change par 2 from absolutePath to data_path
    sw_data <-
      data_manipulation(NA, data_path, keeplist,
                        treatment, id, period, outcome, eligible,
                        outcomeCov_var,
                        cov_switchn, model_switchn, class_switchn,
                        cov_switchd, model_switchd, class_switchd,
                        first_period, last_period,
                        use_weight, use_censor, check_missing,
                        cense, pool_cense, cov_censed, model_censed,
                        class_censed, cov_censen, model_censen,
                        class_censen,
                        include_regime_length, eligible_wts_0,
                        eligible_wts_1, lag_p_nosw, where_var, data_dir,
                        numCores,
                        quiet = quiet)
  })
  h_quiet_print(quiet, "Finish data manipulation")
  h_quiet_print(quiet, "Processing time of data manipulation:")
  h_quiet_print(quiet, timing)
  h_quiet_print(quiet, "----------------------------")
  # CCS
  #absolutePath <- normalizePath(file.path(data_dir, "sw_data.csv"))

  h_quiet_print(quiet, "Start data extension")
  timing = system.time({
    # CCS: Don't use switch_data.csv
    #df <- data.frame(matrix(ncol = length(keeplist), nrow = 0))
    #colnames(df) <- keeplist

    #write.csv(df, file.path(data_dir, "switch_data.csv"), row.names=FALSE)


    # CCS: Force use of single-core version
    manipulate = data_extension(data_path = NA, keeplist, outcomeCov_var,
                   first_period, last_period, use_censor, lag_p_nosw,
                   where_var, followup_spline, period_spline,
                   data_dir, sw_data = sw_data)

    ## if(!chunk_expansion && numCores == 1){
    ##   #doesn't want to do chunks or parallel threads, but might have to do chunks if not enough memory
    ##   manipulate = tryCatch(
    ##     data_extension(absolutePath, keeplist, outcomeCov_var,
    ##                    first_period, last_period, use_censor, lag_p_nosw,
    ##                    where_var, followup_spline, period_spline,
    ##                    data_dir),

    ##     error = function(err){
    ##       gc()
    ##       file.remove(file.path(data_dir, "switch_data.csv"))
    ##       write.csv(df, file.path(data_dir, "switch_data.csv"), row.names=FALSE)
    ##       warning(paste0("The memory is not enough to do the data extention without data division so performed in chunks with numCores=1 and chunk_size=",chunk_size,"!"))
    ##       # data = tryCatch({
    ##       #   suppressWarnings(bigmemory::read.big.matrix(absolutePath, header = TRUE, type="double"))
    ##       # })
    ##       data_extension_parallel(sw_data, keeplist, outcomeCov_var,
    ##                               first_period, last_period, use_censor, lag_p_nosw,
    ##                               where_var, followup_spline,
    ##                               period_spline, data_dir, numCores,
    ##                               chunk_size, separate_files)
    ##     }
    ##   )
    ## }else{
    ##   # data = tryCatch({
    ##   #   suppressWarnings(bigmemory::read.big.matrix(absolutePath, header = TRUE, type="double"))
    ##   # })

    ##   manipulate = data_extension_parallel(sw_data, keeplist,
    ##                                        outcomeCov_var,
    ##                                        first_period, last_period,
    ##                                        use_censor, lag_p_nosw,
    ##                                        where_var, followup_spline,
    ##                                        period_spline, data_dir,
    ##                                        numCores, chunk_size, separate_files)
    ## }





  })
  h_quiet_print(quiet, "Finish data extension")
  h_quiet_print(quiet, "Processing time of data extension:")
  h_quiet_print(quiet, timing)
  h_quiet_print(quiet, "----------------------------")

  h_quiet_print(quiet, paste0("Number of observations in expanded data: ",manipulate$N))

  # CCS
  #rm(sw_data)
  #gc()


  #
  #   if(is.na(first_followup)){
  #     first_followup = 0
  #   }
  #   if(is.na(last_followup)){
  #     last_followup = manipulate$max_period
  #   }
  #

  # CCS: Change return type
  manipulate
  
  #return(list(absolutePath = manipulate$path,
  #            N = manipulate$N,
  #            range = manipulate$range,
  #            min_period = manipulate$min_period,
  #            max_period = manipulate$max_period))
}
