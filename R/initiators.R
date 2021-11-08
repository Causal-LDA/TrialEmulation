#' INITIATORS Function
#'
#' This function analyses observational longitudinal data to estimate the effect of interventions sustained over time. This emulates the design of a hypothetical randomized trial.
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
#' @param cense Censoring variable
#' @param pool_cense Pool the numerator and denominator models (0: split models by previous treatment Am1 = 0 and Am1 = 1 as in treatment models and 1: pool all observations together into a single numerator and denominator model) Defaults to 0
#' @param cov_censed List of covariates to be used in logistic model for censoring weights in denominator model
#' @param model_censed List of models (functions) to use the covariates from cov_censed
#' @param class_censed Class variables used in censoring logistic regression in denominator model
#' @param cov_censen List of covariates to be used in logistic model for censoring weights in numerator model
#' @param model_censen List of models (functions) to use the covariates from cov_censen
#' @param class_censen Class variables used in censoring logistic regression in numerator model
#' @param include_followup_time_case The model to include follow up time in outcome model. This has 3 options c("linear","quadratic","spline")
#' @param include_expansion_time_case The model to include for_period in outcome model. This has 3 options c("linear","quadratic","spline")
#' @param followup_spline The parameters for spline model for followup time when choose "spline" in the include_followup_time_case (ex. list(df=2))
#' @param period_spline The parameters for spline model for for_period when choose "spline" in the include_expansion_time_case (ex. list(df=3))
#' @param include_regime_length If defined as 1 a new variable (time_on_regime) is added to dataset - This variable stores the duration of time that the patient has been on the current treatment value
#' @param eligible_wts_0 Eligibility criteria used in weights for model condition Am1 = 0
#' @param eligible_wts_1 Eligibility criteria used in weights for model condition Am1 = 1
#' @param lag_p_nosw When 1 this will set the first weight to be 1 and use p_nosw_d and p_nosw_n at followup-time (t-1) for calculating the weights at followup-time t - can be set to 0 which will increase the maximum and variance of weights (Defaults to 1)
#' @param where_var List of variables used in where conditions used in subsetting the data used in final analysis (where_case), the variables not included in the final model
#' @param where_case List of where conditions used in subsetting the data used in final analysis
#' @param run_base_model Run the model with no conditions Defaults to 1
#' @param case_control Run the case control sampling or not Defaults to 0
#' @param n_control Number of controls used in case control sampling Defaults to 5
#' @param data_dir Direction to save data
#' @param numCores Number of cores for parallel programming (default value is maximum cores and parallel programming)
#' @param chunk_expansion Do the expansion in chunks (and in parallel if numCores > 1). Turn this off if you have enough memory to expand the whole dataset at once. (default TRUE)
#' @param chunk_size Number of ids to process at once for the chunk expansion (default 500). Larger chunk_sizes may be faster but require more memory.
#' @param separate_files Write to one file or one per trial (default FALSE)
#' initiators()
#' @export
#' @import foreach
#' @import doParallel
#' @import parallel
#' @import data.table
#' @importFrom Rcpp sourceCpp
#' @useDynLib RandomisedTrialsEmulation

initiators <- function(data_path, id="id", period="period",
                       treatment="treatment", outcome="outcome",
                       eligible="eligible", outcomeCov_var=NA,
                       outcomeCov=NA, outcomeClass=NA, model_var=NA,
                       cov_switchn=NA, model_switchn=NA, class_switchn=NA,
                       cov_switchd=NA, model_switchd=NA, class_switchd=NA,
                       first_period=NA, last_period=NA,
                       first_followup=NA, last_followup=NA,
                       use_weight=0, run_unweighted_analysis=0,
                       run_weighted_analysis=1, run_p99_analysis=0,
                       run_user_limits_analysis=0, lower_weight=NA,
                       upper_weight=NA, use_censor=0, check_missing=0,
                       cense=NA, pool_cense=0,
                       cov_censed=NA, model_censed=NA, class_censed=NA,
                       cov_censen=NA, model_censen=NA, class_censen=NA,
                       include_followup_time_case="linear",
                       include_expansion_time_case="linear",
                       followup_spline=NA,
                       period_spline=NA,
                       include_regime_length=0,
                       eligible_wts_0=NA, eligible_wts_1=NA, lag_p_nosw=1,
                       where_var=NA, where_case=NA, run_base_model=1,
                       case_control=0, n_control=5,
                       data_dir="~/rds/hpc-work/",
                       numCores=NA, chunk_expansion=TRUE,
                       chunk_size=500, separate_files=FALSE){

  prep_result <- data_preparation(data_path, id, period, treatment, outcome,
                                  eligible, outcomeCov_var, outcomeCov,
                                  outcomeClass, model_var, cov_switchn,
                                  model_switchn, class_switchn, cov_switchd,
                                  model_switchd, class_switchd,
                                  first_period, last_period,
                                  first_followup, last_followup,
                                  use_weight, run_unweighted_analysis,
                                  run_weighted_analysis, run_p99_analysis,
                                  run_user_limits_analysis, lower_weight,
                                  upper_weight, use_censor, check_missing,
                                  cense, pool_cense, cov_censed, model_censed, class_censed,
                                  cov_censen, model_censen, class_censen,
                                  include_followup_time_case,
                                  include_expansion_time_case,
                                  followup_spline,
                                  period_spline,
                                  include_regime_length,
                                  eligible_wts_0, eligible_wts_1, lag_p_nosw,
                                  where_var, where_case, run_base_model,
                                  case_control, n_control, data_dir, numCores,
                                  chunk_expansion, chunk_size, separate_files)

  model_full <- data_modelling(id, period, treatment, outcome,
                               eligible, outcomeCov_var, outcomeCov,
                               outcomeClass, model_var, cov_switchn,
                               model_switchn, class_switchn, cov_switchd,
                               model_switchd, class_switchd,
                               first_period, last_period,
                               prep_result$first_followup,
                               prep_result$last_followup,
                               use_weight, run_unweighted_analysis,
                               run_weighted_analysis, run_p99_analysis,
                               run_user_limits_analysis, lower_weight,
                               upper_weight, use_censor, check_missing,
                               cense, pool_cense, cov_censed, model_censed,
                               class_censed, cov_censen, model_censen,
                               class_censen, include_followup_time_case,
                               include_expansion_time_case,
                               followup_spline,
                               period_spline,
                               include_regime_length,
                               eligible_wts_0, eligible_wts_1, lag_p_nosw,
                               where_var, where_case, run_base_model,
                               case_control, n_control,
                               data_dir, prep_result$absolutePath, numCores)

  return(list(model = model_full))
}


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
#' @param where_case List of where conditions used in subsetting the data used in final analysis
#' @param run_base_model Run the model with no conditions Defaults to 1
#' @param case_control Run the case control sampling or not Defaults to 0
#' @param n_control Number of controls used in case control sampling Defaults to 5
#' @param data_dir Direction to save data
#' @param numCores Number of cores for parallel programming (default value is maximum cores and parallel programming)
#' @param chunk_expansion Do the expansion in chunks (and in parallel if numCores > 1). Turn this off if you have enough memory to expand the whole dataset at once. (default TRUE)
#' @param chunk_size Number of ids to process at once for the chunk expansion (default 500). Larger chunk_sizes may be faster but require more memory.
#' @param separate_files Write to one file or one per trial (default FALSE)
#' data_preparation()
#' @export


data_preparation <- function(data_path, id="id", period="period",
                             treatment="treatment", outcome="outcome",
                             eligible="eligible", outcomeCov_var=NA,
                             outcomeCov=NA, outcomeClass=NA, model_var=NA,
                             cov_switchn=NA, model_switchn=NA, class_switchn=NA,
                             cov_switchd=NA, model_switchd=NA, class_switchd=NA,
                             first_period=NA, last_period=NA,
                             first_followup=NA, last_followup=NA,
                             use_weight=0, run_unweighted_analysis=0,
                             run_weighted_analysis=1, run_p99_analysis=0,
                             run_user_limits_analysis=0, lower_weight=NA,
                             upper_weight=NA, use_censor=0, check_missing=0,
                             cense=NA, pool_cense=0,
                             cov_censed=NA, model_censed=NA, class_censed=NA,
                             cov_censen=NA, model_censen=NA, class_censen=NA,
                             include_followup_time_case="linear",
                             include_expansion_time_case="linear",
                             followup_spline=NA,
                             period_spline=NA,
                             include_regime_length=0,
                             eligible_wts_0=NA, eligible_wts_1=NA, lag_p_nosw=1,
                             where_var=NA, where_case=NA, run_base_model=1,
                             case_control=0, n_control=5,
                             data_dir="~/rds/hpc-work/",
                             numCores=NA,
                             chunk_expansion=TRUE,
                             chunk_size=500,
                             separate_files=FALSE){

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

  # line1 = read.csv(data_path, header = TRUE, nrows = 1)
  # col.names = colnames(line1)

  absolutePath <- normalizePath(data_path)

  data = tryCatch({
    suppressWarnings(out <- bigmemory::read.big.matrix(absolutePath, header = TRUE, type="double"))
  })

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

  print("Start data manipulation")
  timing = system.time({
    data_manipulation(data, data_path, keeplist,
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
                      numCores)
    })
  print("Finish data manipulation")
  print("Processing time of data manipulation:")
  print(timing)
  print("----------------------------")
  absolutePath <- normalizePath(file.path(data_dir, "sw_data.csv"))

  print("Start data extension")
  timing = system.time({
    df <- data.frame(matrix(ncol = length(keeplist), nrow = 0))
    colnames(df) <- keeplist

    write.csv(df, file.path(data_dir, "switch_data.csv"), row.names=FALSE)

    if(!chunk_expansion && numCores == 1){
      #doesn't want to do chunks or parallel threads, but might have to do chunks if not enough memory
      manipulate = tryCatch(
        data_extension(absolutePath, keeplist, outcomeCov_var,
                       first_period, last_period, use_censor, lag_p_nosw,
                       where_var, followup_spline, period_spline,
                       data_dir),
        error = function(err){
          gc()
          file.remove(file.path(data_dir, "switch_data.csv"))
          write.csv(df, file.path(data_dir, "switch_data.csv"), row.names=FALSE)
          print(paste0("The memory is not enough to do the data extention without data division so performed in chunks with numCores=1 and chunk_size=",chunk_size,"!"))
          data = tryCatch({
            suppressWarnings(out <- bigmemory::read.big.matrix(absolutePath, header = TRUE, type="double"))
          })
          data_extension_parallel(data, keeplist, outcomeCov_var,
                                  first_period, last_period, use_censor,
                                  lag_p_nosw, where_var, followup_spline,
                                  period_spline, data_dir, numCores,
                                  chunk_size, separate_files)
        }
      )
    }else{
      data = tryCatch({
        suppressWarnings(out <- bigmemory::read.big.matrix(absolutePath, header = TRUE, type="double"))
      })

      manipulate = data_extension_parallel(data, keeplist,
                                           outcomeCov_var,
                                           first_period, last_period,
                                           use_censor, lag_p_nosw,
                                           where_var, followup_spline,
                                           period_spline, data_dir,
                                           numCores, chunk_size, separate_files)
    }

    # adding spline
    add_splines(data_path = file.path(data_dir, "switch_data.csv"),
                period_spline = period_spline,
                followup_spline = followup_spline)


  })
  print("Finish data extension")
  print("Processing time of data extension:")
  print(timing)
  print("----------------------------")

  print(paste0("Number of observations in expanded data: ",manipulate$N))
  n_rows <- manipulate$N
  range <- manipulate$range
  min_period = manipulate$min_period
  max_period = manipulate$max_period

  if(is.na(first_followup)){
    first_followup = 0
  }
  if(is.na(last_followup)){
    last_followup = max_period
  }

  rm(data, absolutePath)
  gc()


  # absolutePath <- normalizePath(file.path(data_dir, "switch_data.csv"))
  # data_address = tryCatch({
  #   suppressWarnings(out <- bigmemory::read.big.matrix(absolutePath, header = TRUE, shared=FALSE, type="double"))
  # })

  write.csv(df, file.path(data_dir, "temp_data.csv"), row.names=FALSE)
  # print("----------------------------------------------")
  # print("Analysis of weights for switching treatment:")
  # print(summary(switch_data[, weight]))
  # print(describe(switch_data[, weight]))
  # print("variance:")
  # print(var(switch_data[, weight]))
  # print("standard deviation:")
  # print(sd(switch_data[, weight]))

  if(case_control == 1){ # Do case-control sampling:
    if(!separate_files){# Extended data in switch_data.csv

      sample_data_path <- case_control_sampling(data_dir,
                                                samples_file = "temp_data.csv",
                                                min_period=min_period,
                                                max_period = max_period,
                                                n_control, numCores=numCores)

      absolutePath <- normalizePath(sample_data_path)

    }else{ # Extended data in trial_*.csv files
      sample_data_path <- case_control_sampling_trials(data_dir, n_control, numCores, samples_file="temp_data.csv", infile_pattern = "trial_")
      absolutePath <- normalizePath(sample_data_path)
    }
  }else{ # No case-control sampling
    if(!separate_files){# Extended data in switch_data.csv

      if(n_rows >= 2^31 -1){ #if data is too big, we sample

        print("case control sampling needed!")
        sample_data_path <- case_control_sampling(data_dir,
                                                  samples_file = "temp_data.csv",
                                                  min_period=min_period,
                                                  max_period = max_period,
                                                  n_control, numCores=numCores)

        absolutePath <- normalizePath(sample_data_path)

      } else { #data is not bigger than R's limit
        absolutePath <- normalizePath(file.path(data_dir, "switch_data.csv"))
      }
    } else { # Extended data in trial_*.csv files, but case-control != 1
      print("Separate trial files used but case control sampling not specified. Sampling will be done anyway!")
      sample_data_path <- case_control_sampling_trials(data_dir, n_control, numCores, samples_file="temp_data.csv", infile_pattern = "trial_")
      absolutePath <- normalizePath(sample_data_path)
    }
  }

  gc()

  return(list(absolutePath = absolutePath,
              first_followup = first_followup,
              last_followup =last_followup))
}

#' Data modelling Function
#'
#' This function do the modelling.
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
#' @param where_case List of where conditions used in subsetting the data used in final analysis
#' @param run_base_model Run the model with no conditions Defaults to 1
#' @param case_control Run the case control sampling or not Defaults to 0
#' @param n_control Number of controls used in case control sampling Defaults to 5
#' @param absolutePath Direction to where the data for modelling is saved
#' @param data_dir Directory containing 'sw_data.csv'
#' @param numCores Number of cores for parallel programming (default value is maximum cores and parallel programming)
#' data_modelling()
#'
#' @details The class variables paramers (`outcomeClass`,`class_switchn`,`class_switchd`,`class_censen`,`class_censed`)
#' can be given as a character vector which will construct factors using `as.factor` or as a named list with the arguments for factor
#' eg `list(risk_cat=list(levels = c(1,2,3,0), age_cat=list(levels=c(1,2,3),labels=c("50-60","60-70","70+")`
#'
#' @export
#' @importFrom splines ns
#' @importFrom stats as.formula binomial pnorm quantile relevel
#' @importFrom utils write.csv


data_modelling <- function(id="id", period="period",
                           treatment="treatment", outcome="outcome",
                           eligible="eligible", outcomeCov_var=NA,
                           outcomeCov=NA, outcomeClass=NA, model_var=NA,
                           cov_switchn=NA, model_switchn=NA, class_switchn=NA,
                           cov_switchd=NA, model_switchd=NA, class_switchd=NA,
                           first_period=NA, last_period=NA,
                           first_followup=NA, last_followup=NA,
                           use_weight=0, run_unweighted_analysis=0,
                           run_weighted_analysis=1, run_p99_analysis=0,
                           run_user_limits_analysis=0, lower_weight=NA,
                           upper_weight=NA, use_censor=0, check_missing=0,
                           cense=NA, pool_cense=0,
                           cov_censed=NA, model_censed=NA, class_censed=NA,
                           cov_censen=NA, model_censen=NA, class_censen=NA,
                           include_followup_time_case="linear",
                           include_expansion_time_case="linear",
                           followup_spline=NA,
                           period_spline=NA,
                           include_regime_length=0,
                           eligible_wts_0=NA, eligible_wts_1=NA, lag_p_nosw=1,
                           where_var=NA, where_case=NA, run_base_model=1,
                           case_control=0, n_control=5,
                           data_dir="~/rds/hpc-work/",
                           absolutePath="~/rds/hpc-work/switch_data.csv",
                           numCores=NA){


  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  weight <- NULL

  path = normalizePath(file.path(data_dir, "sw_data.csv"))

  data_address = tryCatch({
    suppressWarnings(out <- bigmemory::read.big.matrix(path, header = TRUE, type="double", has.row.names = FALSE))
  })

  max_period = max(data_address[, "period"])
  min_period = min(data_address[, "period"])

  if(is.na(first_followup)){
    first_followup = 0
  }
  if(is.na(last_followup)){
    last_followup = max_period
  }

  rm(path, data_address)
  gc()

  # Isaac: if the data is in separate file what will happen here?
  data = tryCatch({
    suppressWarnings(out <- bigmemory::read.big.matrix(absolutePath, header=TRUE, shared=FALSE, type="double"))
  })

  temp_data = data[bigmemory::mwhich(data, c("followup_time", "followup_time"), c(first_followup, last_followup), c('ge', 'le'), 'AND'), ]
  temp_data = as.data.table(temp_data)
  cols = colnames(temp_data)
  rm(data)
  gc()

  if(any(!is.na(outcomeClass))){
    if(!(is.list(outcomeClass) | is.character(outcomeClass))) stop("outcomeClass is not a list or character vector")
    #class_var given as a character vector, convert to list
    if(is.character(outcomeClass)) outcomeClass <- as.list(outcomeClass)

    #process the list
    for(i in seq_along(outcomeClass)){
      # variable name provided only
      if(is.character(outcomeClass[[i]])) {
        this_var <- outcomeClass[[i]]
        set(temp_data, j = this_var, value = as.factor(temp_data[[this_var]]))
      }

      # named list provided
      if(is.list(outcomeClass[[i]])){
        this_var <- names(outcomeClass[i])
        set(temp_data, j = this_var,
            value = do.call("factor", c(x = list(temp_data[[this_var]]), outcomeClass[[this_var]])))
      }
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
      idx = sapply("period_base", function(y) grep(y, cols))
      if(any(regexpr("period_base", cols > 0))){
        vars <- c(vars, cols[idx])
      }
    }
  }else{
    vars <- c(vars, "for_period")
  }
  if(any(!is.na(include_followup_time_case))){
    if("linear" %in% include_followup_time_case){
      vars <- c(vars, "followup_time")
    }
    if("quadratic" %in% include_followup_time_case){
      vars <- c(vars, "followup_time2")
    }
    if("spline" %in% include_followup_time_case){
      idx = sapply("followup_base", function(y) grep(y, cols))
      if(any(regexpr("followup_base", cols > 0))){
        vars <- c(vars, cols[idx])
      }
    }
  }else{
    vars <- c(vars, "followup_time")
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
      d[[i]] = list(temp_data[eval(parse(text = where_case[i]))], regform)
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
      print(paste("Analysis with", where_case[i], sep=" "))
      print(summary(m[i]$model))
      print(paste("Analysis with", where_case[i], "using robust variance", sep=" "))
      print(m[i]$output)
    }
    })
    print("------------------------------------")
    print("Processing time of modeling for where case analysis in total parallel:")
    print(timing)
  }

  if(run_base_model == 1){
    if(use_weight == 1){
      if(run_p99_analysis == 1){
        temp_data = p99_weight(temp_data)
      }else if(run_user_limits_analysis == 1){
        temp_data = limit_weight(temp_data, lower_weight, upper_weight)
      }else if(run_unweighted_analysis == 1){
        temp_data[, weight] = 1
      }
    }

    timing = system.time({
      model.full = parglm::parglm(as.formula(regform), data=temp_data,
                                  weights=temp_data[, weight],
                                  family=binomial(link = "logit"),
                                  control=parglm::parglm.control(nthreads = 4, method='FAST'))
    })
    print("Base Analysis")
    print(summary(model.full))
    print("-------------------------------------------------")
    print("Processing time of modeling for base analysis:")
    print(timing)

    print("Base Analysis with robust variance")
    timing = system.time({
      out = robust_calculation(model.full, temp_data[, id])
    })
    print(out)
    print("----------------------------------------------")
    print("Processing time of getting the output and sandwich with reduced switch data:")
    print(timing)
  }

  return(list(model = model.full, robust = out))
}
