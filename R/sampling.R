
#' Case-control sampling from extended data
#'
#' @param data_dir Directory containing 'sw_data.csv' and "switch_data.csv"
#' @param n_control Number of controls for each case to sample
#' @param numCores Number of cores used by mclapply for sampling from each trial
#' @param min_period First trial period to sample from
#' @param max_period Last trial period to sample from
#' @param samples_file CSV file name for the  sampled data. Should be the same length as n_control.
#'
#' @export
#'
case_control_sampling <- function(data_dir, samples_file = "sample_data.csv", min_period, max_period, n_control, numCores=1){
  print("Starting case-control sampling function")

  if(length(samples_file) != length(n_control)){
    warning("Number of filenames and number of samples are not equal. Using first specified name and counter.")
    samples_file <- paste0(rep(sub(".csv","",samples_file[1]), length(n_control)),
                 "_",seq_along(n_control),".csv")
  }

  if(missing(min_period)|missing(max_period)){
    # get the periods
    print("Getting the periods")
    timing <- system.time({
      small_data <- fread(normalizePath(file.path(data_dir, "sw_data.csv")))
      max_period = max(small_data[, "period"])
      min_period = min(small_data[, "period"])
    })
    rm(small_data)
    print("Finished getting the periods")
    print(timing)
    print("-------------------------")
  }


  # read the data:
  print("Reading the expanded data")
  timing <- system.time({
    absolutePath <- normalizePath(file.path(data_dir, "switch_data.csv"))
    data_address = tryCatch({
      suppressWarnings(out <- bigmemory::read.big.matrix(absolutePath, header = TRUE, shared=FALSE, type="double"))
    })
  })
  print("Finished reading the expanded data")
  print(timing)
  print("-------------------------")

  for(i in seq_along(n_control)){

    print("Starting the sampling")
    timing <- system.time({
      j = seq(min_period, max_period, 1)
      mclapply(j, case_control_func,
               data_address=data_address, n_control=n_control[i],
               data_dir=data_dir,
               name_csv = samples_file,
               numCores=1,
               mc.cores=numCores)
    })
    print("Finished sampling")
    print(timing)
    print("-------------------------")
  }

  return(file.path(data_dir,samples_file))
}


#' Case Control Sampling Function
#'
#' This function apply case control sampling on the extended data
#'
#' @param period_num Period id
#' @param data_address A data.table which is the extended version of input data
#' @param n_control Number of controls in the case control sampling Defaults to 5
#' @param numCores Number of cores for parallel programming
#' @param name_csv File name for the sampled data csv file
#' @param data_dir Directory to write sampled data csv to
#'

case_control_func <- function(period_num, data_address, n_control=5,
                              data_dir="~/rds/hpc-work/",
                              name_csv = "sample_data.csv",
                              numCores=NA){

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
  fwrite(new_d, file.path(data_dir, name_csv), append=TRUE, row.names=FALSE)
  rm(d_period, d, m, new_d)
  gc()
}

#' Case-control sampling from extended data in separate trial csvs
#'
#' @param data_dir Directory containing trial-level extended data
#' @param n_control Number of controls to sample per case
#' @param numCores Number of cores for parallel processing
#' @param samples_file Name of output csv files for each case-control sample. eg `paste0("samples_",seq_along(n_control),"_1x",n_control,".csv")`
#' @param infile_pattern Name of trial dataset csv files. This is passed to the `pattern` argument of `dir()`
#' @param subset_condition Expression used to `subset` the trial data before sampling
#'
#' @return A vector of file paths of the sampled data
#'
#' @export
#'
case_control_sampling_trials <- function(data_dir, n_control, numCores, samples_file = "sample_data.csv", infile_pattern = "trial_", subset_condition){

  trial_files <- dir(path=data_dir, pattern = infile_pattern, full.names = TRUE)

  if(length(samples_file) != length(n_control)){
    warning("Number of filenames and number of samples are not equal. Using first specified name and counter.")
    samples_file <- paste0(rep(sub(".csv","",samples_file[1]), length(n_control)),
                           "_",seq_along(n_control),".csv")
  }

  if(!missing(subset_condition)) subset_cond <- substitute(subset_condition)

  # function used in mclapply to read in a trial file and call sampling function
  trial_fun <- function(trial_file){
    d_period <- fread(input = trial_file)

    if(exists("subset_cond")) d_period <- subset(d_period, eval(subset_cond))

    d <- split(d_period, d_period$followup_time)

    all_samples <- lapply(n_control, function(nc){
      m <- lapply(d, case_util, n_control=nc)
      rbindlist(m)
    })

    rbindlist(all_samples, idcol = TRUE)
  }

  # Start of the actual sampling
  trial_samples <- mclapply(trial_files, trial_fun, mc.cores = numCores, mc.preschedule = FALSE)
  trial_samples_bind <- rbindlist(trial_samples)

  if(nrow(trial_samples_bind)>0){
    mapply(fwrite,
           split(trial_samples_bind, by = ".id", keep.by = FALSE),
           file.path(data_dir, samples_file),
           append=FALSE, row.names=FALSE)

    return(file.path(data_dir, samples_file))
  }
}




#' Sample cases utility function
#'
#' @param data Data to sample from
#' @param n_control Number of controls for each case
#'
#' @return A data frame with the cases and sampled controls

case_util <- function(data, n_control=5){
  ### cases occurred at each period and follow-up visit
  casedatajk<-data[data$outcome==1, ]
  ### controls (still under follow-up and events haven't occurred) at each period and follow-up visit
  controldatajk<-data[data$outcome==0, ]
  dataall = NULL
  ncase<-dim(casedatajk)[1]  ## number of cases
  ncontrol<-dim(controldatajk)[1]  ## number of potential controls
  if(ncase>0){

    if(ncontrol >= n_control*ncase) {
      controlselect <- controldatajk[sample(1:ncontrol, n_control*ncase),] ## sample 5 controls for each case without replacement
    } else{
      warning(paste0(ncontrol, ifelse(ncontrol==1, "control", "controls"),
       " available in this period but ", n_control*ncase, " were requested. All available controls will be used."))
      controlselect <- controldatajk
    }

    # sampling weights
    casedatajk$samplew<-rep(1,ncase)
    controlselect$samplew<-rep(ncontrol/(ncase*5), ncase*5)

    dataall<-rbind(casedatajk, controlselect) ## append sampled data
  }
  return(dataall)
}
