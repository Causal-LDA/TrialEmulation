#' Read Data Function
#'
#' This function read data from csv file and select the columns you need
#' @param data_address Address for data read with bigmemory
#' @param data_path Path of the csv file
#' @param id_num Id number
#' @param id Name of the data column for id feature Defaults to id
#' @param period Name of the data column for period feature Defaults to period
#' @param treatment Name of the data column for treatment feature Defaults to treatment
#' @param outcome Name of the data column for outcome feature Defaults to outcome
#' @param eligible Indicator of whether or not an observation is eligible to be expanded about Defaults to eligible
#' @param eligible_wts_0 Eligibility criteria used in weights for model condition Am1 = 0
#' @param eligible_wts_1 Eligibility criteria used in weights for model condition Am1 = 1
#' @param outcomeCov_var List of individual baseline variables used in final model
#' @param cov_switchn Covariates to be used in logistic model for switching probabilities for numerator model
#' @param cov_switchd Covariates to be used in logistic model for switching probabilities for denominator model
#' @param cov_censed Covariates to be used in logistic model for censoring weights for denominator model
#' @param cov_censen Covariates to be used in logistic model for censoring weights for nominator model
#' @param cense Censoring variable
#' @param where_var Variables used in where conditions used in subsetting the data used in final analysis (where_case), the variables not included in the final model
#' read_data()


# CCS: data_address is NA, data_path is data.frame
read_data <- function(data_address, data_path=NA, id_num=NA,
                      id="id",
                      period="period",
                      treatment="treatment",
                      outcome="outcome",
                      eligible="eligible",
                      eligible_wts_0=NA,
                      eligible_wts_1=NA,
                      outcomeCov_var=NA,
                      cov_switchn=NA, cov_switchd=NA,
                      cov_censed=NA, cov_censen=NA, cense=NA, where_var=NA){

  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  weight <- NULL

  covs <- c()
  if(any(!is.na(eligible_wts_0))){
    covs <- c(covs, eligible_wts_0)
  }
  if(any(!is.na(eligible_wts_1))){
    covs <- c(covs, eligible_wts_1)
  }
  if(any(!is.na(outcomeCov_var))){
    covs <- c(covs, outcomeCov_var)
  }
  if(any(!is.na(cov_switchd))){
    covs <- c(covs, cov_switchd)
  }
  if(any(!is.na(cov_switchn))){
    covs <- c(covs, cov_switchn)
  }
  if(any(!is.na(cov_censed))){
    covs <- c(covs, cov_censed)
  }
  if(any(!is.na(cov_censen))){
    covs <- c(covs, cov_censen)
  }
  if(any(!is.na(cense))){
    covs <- c(covs, cense)
  }
  if(any(!is.na(where_var))){
    covs <- c(covs, where_var)
  }
  covs <- covs[!duplicated(covs)]
  cols = c(id, period, treatment, outcome, eligible, covs)

  # CCS: Replace read with:
  setDT(data_path)
  data_new = data_path[, ..cols]
  
  #if(!is.na(id_num) & bigmemory::is.big.matrix(data_address)){
  #  data_new = as.data.table(data_address[bigmemory::mwhich(data_address, c("id"), c(id_num), c('eq')),])
  #}else{
  #  data_new = fread(data_path, header = TRUE, sep = ",", select = cols)
  #}

  if(!eligible %in% colnames(data_new)){
    warning(paste0("Eligibility variable not found in data: ",eligible))
    warning("Eligibility set to 1 for all patients for all periods")
    data_new[, (eligible) := 1]
  }

  # CCS
  #data_new = subset(data_new, select=cols)

  tryCatch({
    suppressWarnings(setnames(data_new,
                              c(id, period, outcome, eligible, treatment),
                              c("id", "period", "outcome", "eligible", "treatment")))
  })
  if(any(!is.na(eligible_wts_0))){
    setnames(data_new, c(eligible_wts_0), c("eligible_wts_0"))
  }
  if(any(!is.na(eligible_wts_1))){
    setnames(data_new, c(eligible_wts_1), c("eligible_wts_1"))
  }
  # ccs
  #rm(covs, cols)
  data_new = data_new[order(id, period)]
  return(data_new)
}


#' Period Expanding Function
#'
#' This function get the data.table with period column and expand it based on it
#' @param y The data.table with period column

f <- function(y){
  last = !duplicated(y$period, fromLast=TRUE)
  last_ind = which(last == TRUE)
  return(seq(0, y$period[last_ind]))
}

#' For_period Feature Function
#'
#' This function get the data.table with period and id columns and generate the for_period feature
#' @param x The data.table with id and period columns
#' for_period_func()

for_period_func <- function(x){
  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  period <- id <- for_period <- NULL

  x_new = x[rep(1:.N, period+1), list(id, period)]
  x_new[, for_period := f(.BY), by=list(id, period)]
  return(x_new[, for_period])
}

#' Weight Calculation Function
#'
#' This function performs the calculation for weight of the data
#' @param sw_data A data.table
#' @param cov_switchn List of covariates to be used in logistic model for switching probabilities for numerator model
#' @param model_switchn List of models (functions) to use the covariates from cov_switchn
#' @param class_switchn Class variables used in logistic model for nominator model
#' @param cov_switchd List of covariates to be used in logistic model for switching probabilities for denominator model
#' @param model_switchd List of models (functions) to use the covariates from cov_switchd
#' @param class_switchd Class variables used in logistic model for denominator model
#' @param eligible_wts_0 Eligibility criteria used in weights for model condition Am1 = 0
#' @param eligible_wts_1 Eligibility criteria used in weights for model condition Am1 = 1
#' @param cense Censoring variable
#' @param pool_cense Pool the numerator and denominator models (0: split models by previous treatment Am1 = 0 and Am1 = 1 as in treatment models and 1: pool all observations together into a single numerator and denominator model) Defaults to 0
#' @param cov_censed List of covariates to be used in logistic model for censoring weights in denominator model
#' @param model_censed List of models (functions) to use the covariates from cov_censed
#' @param class_censed Class variables used in censoring logistic regression in denominator model
#' @param cov_censen List of covariates to be used in logistic model for censoring weights in numerator model
#' @param model_censen List of models (functions) to use the covariates from cov_censen
#' @param class_censen Class variables used in censoring logistic regression in numerator model
#' @param include_regime_length If defined as 1 a new variable (time_on_regime) is added to dataset - This variable stores the duration of time that the patient has been on the current treatment value
#' @param numCores Number of cores for parallel programming
#' @param save_dir Directory to save tidy weight model summaries in as 'weight_models.rda'
#' @param quiet Don't print progress messages.
#'
weight_func <- function(sw_data, cov_switchn=NA, model_switchn=NA,
                        class_switchn=NA, cov_switchd=NA,
                        model_switchd=NA, class_switchd=NA,
                        eligible_wts_0=NA, eligible_wts_1=NA,
                        cense=NA, pool_cense=0, cov_censed=NA,
                        model_censed=NA, class_censed=NA,
                        cov_censen=NA, model_censen=NA, class_censen=NA,
                        include_regime_length=0,
                        numCores=NA, save_dir, quiet = FALSE){

  # Dummy variables used in data.table calls declared to prevent package check NOTES:
  eligible0 <- eligible1 <- id <- period <- eligible0.y <- eligible1.y <- am_1 <-
    treatment <- wt <- wtC <- p0_n <- p0_d <- p1_n <- p1_d <- pC_n0 <- pC_d0 <-
    pC_n1 <- pC_d1 <- pC_n <- pC_d <- NULL


  if(include_regime_length == 1){
    model_switchd <- c(model_switchd[!is.na(model_switchd)], c("time_on_regime", "time_on_regime2"))
    model_switchn <- c(model_switchn[!is.na(model_switchn)], c("time_on_regime", "time_on_regime2"))
  }

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Switching weights --------------------
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # ------------------- eligible0 == 1 --------------------
  # --------------- denominator ------------------
  if(any(!is.na(model_switchd))){
    len_d = length(model_switchd)
    regformd <- paste(
      paste("treatment", "~"),
      paste(
        paste(model_switchd, collapse="+"),
        sep="+"
      )
    )
  }else{
    len_d = 0
    regformd <- paste(
      paste("treatment", "~"),
      "1"
    )
  }

  if(any(!is.na(model_switchn))){
    len_n = length(model_switchn)
    regformn <- paste(
      paste("treatment", "~"),
      paste(
        paste(model_switchn, collapse="+"),
        sep="+"
      )
    )
  }else{
    len_n = 0
    regformn <- paste(
      paste("treatment", "~"),
      "1"
    )
  }

  # Fit the models for the weights in the four scenarios
  weight_models <- list()
  # ------------------- eligible0 == 1 --------------------
  # --------------- denominator ------------------
  h_quiet_print(quiet, "P(treatment=1 | treatment=0) for denominator")

  model1 <- weight_lr(
    sw_data[if(any(!is.na(eligible_wts_0))) (eligible0 == 1 & eligible_wts_0 == 1) else eligible0 == 1],
    regformd,
    class_switchd)

  h_quiet_print(quiet, summary(model1))
  switch_d0 = data.table(p0_d = model1$fitted.values,
                         eligible0 = unlist(model1$data$eligible0),
                         id = model1$data[, id],
                         period = model1$data[, period])

  model1$method <- "glm.fit" #TODO remove when bug is fixed in broom
  weight_models$switch_d0 <- broom::tidy(model1)
  weight_models$switch_d0_statistics <- broom::glance(model1)
  rm(model1)

  # -------------- numerator --------------------

  h_quiet_print(quiet, "P(treatment=1 | treatment=0) for numerator")

  model2 <- weight_lr(
    sw_data[if(any(!is.na(eligible_wts_0))) (eligible0 == 1 & eligible_wts_0 == 1) else eligible0 == 1],
    regformn,
    class_switchn)

  h_quiet_print(quiet, summary(model2))
  switch_n0 = data.table(p0_n = model2$fitted.values,
                         eligible0 = unlist(model2$data$eligible0),
                         id = model2$data[, id],
                         period = model2$data[, period])

  model2$method <- "glm.fit" #TODO remove when bug is fixed in broom
  weight_models$switch_n0 <- broom::tidy(model2)
  weight_models$switch_n0_statistics <- broom::glance(model2)
  rm(model2)

  # ------------------- eligible1 == 1 --------------------
  # --------------- denominator ------------------
  h_quiet_print(quiet, "P(treatment=1 | treatment=1) for denominator")
  model3 <- weight_lr(
    sw_data[if(any(!is.na(eligible_wts_1))) (eligible1 == 1 & eligible_wts_1 == 1) else eligible1 == 1],
    regformd,
    class_switchd)

  h_quiet_print(quiet, summary(model3))
  switch_d1 = data.table(p1_d = model3$fitted.values,
                         eligible1 = unlist(model3$data$eligible1),
                         id = model3$data[, id],
                         period = model3$data[, period])

  model3$method <- "glm.fit" #TODO remove when bug is fixed in broom
  weight_models$switch_d1 <- broom::tidy(model3)
  weight_models$switch_statistics <- broom::glance(model3)
  rm(model3)

  # -------------------- numerator ---------------------------
  h_quiet_print(quiet, "P(treatment=1 | treatment=1) for numerator")
  model4 <- weight_lr(
    sw_data[if(any(!is.na(eligible_wts_1))) (eligible1 == 1 & eligible_wts_1 == 1) else eligible1 == 1],
    regformn,
    class_switchn)

  h_quiet_print(quiet, summary(model4))
  switch_n1 = data.table(p1_n = model4$fitted.values,
                         eligible1 = unlist(model4$data$eligible1),
                         id = model4$data[, id],
                         period = model4$data[, period])

  model4$method <- "glm.fit"  #TODO remove when bug is fixed in broom
  weight_models$switch_n1 <- broom::tidy(model4)
  weight_models$switch_n1_statistics <- broom::glance(model4)

  rm(model4)


  # -------------- Combine results --------------------
  # CCS: Output only
  #if(!missing(save_dir)){
  #  save(weight_models, file = file.path(save_dir,"weight_models.rda"))
  #}
  #rm(weight_models)

  switch_0 = switch_d0[switch_n0, on = list(id=id, period=period,
                                         eligible0=eligible0)]
  switch_1 = switch_d1[switch_n1, on = list(id=id, period=period,
                                         eligible1=eligible1)]

  rm(switch_d0, switch_d1, switch_n0, switch_n1)

  new_data = Reduce(function(x,y) merge(x, y,
                                        by = c("id", "period"),
                                        all = TRUE),
                    list(sw_data, switch_1, switch_0))

  rm(switch_1, switch_0)

  #TODO Can we remove sw_data here?

  new_data[, eligible0.y := NULL]
  new_data[, eligible1.y := NULL]
  setnames(new_data, c("eligible0.x", "eligible1.x"),
           c("eligible0", "eligible1"))


  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Censoring weights --------------------
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cens_models <- list()

  if(!is.na(cense)){
    if(any(!is.na(model_censed))){
      regformd <- paste(
        paste("1", "-"),
        paste(eval(cense), "~"),
        paste(
          paste(model_censed, collapse="+"),
          sep="+"
        )
      )
    }else{
      regformd <- paste(
        paste("1", "-"),
        paste(eval(cense), "~"),
        "1"
      )
    }
    if(any(!is.na(model_censen))){
      regformn <- paste(
        paste("1", "-"),
        paste(eval(cense), "~"),
        paste(
          paste(model_censen, collapse="+"),
          sep="+"
        )
      )
    }else{
      regformn <- paste(
        paste("1", "-"),
        paste(eval(cense), "~"),
        "1"
      )
    }

    if(pool_cense == 1){
      # -------------------- denominator -------------------------
      h_quiet_print(quiet, "Model for P(cense = 0 |  X ) for denominator")
      # -----------------------------------------------------------
      model1.cense <- weight_lr(new_data, regformd, class_censed)
      h_quiet_print(quiet, summary(model1.cense))
      cense_d0 = data.table( pC_d = model1.cense$fitted.values,
                             id = model1.cense$data[, id],
                             period = model1.cense$data[, period])

      model1.cense$method <- "glm.fit"  #TODO remove when bug is fixed in broom
      cens_models$cens_pool_d <- broom::tidy(model1.cense)
      cens_models$cens_pool_d_statistics <- broom::glance(model1.cense)

      rm(model1.cense)

      # --------------------- numerator ---------------------------
      h_quiet_print(quiet, "Model for P(cense = 0 |  X ) for numerator")
      # ---------------------------------------------------------
      model2.cense = weight_lr(new_data, regformn, class_censen)
      h_quiet_print(quiet, summary(model2.cense))
      cense_n0 = data.table( pC_n = model2.cense$fitted.values,
                             id = model2.cense$data[, id],
                             period = model2.cense$data[, period])

      model2.cense$method <- "glm.fit"  #TODO remove when bug is fixed in broom
      cens_models$cens_pool_n <- broom::tidy(model2.cense)
      cens_models$cens_pool_n_statistics <- broom::glance(model2.cense)

      rm(model2.cense)
      new_data = Reduce(function(x,y) merge(x, y,
                                            by = c("id", "period"),
                                            all.x = TRUE, all.y = TRUE),
                        list(new_data, cense_d0, cense_n0))
      rm(cense_d0, cense_n0)

    }else{ # when pool_cense != 1

      # ---------------------- denominator -----------------------
      h_quiet_print(quiet, "Model for P(cense = 0 |  X, Am1=0) for denominator")
      # ---------------------- eligible0 ---------------------------

      model1.cense <- weight_lr(new_data[eligible0 == 1], regformd, class_censed)
      h_quiet_print(quiet, summary(model1.cense))
      cense_d0 = data.table( pC_d0 = model1.cense$fitted.values,
                             id = model1.cense$data[, id],
                             period = model1.cense$data[, period])

      model1.cense$method <- "glm.fit"  #TODO remove when bug is fixed in broom
      cens_models$cens_d0 <- broom::tidy(model1.cense)
      cens_models$cens_d0_statistics <- broom::glance(model1.cense)

      rm(model1.cense)
      # -------------------------- numerator ----------------------
      h_quiet_print(quiet, "Model for P(cense = 0 |  X, Am1=0) for numerator")
      #--------------------------- eligible0 -----------------------
      model2.cense <- weight_lr(new_data[eligible0 == 1], regformn, class_censen)
      h_quiet_print(quiet, summary(model2.cense))
      cense_n0 = data.table( pC_n0=model2.cense$fitted.values,
                             id = model2.cense$data[, id],
                             period = model2.cense$data[, period])

      model2.cense$method <- "glm.fit"  #TODO remove when bug is fixed in broom
      cens_models$cens_n0 <- broom::tidy(model2.cense)
      cens_models$cens_n0_statistics <- broom::glance(model2.cense)

      rm(model2.cense)
      # ------------------------- denomirator ---------------------
      h_quiet_print(quiet, "Model for P(cense = 0 |  X, Am1=1) for denominator")
      # ------------------------ eligible1 -------------------------
      model3.cense <- weight_lr(new_data[eligible1 == 1], regformd, class_censed)
      h_quiet_print(quiet, summary(model3.cense))
      cense_d1 = data.table( pC_d1=model3.cense$fitted.values,
                             id = model3.cense$data[, id],
                             period = model3.cense$data[, period])

      model3.cense$method <- "glm.fit"  #TODO remove when bug is fixed in broom
      cens_models$cens_d1 <- broom::tidy(model3.cense)
      cens_models$cens_d1_statistics <- broom::glance(model3.cense)

      rm(model3.cense)
      # ------------------------ numerator -------------------------
      h_quiet_print(quiet, "Model for P(cense = 0 |  X, Am1=1) for numerator")
      # ------------------------- eligible1 -----------------------
      model4.cense <- weight_lr(new_data[eligible1 == 1], regformn, class_censen)
      h_quiet_print(quiet, summary(model4.cense))
      cense_n1 = data.frame( pC_n1 = model4.cense$fitted.values,
                             id = model4.cense$data[, id],
                             period = model4.cense$data[, period])

      model4.cense$method <- "glm.fit"  #TODO remove when bug is fixed in broom
      cens_models$cens_n1 <- broom::tidy(model4.cense)
      cens_models$cens_n1_statistics <- broom::glance(model4.cense)

      rm(model4.cense)

      # combine ------------------------------
      if(!missing(save_dir)){
        save(cens_models, file = file.path(save_dir,"cens_models.rda"))
      }
      rm(cens_models)

      cense_0 = cense_d0[cense_n0, on = list(id=id, period=period)]
      cense_1 = cense_d1[cense_n1, on = list(id=id, period=period)]
      rm(cense_n1, cense_d1, cense_n0, cense_d0)

      new_data = Reduce(function(x,y) merge(x, y,
                                            by = c("id", "period"),
                                            all.x = TRUE, all.y = TRUE),
                        list(new_data, cense_0, cense_1))
      rm(cense_0, cense_1)
    }
  }
  # wt and wtC calculation
  if(any(!is.na(eligible_wts_0))){
    new_data[(am_1 == 0 & eligible_wts_0 == 1 & treatment == 0 & !is.na(p0_n) & !is.na(p0_d)),
             wt := (1.0-p0_n)/(1.0-p0_d)]
    new_data[(am_1 == 0 & eligible_wts_0 == 1 & treatment == 1 & !is.na(p0_n) & !is.na(p0_d)),
             wt := p0_n/p0_d]
    new_data[(am_1 == 0 & eligible_wts_0 == 0), wt := 1.0]
  }else{
    new_data[(am_1 == 0 & treatment == 0 & !is.na(p0_n) & !is.na(p0_d)),
             wt := (1.0-p0_n)/(1.0-p0_d)]
    new_data[(am_1 == 0 & treatment == 1 & !is.na(p0_n) & !is.na(p0_d)),
             wt := p0_n/p0_d]
  }
  if(any(!is.na(eligible_wts_1))){
    new_data[(am_1 == 1 & eligible_wts_1 == 1 &treatment == 0 & !is.na(p1_n) & !is.na(p1_d)),
             wt := (1.0-p1_n)/(1.0-p1_d)]
    new_data[(am_1 == 1 & eligible_wts_1 == 1 & treatment == 1 & !is.na(p1_n) & !is.na(p1_d)),
             wt := p1_n/p1_d]
    new_data[(am_1 == 1 & eligible_wts_1 == 0), wt := 1.0]
  }else{
    new_data[(am_1 == 1 & treatment == 0 & !is.na(p1_n) & !is.na(p1_d)),
             wt := (1.0-p1_n)/(1.0-p1_d)]
    new_data[(am_1 == 1 & treatment == 1 & !is.na(p1_n) & !is.na(p1_d)),
             wt := p1_n/p1_d]
  }

  if(is.na(cense)){
    new_data[, wtC := 1.0]
  }else{
    #new_data[, pC_d := as.numeric(NA)]
    #new_data[, pC_n := as.numeric(NA)]
    if(pool_cense == 0){
      new_data[am_1 == 0, ':='(pC_n=pC_n0, pC_d=pC_d0)]
      new_data[am_1 == 1, ':='(pC_n=pC_n1, pC_d=pC_d1)]
    }
    new_data[is.na(pC_d), pC_d := 1]
    new_data[is.na(pC_n), pC_n := 1]
    new_data[, wtC := pC_n/pC_d]
  }
  new_data[, wt := wt * wtC]

  # CCS: No need to copy to sw_data
  #sw_data <- new_data
  #rm(new_data)
  #gc()
  #return(sw_data)
  return(new_data)
}

