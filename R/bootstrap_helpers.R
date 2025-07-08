lef_load_models <- function(object) {
  r <- list()
  # load weight model objects and extract model matrices and residuals
  r$switch_d0 <- readRDS(object@switch_weights@fitted$d0@summary$save_path$path)

  r$X_sw_d0 <- model.matrix(r$switch_d0)
  r$e_sw_d0 <- r$switch_d0$y - r$switch_d0$fitted.values


  r$switch_n0 <- readRDS(object@switch_weights@fitted$n0@summary$save_path$path)
  r$X_sw_n0 <- model.matrix(r$switch_n0)
  r$e_sw_n0 <- r$switch_n0$y - r$switch_n0$fitted.values


  r$switch_d1 <- readRDS(object@switch_weights@fitted$d1@summary$save_path$path)
  r$X_sw_d1 <- model.matrix(r$switch_d1)
  r$e_sw_d1 <- r$switch_d1$y - r$switch_d1$fitted.values


  r$switch_n1 <- readRDS(object@switch_weights@fitted$n1@summary$save_path$path)
  r$X_sw_n1 <- model.matrix(r$switch_n1)
  r$e_sw_n1 <- r$switch_n1$y - r$switch_n1$fitted.values


  if (object@censor_weights@pool_denominator) {
    r$cense_d <- readRDS(object@censor_weights@fitted$d@summary$save_path$path)

    r$X_c_d <- model.matrix(r$cense_d)
    r$e_c_d <- r$cense_d$y - r$cense_d$fitted.values
  } else {
    r$cense_d0 <- readRDS(object@censor_weights@fitted$d0@summary$save_path$path)

    r$X_c_d0 <- model.matrix(r$cense_d0)
    r$e_c_d0 <- r$cense_d0$y - r$cense_d0$fitted.values


    r$cense_d1 <- readRDS(object@censor_weights@fitted$d1@summary$save_path$path)

    r$X_c_d1 <- model.matrix(r$cense_d1)
    r$e_c_d1 <- r$cense_d1$y - r$cense_d1$fitted.values
  }

  if (object@censor_weights@pool_numerator) {
    r$cense_n <- readRDS(object@censor_weights@fitted$n@summary$save_path$path)

    r$X_c_n <- model.matrix(r$cense_n)
    r$e_c_n <- r$cense_n$y - r$cense_n$fitted.values
  } else {
    r$cense_n0 <- readRDS(object@censor_weights@fitted$n0@summary$save_path$path)

    r$X_c_n0 <- model.matrix(r$cense_n0)
    r$e_c_n0 <- r$cense_n0$y - r$cense_n0$fitted.values


    r$cense_n1 <- readRDS(object@censor_weights@fitted$n1@summary$save_path$path)

    r$X_c_n1 <- model.matrix(r$cense_n1)
    r$e_c_n1 <- r$cense_n1$y - r$cense_n1$fitted.values
  }
  r
}


lef_calc_coefs <- function(weights_table_boot, ...) {
  with(
    list(...),
    {
      r <- list()
      # Calculate the weight models' coefficient LEF approximates
      data_0 <- merge(weights_table_boot, switch_d0$data, by = "id", all.y = TRUE)
      data_1 <- merge(weights_table_boot, switch_d1$data, by = "id", all.y = TRUE)

      LEF_sw_d0_boot <- t(X_sw_d0) %*% (data_0$weight_boot * e_sw_d0)
      LEF_sw_n0_boot <- t(X_sw_n0) %*% (data_0$weight_boot * e_sw_n0)
      LEF_sw_d1_boot <- t(X_sw_d1) %*% (data_1$weight_boot * e_sw_d1)
      LEF_sw_n1_boot <- t(X_sw_n1) %*% (data_1$weight_boot * e_sw_n1)

      # Calculate \hat \beta(b)
      r$beta_sw_d0 <- switch_d0$coefficients + vcov(switch_d0) %*% LEF_sw_d0_boot
      r$beta_sw_n0 <- switch_n0$coefficients + vcov(switch_n0) %*% LEF_sw_n0_boot
      r$beta_sw_d1 <- switch_d1$coefficients + vcov(switch_d1) %*% LEF_sw_d1_boot
      r$beta_sw_n1 <- switch_n1$coefficients + vcov(switch_n1) %*% LEF_sw_n1_boot

      if (object@censor_weights@pool_denominator) {
        # Calculate the weight models' coefficient LEF approximates
        data <- merge(weights_table_boot, cense_d$data, by = "id", all.y = TRUE)
        LEF_c_d_boot <- t(X_c_d) %*% (data$weight_boot * e_c_d)

        # Calculate \hat \beta(b)
        r$beta_c_d <- cense_d$coefficients + vcov(cense_d) %*% LEF_c_d_boot
      } else {
        data_0 <- merge(weights_table_boot, cense_d0$data, by = "id", all.y = TRUE)
        data_1 <- merge(weights_table_boot, cense_d1$data, by = "id", all.y = TRUE)

        LEF_c_d0_boot <- t(X_c_d0) %*% (data_0$weight_boot * e_c_d0)
        LEF_c_d1_boot <- t(X_c_d1) %*% (data_1$weight_boot * e_c_d1)


        # Calculate \hat \beta(b)
        r$beta_c_d0 <- cense_d0$coefficients + vcov(cense_d0) %*% LEF_c_d0_boot
        r$beta_c_d1 <- cense_d1$coefficients + vcov(cense_d1) %*% LEF_c_d1_boot
      }

      if (object@censor_weights@pool_numerator) {
        # Calculate the weight models' coefficient LEF approximates
        data <- merge(weights_table_boot, cense_n$data, by = "id", all.y = TRUE)
        LEF_c_n_boot <- t(X_c_n) %*% (data$weight_boot * e_c_n)

        # Calculate \hat \beta(b)
        r$beta_c_n <- cense_n$coefficients + vcov(cense_n) %*% LEF_c_n_boot
      } else {
        data_0 <- merge(weights_table_boot, cense_n0$data, by = "id", all.y = TRUE)
        data_1 <- merge(weights_table_boot, cense_n1$data, by = "id", all.y = TRUE)

        LEF_c_n0_boot <- t(X_c_n0) %*% (data_0$weight_boot * e_c_n0)
        LEF_c_n1_boot <- t(X_c_n1) %*% (data_1$weight_boot * e_c_n1)


        # Calculate \hat \beta(b)
        r$beta_c_n0 <- cense_n0$coefficients + vcov(cense_n0) %*% LEF_c_n0_boot
        r$beta_c_n1 <- cense_n1$coefficients + vcov(cense_n1) %*% LEF_c_n1_boot
      }
      r
    }
  )
}
