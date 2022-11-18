test_that("weight_func works as expected", {
  data <- readRDS(test_path("data/pre_weight_func.rds"))
  save_dir <- withr::local_tempdir(pattern = "weights", tempdir(TRUE))
  expect_true(dir.exists(save_dir))

  expect_snapshot(
    result <- weight_func(
      sw_data = data,
      switch_n_cov = ~1,
      switch_d_cov = ~ X1 + X2,
      cense = "C",
      pool_cense = 0,
      cense_d_cov = ~ X1 + X2 + X3 + X4 + age_s,
      cense_n_cov = ~ X3 + X4,
      save_weight_models = FALSE,
      save_dir = save_dir,
      glm_function = "parglm"
    )
  )

  expect_names(colnames(result$data), must.include = c(
    "p0_d", "p0_n", "p1_d", "p1_n", "pC_d0", "pC_n0",
    "pC_d1", "pC_n1", "wt", "pC_n", "pC_d", "wtC"
  ))
  expect_equal(sum(result$data$wt), 5124.4538)
  expect_equal(sum(result$data$wtC), 5127.7397)

  expect_list(result$censor_models, types = "data.frame", any.missing = FALSE, len = 8)
  expect_equal(
    result$censor_models$cens_d0$estimate,
    c(0.900038686916255, 0.588866421245376, -0.464693730180448, 0.32342303175603, -0.25226496458668, 0.9730384163288)
  )

  expect_list(result$switch_models, types = "data.frame", any.missing = FALSE, len = 8)
  expect_equal(
    result$switch_models$switch_d0$estimate,
    c(-0.52632937, 0.35856345, 0.42935005)
  )
})




test_that("weight_func works saves model objects", {
  data <- readRDS(test_path("data/pre_weight_func.rds"))

  save_dir <- withr::local_tempdir(pattern = "weights", tempdir(TRUE))
  expect_true(dir.exists(save_dir))

  result <- weight_func(
    sw_data = data,
    switch_n_cov = ~1,
    switch_d_cov = ~ X1 + X2,
    cense = "C",
    pool_cense = 0,
    cense_d_cov = ~ X1 + X2 + X3 + X4 + age_s,
    cense_n_cov = ~ X3 + X4,
    save_weight_models = TRUE,
    save_dir = save_dir,
    quiet = TRUE
  )

  expect_file_exists(file.path(
    save_dir,
    c(
      "cense_model_d0.rds", "cense_model_d1.rds", "cense_model_n0.rds",
      "cense_model_n1.rds", "weight_model_switch_d0.rds", "weight_model_switch_d1.rds",
      "weight_model_switch_n0.rds", "weight_model_switch_n1.rds"
    )
  ))

  cense_d0 <- readRDS(file.path(save_dir, "cense_model_d0.rds"))
  expect_class(cense_d0, "glm")
  expect_equal(
    coef(cense_d0),
    c(
      `(Intercept)` = 0.900038686916255, X1 = 0.588866421245376,
      X2 = -0.464693730180448, X3 = 0.323423031756038, X4 = -0.252264964586683,
      age_s = 0.973038416328826
    )
  )

  expect_data_frame(cense_d0$data, nrows = 2849, ncols = 24)

  switch_d1 <- readRDS(file.path(save_dir, "weight_model_switch_d1.rds"))
  expect_class(switch_d1, "glm")
  expect_equal(
    coef(switch_d1),
    c(`(Intercept)` = 0.897948772340035, X1 = 0.343105319788626, X2 = 0.44842891370765)
  )

  expect_data_frame(switch_d1$data, nrows = 2154, ncols = 20)
})
