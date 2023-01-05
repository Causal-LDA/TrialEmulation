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
      glm_function = "parglm",
      control = parglm.control(nthreads = 2, method = "FAST")
    )
  )

  expect_names(colnames(result$data), must.include = c(
    "p0_d", "p0_n", "p1_d", "p1_n", "pC_d0", "pC_n0",
    "pC_d1", "pC_n1", "wt", "pC_n", "pC_d", "wtC"
  ))
  expect_equal(sum(result$data$wt), 5124.4538)
  expect_equal(sum(result$data$wtC), 5127.7397)

  expect_list(result$censor_models, types = "TE_weight_summary", any.missing = FALSE, len = 4)
  expect_equal(
    result$censor_models$cens_d0$summary$estimate,
    c(0.900038686916255, 0.588866421245376, -0.464693730180448, 0.32342303175603, -0.25226496458668, 0.9730384163288)
  )

  expect_list(result$switch_models, types = "TE_weight_summary", any.missing = FALSE, len = 4)
  expect_equal(
    result$switch_models$switch_d0$summary$estimate,
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


test_that("weight_func works time on regime", {
  data <- readRDS(test_path("data/pre_weight_func.rds"))

  result <- weight_func(
    sw_data = data,
    switch_n_cov = ~1,
    switch_d_cov = ~ X1 + X2,
    cense = "C",
    pool_cense = 0,
    cense_d_cov = ~ X1 + X2 + X3 + X4 + age_s,
    cense_n_cov = ~ X3 + X4,
    include_regime_length = TRUE,
    quiet = TRUE
  )
  expect_snapshot(for (i in result$switch_models) print(i))
  expect_snapshot(for (i in result$censor_models) print(i))
})


test_that("weight_func works with user specified time on regime", {
  data <- readRDS(test_path("data/pre_weight_func.rds"))

  result <- weight_func(
    sw_data = data,
    switch_n_cov = ~time_on_regime,
    switch_d_cov = ~ X1 + X2 + time_on_regime,
    include_regime_length = FALSE,
    quiet = TRUE
  )
  expect_snapshot(for (i in result$switch_models) print(i))
  expect_snapshot(for (i in result$censor_models) print(i))
})

test_that("weight_func works with pool_cense = 1", {
  data <- readRDS(test_path("data/pre_weight_func.rds"))

  result <- weight_func(
    sw_data = data,
    switch_n_cov = ~1,
    switch_d_cov = ~ X1 + X2,
    cense = "C",
    pool_cense = 1,
    cense_d_cov = ~ X1 + X2 + X3 + X4 + age_s,
    cense_n_cov = ~ X3 + X4,
    quiet = TRUE
  )
  expect_snapshot(lapply(result$switch_models, print))
  expect_snapshot(lapply(result$censor_models, print))
})


test_that("select_data_cols works as expected", {
  result <- select_data_cols(
    data = trial_example,
    formula_vars = c("nvarA", "nvarC"),
    where_var = "catvarA",
    eligible_wts_0 = NA,
    eligible_wts_1 = NA,
    cense = NA
  )
  expect_data_frame(
    result,
    nrows = 48400,
    ncols = 8
  )
  check_names(
    colnames(result),
    permutation.of = c(
      "id", "period", "outcome", "eligible",
      "treatment", "catvarA", "nvarA", "nvarC"
    )
  )
})

test_that("select_data_cols works as expected with non-default names", {
  result <- select_data_cols(
    data = readRDS(test_path("data/raw_data.rds")),
    id = "ID",
    period = "t",
    treatment = "A",
    outcome = "Y",
    eligible = "eligible",
    formula_vars = c("X1", "age"),
    where_var = "X3",
    eligible_wts_0 = NA,
    eligible_wts_1 = NA,
    cense = "C"
  )
  expect_data_frame(
    result,
    nrows = 4926,
    ncols = 9
  )
  expect_names(
    colnames(result),
    permutation.of = c(
      "id", "period", "outcome", "eligible",
      "treatment", "C", "X3", "X1", "age"
    )
  )
})

test_that("user can select period in select_data_cols", {
  result <- select_data_cols(
    data = readRDS(test_path("data/raw_data.rds")),
    id = "ID",
    period = "t",
    treatment = "A",
    outcome = "Y",
    eligible = "eligible",
    formula_vars = c("X1", "period"),
    where_var = "X3",
    eligible_wts_0 = NA,
    eligible_wts_1 = NA,
    cense = "C"
  )
  expect_names(
    colnames(result),
    permutation.of = c(
      "id", "period", "outcome", "eligible",
      "treatment", "C", "X3", "X1", "period"
    )
  )
})

test_that("select_data_cols allows derived variables in formula vars", {
  result <- select_data_cols(
    data = readRDS(test_path("data/raw_data.rds")),
    id = "ID",
    period = "t",
    treatment = "A",
    outcome = "Y",
    eligible = "eligible",
    formula_vars = c("X1", "time_on_regime"),
    where_var = "X3",
    eligible_wts_0 = NA,
    eligible_wts_1 = NA,
    cense = "C"
  )
  expect_names(
    colnames(result),
    permutation.of = c(
      "id", "period", "outcome", "eligible",
      "treatment", "C", "X3", "X1"
    )
  )
})
