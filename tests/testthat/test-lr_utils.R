test_that("fit_glm works", {
  set.seed(1000)
  my_data <- data.frame(outcome = rbinom(100, 1, 0.5), x1 = runif(100), x2 = runif(100))
  expect_warning(
    results <- fit_glm(data = my_data, formula = outcome ~ x1 + x2, weights = runif(100)),
    "non-integer"
  )
  expect_class(results, "glm")
})

test_that("fit_glm works with parglm and defaults", {
  set.seed(1000)
  my_data <- data.frame(outcome = rbinom(100, 1, 0.5), x1 = runif(100), x2 = runif(100))
  expect_warning(
    results <- fit_glm(data = my_data, formula = outcome ~ x1 + x2, weights = runif(100), glm_function = "parglm"),
    "but no `nthreads`"
  )
  expect_class(results, "glm")
  expect_equal(
    results$control,
    list(epsilon = 1e-08, maxit = 25, trace = FALSE, nthreads = 4, block_size = NULL, method = "FAST")
  )
  expect_equal(results$coefficients, c(`(Intercept)` = 0.3835088968, x1 = 0.1197949945, x2 = -0.3195365753))
})

test_that("fit_glm works with parglm", {
  set.seed(1000)
  my_data <- data.frame(outcome = rbinom(100, 1, 0.5), x1 = runif(100), x2 = runif(100))
  control <- parglm::parglm.control(method = "LAPACK", nthreads = 2L)
  results <- fit_glm(data = my_data, formula = outcome ~ x1 + x2, glm_function = "parglm", control = control)
  expect_class(results, "glm")
  expect_equal(results$coefficients, c(`(Intercept)` = 0.406722705029, x1 = -0.0318483640784, x2 = -0.520410290023))
  expect_equal(results$control, control)
  expect_equal(results$formula, outcome ~ x1 + x2)
})
