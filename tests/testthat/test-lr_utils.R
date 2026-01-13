test_that("fit_glm works", {
  set.seed(1000)
  my_data <- data.frame(outcome = rbinom(100, 1, 0.5), x1 = runif(100), x2 = runif(100))
  expect_warning(
    results <- fit_glm(data = my_data, formula = outcome ~ x1 + x2, weights = runif(100)),
    "non-integer"
  )
  expect_class(results, "glm")
})


test_that("p99_weight works as expected", {
  object <- 1:1000
  result <- p99_weight(object)

  expected <- 1:1000
  expected[1:10] <- 10
  expected[990:1000] <- 990

  expect_equal(result, expected)
})

test_that("limit_weight works as expected", {
  object <- 1:10
  result <- limit_weight(object, 3, 7)
  expected <- c(3, 3, 3, 4, 5, 6, 7, 7, 7, 7)
  expect_equal(result, expected)
})
