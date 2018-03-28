context("log sum/mean exp")

x <- runif(100, -100, 100)

test_that("log_sum_exp", {
  expect_equal(log_sum_exp(x), log(sum(exp(x))))
})

test_that("log_sum_exp with Inf", {
  expect_equal(log_sum_exp(c(x, Inf)), Inf)
  expect_equal(log_sum_exp(c(x, -Inf)), log_sum_exp(x))
  expect_equal(log_sum_exp(rep(-Inf, 100)), -Inf)
})

test_that("log_mean_exp", {
  expect_equal(log_mean_exp(x), log(mean(exp(x))))
})

test_that("log_mean_exp with Inf", {
  expect_equal(log_mean_exp(c(x, Inf)), Inf)
  expect_equal(log_mean_exp(c(x, -Inf)), log_sum_exp(x) - log(101))
  expect_equal(log_mean_exp(rep(-Inf, 100)), -Inf)
})
