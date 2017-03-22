library(magrittr)

context("Bootstrapped CIs")

## manually centered
x <- rnorm(1000) %>% { . - mean(.) }

test_that("boot_ci with one stat", {
  b0 <- boot_ci(x, function(d,i) mean(d[i]))
  expect_equal(nrow(b0), 1)
})

mean_sd <- list(mean=function(d,i) mean(d[i]),
                sd=function(d,i) sd(d[i]))

test_that("boot_ci produces a data_frame with correct names", {
  b1 <- boot_ci(x, mean_sd)

  expect_is(b1, 'tbl_df')
  expect_named(b1, c('stat', 'observed', 'ci_lo', 'ci_high'))
  expect_equal(nrow(b1), 2)
  expect_equal(b1$stat, c('mean', 'sd'))
  expect_equal(b1$observed, c(mean(x), sd(x)))
})

test_that("length(h0) = 1 or length(stats)", {
  b2 <- boot_ci(x, mean_sd, h0=0)
  expect_named(b2, c('stat', 'observed', 'ci_lo', 'ci_high', 'boot_p'))

  b3 <- boot_ci(x, mean_sd, h0=c(0,0))
  
  expect_error(boot_ci(x, mean_sd, h0=c(0,0,0)))
})

test_that("h0s are assigned to the proper stat", {
  mean_plus <- function(n) function(d,i) mean(d[i])+n

  ## nulls are both wrong
  boot_ci(x, list(mean_plus(10), mean_plus(0)), h0=c(0, 10)) %$%
    expect_equal(boot_p, rep(1/1001, 2))

  ## nulls are both right
  boot_ci(x, list(mean_plus(10), mean_plus(0)), h0=c(10, 0)) %$%
    walk(boot_p, ~ expect_gt(., 0.9))

})
