context("p-values")

p_vals <- c(0, 0.0001, 0.002, 0.02, 0.06, 0.2, 0.8)

test_that("p values are converted to stars correctly", {
  expect_equal(p_val_to_stars(p_vals),
               c('***', '***', '**', '*', '', '', ''))
  expect_equal(p_val_to_stars(p_vals, approaching=TRUE),
               c('***', '***', '**', '*', '.', '', ''))
})

test_that("p values are converted to 'p < ...' correctly", {
  p_val_to_less_than(p_vals) %>%
    expect_equal(c("p < 0.0001", "p < 0.0001", "p < 0.01", "p < 0.05",
                   "p = 0.06", "p = 0.20", "p = 0.80"))
})

test_that("Two-tailed conversion", {
  p_vals %>% p_val_to_two_tail() %>%
    expect_equal(c(0, 0.0002, 0.004, 0.04, 0.12, 0.4, 0.4))
})

test_that("p values outside 0 and 1 give errors", {
  walk(c(-Inf, -1, 2, Inf), ~ expect_error(p_val_to_stars(.)))
  walk(c(-Inf, -1, 2, Inf), ~ expect_error(p_val_to_two_tail(.)))
  walk(c(-Inf, -1, 2, Inf), ~ expect_error(p_val_to_less_than(.)))
})

