context("Formatting p values")


test_that("correctly formats p < NNN with default cutoffs", {
  expect_equal(p_val_to_less_than(0.09), "p = 0.09")
  expect_equal(p_val_to_less_than(0.04), "p < 0.05")
  expect_equal(p_val_to_less_than(0.009), "p < 0.01")
  expect_equal(p_val_to_less_than(0.0009), "p < 0.001")
  expect_equal(p_val_to_less_than(0.00009), "p < 0.0001")
})


test_that("correctly formats p < NNN with custom cutoffs", {
  p_lt <- purrr::partial(p_val_to_less_than,
                         cutoffs = c(0.1, 0.09, 0.009, 0.0009))
  expect_equal(p_lt(0.099), "p < 0.1")
  expect_equal(p_lt(0.04), "p < 0.09")
  expect_equal(p_lt(0.008), "p < 0.009")
  expect_equal(p_lt(0.0008), "p < 0.0009")
})
