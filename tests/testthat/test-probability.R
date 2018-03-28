context("Tidy probabilities")

library(dplyr)
library(magrittr)

d <- data_frame(prob = runif(100),
                log_prob = log(prob),
                x = rep(c('a', 'b'), each=50),
                y = rep(c('c', 'd', 'e', 'f'), times=25))

test_that("marginalize sums (log) probabilities", {
  d %>% marginalize('prob') %$% expect_equal(prob, sum(d$prob))
  d %>% marginalize_log('log_prob') %>%
    transmute(prob=exp(log_prob)) %$%
    expect_equal(prob, marginalize(d, 'prob')$prob)
})

test_that("marginalize respects grouping", {

  d %>% group_by(x) %>% marginalize('prob') %>% {
    expect_equal(nrow(.), 2)
    expect_named(., c('prob', 'x'), ignore.order=TRUE)
    dd <- d %>% group_by(x) %>% summarize(prob=sum(prob))
    expect_equal(., dd)
  }

  d %>% group_by(y) %>% marginalize('prob') %>% {
    expect_equal(nrow(.), 4)
    expect_named(., c('prob', 'y'), ignore.order=TRUE)
    dd <- d %>% group_by(y) %>% summarize(prob=sum(prob))
    expect_equal(., dd)
  }

  d %>% group_by(x, y) %>% marginalize('prob') %>% {
    expect_equal(nrow(.), 8)
    expect_named(., c('prob', 'x', 'y'), ignore.order=TRUE)
    dd <- d %>% group_by(x,y) %>% summarize(prob=sum(prob))
    expect_equal(., dd)
  }

  d %>% group_by(x) %>% marginalize_log('log_prob') %>% {
    expect_equal(nrow(.), 2)
    expect_named(., c('log_prob', 'x'), ignore.order=TRUE)
    dd <- d %>% group_by(x) %>% summarize(log_prob=log_sum_exp(log_prob))
    expect_equal(., dd)
  }

  d %>% group_by(y) %>% marginalize_log('log_prob') %>% {
    expect_equal(nrow(.), 4)
    expect_named(., c('log_prob', 'y'), ignore.order=TRUE)
    dd <- d %>% group_by(y) %>% summarize(log_prob=log_sum_exp(log_prob))
    expect_equal(., dd)
  }

  d %>% group_by(x, y) %>% marginalize_log('log_prob') %>% {
    expect_equal(nrow(.), 8)
    expect_named(., c('log_prob', 'x', 'y'), ignore.order=TRUE)
    dd <- d %>% group_by(x,y) %>% summarize(log_prob=log_sum_exp(log_prob))
    expect_equal(., dd)
  }


  d %>% group_by(x) %>% marginalize('prob') %>% {
    expect_equal(nrow(.), 2)
    expect_named(., c('prob', 'x'), ignore.order=TRUE)
  }

  d %>% group_by(y) %>% marginalize('prob') %>% {
    expect_equal(nrow(.), 4)
    expect_named(., c('prob', 'y'), ignore.order=TRUE)
  }

  d %>% group_by(x, y) %>% marginalize('prob') %>% {
    expect_equal(nrow(.), 8)
    expect_named(., c('prob', 'x', 'y'), ignore.order=TRUE)
  }

  d %>% group_by(x) %>% marginalize_log('log_prob') %>% {
    expect_equal(nrow(.), 2)
    expect_named(., c('log_prob', 'x'), ignore.order=TRUE)
  }

  

})

d %>% group_by(x) %>% aggregate_lhood('prob', 'x')
