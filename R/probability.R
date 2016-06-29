#' @import dplyr
NULL

#' Numerically stable sum of logged numbers
#'
#' If any element of x is \code{Inf}, returns \code{Inf}. If all elements are
#' \code{-Inf}, returns all \code{-Inf}.
#'
#' @param x a numeric vector of logged values.
#' @return log of the sum/mean of the exponentiated entries in x.
#' @export
log_sum_exp <- function(x) {
  if (any(x == Inf)) return(Inf)
  if (all(x == -Inf)) return(-Inf)
  max_x <- max(x)
  log(sum(exp(x - max_x))) + max_x
}

#' @describeIn log_sum_exp Numerically stable mean of logged numbers
#' @export
log_mean_exp <- function(x) log_sum_exp(x) - log(length(x))


#' Convert joint to marginal (log) probabilities
#'
#' @param joint data frame with (possibly un-normalized) joint probabilities
#' @param prob name of column with (log) probability values to normalize
#' @param ... additional arguments are columns used to define additional
#'   groupings for marginalization.
#' @return a data frame with columsn for \code{marginal_vars} and any
#'   pre-existing grouping, plus marginal log likelihood.
#'
#' @export
marginalize <- function(joint, prob, ...) {
  assert_that(is.numeric(joint[[prob]]))
  walk(c(prob, list(...)),
       ~ assert_that(has_name(joint, .)))

  dots <-
    lazyeval::interp(~sum(var), var=as.name(prob)) %>%
    list() %>%
    purrr::set_names(prob)

  joint %>%
    group_by_(..., add=TRUE) %>%
    summarise_(.dots = dots)

}

#' @describeIn marginalize Marginalize log probability
#' @export
marginalize_log <- function(joint, log_prob, ...) {
  assert_that(is.numeric(joint[[log_prob]]))
  walk(c(log_prob, list(...)),
       ~ assert_that(has_name(joint, .)))

  dots <-
    lazyeval::interp(~log_sum_exp(var), var=as.name(log_prob)) %>%
    list() %>%
    purrr::set_names(log_prob)

  joint %>%
    group_by_(..., add=TRUE) %>%
    summarise_(.dots = dots)
}

#' Aggregate observations' likelihoods into single posterior
#'
#' @param lhoods data frame with one row per observation and hypothesis
#'   combination, and column \code{log_lhood} with log-likelihood of
#'   observations given hypothesis
#' @param prob name of column with (log) likelihood values to aggregate
#' @param ... additional arguments are used to define additional groups to
#'   aggregate likelihood within
#' @return a data frame with one row per group (plus additional grouping vars)
#'   and total likelihood in \code{prob}
#'
#' @export
aggregate_lhood <- function(lhoods, prob, ...) {
  walk(c(prob, ...),
       ~ assert_that(has_name(lhoods, .)))
  assert_that(is.numeric(lhoods[[prob]]))

  dots <-
    lazyeval::interp(~ exp(sum(log(var))), var=as.name(prob)) %>%
    list() %>%
    purrr::set_names(prob)

  lhoods %>%
    group_by_(..., add=TRUE) %>%
    summarise_(.dots = dots)
}

#' @describeIn aggregate_lhood Aggregate observations' log-likelihood
#' @export
aggregate_log_lhood <- function(lhoods, log_prob, ...) {
  walk(c(log_prob, ...), ~ assert_that(has_name(lhoods, .)))
  assert_that(is.numeric(lhoods[[log_prob]]))

  dots <-
    lazyeval::interp(~ sum(var), var=as.name(log_prob)) %>%
    list() %>%
    purrr::set_names(log_prob)

  lhoods %>%
    group_by_(..., add=TRUE) %>%
    summarise_(.dots = dots)
}

#' Normalize posterior
#'
#' @param d data frame, grouping will be preserved
#' @param prob_var quoted name of column with probability to normalize
#' @return A data frame with a column \code{posterior} that sums to 1 within
#'   each group in \code{d}. Additionally, \code{log_posterior} and
#'   \code{posterior_choice} columns are created that have the log-posterior and
#'   TRUE for the maximum within each group/FALSE elsewhere, respectively.
#' @export
normalize_probability <- function(d, prob_var) {
  assert_that(is.numeric(d[[prob_var]]))
  d %>%
    mutate_(temp__ = lazyeval::interp(~log(var), var=as.name(prob_var))) %>%
    normalize_log_probability('temp__') %>%
    mutate_(temp__ = ~NULL)
}

#' @describeIn normalize_probability Operate on log-probability
#' @export
normalize_log_probability <- function(d, prob_var) {
  assert_that(is.numeric(d[[prob_var]]))
  mutate_(d,
          log_posterior = lazyeval::interp(~var-log_sum_exp(var),
                                           var = as.name(prob_var)),
          posterior = ~exp(log_posterior),
          posterior_choice = ~posterior == max(posterior))
}
  
