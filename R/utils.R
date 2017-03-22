#' @import assertthat
NULL

#' Combine two factors, preserving level order
#'
#' @export
paste_factors <- function(f1, f2) {
  factor(paste(f1, f2),
         levels = paste(rep(levels(f1), each=nlevels(f2)),
                        rep(levels(f2), times=nlevels(f1))))
}

#' Standard error
#'
#' @export
se <- function(x) sd(x) / sqrt(length(x))

#' Format p-values in "stars" notation
#'
#' @param p p-value (vectorized)
#' @param approaching if FALSE (default), cut off at 0.05. if TRUE, use '.' for
#'   (0.05, 0.10] ('approaching significance')
#'
#' @export
p_val_to_stars <- function(p, approaching=FALSE) {
  ifelse(p <= 0.001, '***',
         ifelse(p <= 0.01, '**',
                ifelse(p <= 0.05, '*',
                       ifelse(approaching && p <= 0.10, '.', ''))))
}

#' Round up to specified ceilings
#'
#' @param x number to round
#' @param ceilings ceilings to round up to
#' @return the lowest ceiling that is greater than x
#' @export
ceil_to <- function(x, ceilings) {
  tmp <- ceilings[x <= ceilings]
  tmp[which.min(tmp)]
}

#' Format p-values in "less than" notation
#'
#' @param p p-value (vectorized)
#' @param cutoffs values to be compared to
#' @return formatted string of the form "p < %f", or "p = %.2f" if p is greater
#'   than the highest cutoff
#' @export
p_val_to_less_than <- function(p, cutoffs = c(0.05, 0.01, 0.001, 0.0001)) {
  format_cutoff <- function(.x) sprintf(paste0('p < %.', ceiling(-log10(.x)), 'f'), .x)

  map_chr(p, ~ ifelse(.x > max(cutoffs),
                      sprintf('p = %.2f', .x),
                      .x %>%
                        ceil_to(cutoffs) %>%
                        format_cutoff()))

}

#' Select groups of grouped tbl
#'
#' Useful for examining or testing one example group
#'
#' @param data Grouped data_frame.
#' @param groups Group numbers to select
#' @export
select_groups <- function(data, groups)
   data[sort(unlist(attr(data, "indices")[ groups ])) + 1, ]

#' Bootstrapped CIs
#'
#' Returns a data_frame with columns stat (names of provided stats), observed
#' (observed value of stats applied to data, t0 in boot), and ci_lo/ci_hi.
#'
#' @param data Data that's provided to `boot`
#' @param stats Function or named vector/list of statistics to compute
#'   bootstrapped CIs for.  Statistics should take data as their first argument,
#'   and indices as their second, and return a scalar statistic.
#' @param R Number of bootstrap samples (default: 1000)
#' @param ci_level Percentage for CI (default 0.95) or lower/upper quantiles of
#'   CI.
#' @param h0 Optional null value of statistics for p value computation
#' @param na_rm Whether to remove NAs in quantile calculation
#' @param ... Additional arguments passed to boot.
#' 
#' @export
boot_ci <- function(data, stats, R=1000, ci_level=0.95, h0 = NULL,
                    na_rm = TRUE, ...) {

  assert_that(is.null(h0) || length(h0) == 1 || length(h0) == length(stats))

  stats_f <- purrr::partial(purrr::invoke_map_dbl, stats, list(NULL))

  booted <- boot::boot(data, stats_f, R=R, ...)

  ci_levels <- if (length(ci_level)==1)
                 (1-ci_level)/2 * c(1, -1) + c(0,1)
               else
                 ci_levels

  cis <- apply(booted$t, 2, function(x) quantile(x, ci_levels, na.rm=na_rm))

  stat_names <- if (is.null(names(booted$t0))) 1:length(booted$t0) else names(booted$t0)

  d <- data_frame(stat = stat_names,
                  observed = booted$t0,
                  ci_lo = cis[1,],
                  ci_high = cis[2,])

  if (!is.null(h0)) {
    d$boot_p <- 
      booted$t %>%
      purrr::array_branch(2) %>%     # make list of columns of stat samples
      map2_dbl(h0, ~ mean(.x > .y)) %>%  # prop of samples where stat > h0
      map_dbl(~ (.x*R + 0.5)/(R+1)) %>% # smooth by 1 pseudo obs
      map_dbl(p_val_to_two_tail)
  }

  return(d)

}
