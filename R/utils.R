
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

#' Select groups of grouped tbl
#'
#' Useful for examining or testing one example group
#'
#' @param data
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
#' @param stats Named vector or list of function(d,i) that take data and indices
#' and return a scalar statistic.
#' @param R Number of bootstrap samples (default: 1000)
#' @param ci_level Percentage for CI (default 0.95) or lower/upper quantiles of CI.
#' @param h0 Optional null value of statistics for p value computation
#' @param na_rm Whether to remove NAs in quantile calculation
#' @param ... Additional arguments passed to boot.
#' 
#' @export
boot_ci <- function(data, stats, R=1000, ci_level=0.95, h0 = NA,
                    na_rm = TRUE, ...) {

  if (!is.function(stats)) {
    stats <- function(d,i) {
      sapply(stats, function(f) f(d,i))
    }
  }

  booted <- boot::boot(data, stats, R=R, ...)

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

  if (!is.na(h0)) {
    boot_p <- sapply(1:length(h0), function(i) mean(booted$t[,i] < h0[i]))
    ## smooth with one pseudo-observation.
    boot_p <- boot_p * (R / (R+1)) + 0.5 / (R+1)
    ## two-tailed
    boot_p <- ifelse(boot_p > 0.5,
                     2*(1 - boot_p),
                     2*boot_p)
    d$boot_p <- boot_p
  }

  return(d)

}

