#' Paste with different separator for last pair
#'
#' @param v vector to paste
#' @param sep separator for all but last
#' @param last_sep last separator
#'
#' @export
paste_with_last <- function(v, sep, last_sep) {
  paste0(paste(head(v, length(v)-1), collapse=sep),
         last_sep,
         tail(v, 1))
}

#' @describeIn paste_with_last final separator of sep plus "and"
#' @export
paste_and <- function(v, sep=', ') {
  paste_with_last(v, sep, paste0(sep, 'and '))
}
