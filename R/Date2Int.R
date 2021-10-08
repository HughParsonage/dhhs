#' Convert a date to a standard integer
#' @description For efficient memory consumption
#'
#' @param x A date in \code{dd/mm/YYYY} format.
#'
#' @return
#' An integer, equivalent to \code{as.integer(as.Date(x, format))}
#' for the format matching dd/mm/YYYY.
#'
#' @note
#' Missing values are mapped to \code{NA_integer_} as would be expected,
#' but so too are dates not in the above format, including for example
#' 1/2/2000 (no leading zeroes).
#'
#'
#'
#' @export

ddmmyyyy2Int <- function(x) {
  if (inherits(x, "Date")) {
    return(as.integer(x))
  }
  .Call("CDate2Int", x, PACKAGE = packageName())
}


#' @rdname ddmmyyyy2Int
#' @export
yyyymmdd2Int <- function(x) {
  if (inherits(x, "Date")) {
    return(as.integer(x))
  }
  .Call("C_yyyy_mm_dd", x, PACKAGE = packageName())
}
