#' Convert a date to a standard integer
#' @description For efficient memory consumption
#'
#' @param x A character vector, represeting a date
#'  in \code{dd/mm/YYYY} format (\code{ddmmyyyy2Int}) or
#' \code{YYYY-mm-dd} format (\code{yyyymmdd2Int}), or a date time (for \code{yyyymmdd_HHMMSS_UTC}).
#'
#' @return
#' An integer, equivalent to \code{as.integer(as.Date(x, format))}
#' for the format matching dd/mm/YYYY.
#'
#' For \code{yyyymmdd_HHMMSS_UTC}, an integer vector the number of seconds
#' since the epoch.
#'
#' @note
#' Missing values are mapped to \code{NA_integer_} as would be expected,
#' but so too are dates not in the above format, including for example
#' 1/2/2000 (no leading zeroes).
#'
#' @examples
#' ddmmyyyy2Int("02/02/2020")
#' yyyymmdd_HHMMSS_UTC("2021-05-05 00:55:55")
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

#' @rdname ddmmyyyy2Int
#' @export
yyyymmdd_HHMMSS_UTC <- function(x) {
  if (inherits(x, "POSIXct")) {
    return(as.integer(x))
  }
  .Call("C_yyyy_mm_dd_HHMMSS_UTC", x, PACKAGE = packageName())
}

check_startsWith202 <- function(x) {
  .Call("Ccheck_startsWith202", x, PACKAGE = packageName())
}




