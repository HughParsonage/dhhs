#' Encode a character vector prefixed with 3202
#' @param x,y \code{character(n)} Vectors to encode.
#' @param enc \code{integer(n)} Encoded vectors to decode.
#'
#' @return \code{encode_3202} returns an integer based on the digits following
#' the prefix \code{"3202"}. \code{decode_3202} is the reverse process.
#'
#' \code{names2int} takes two character vectors of equal length and returns
#' an integer based on the first two digits. \code{Lookup4} is the inverse
#' process, returning the first two characters of each original vector
#' in \code{AaAa} case.
#'
#'
#' @export

encode_3202 <- function(x) {
  .Call("CEncode3202", x, PACKAGE = packageName())
}

#' @rdname encode_3202
#' @export
decode_3202 <- function(enc) {
  stopifnot(is.integer(enc))
  .Call("CDecode3202", enc, PACKAGE = packageName())
}

#' @rdname encode_3202
#' @export
Validate3202 <- function(x) {
  stopifnot(is.character(x))
  .Call("CValidate3202", x, PACKAGE = packageName())
}
