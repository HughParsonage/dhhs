#' Encode fixed width encoding
#' @description Encodes fixed width identifiers like \code{AccountID} and
#' \code{RecordID} character vectors of alphanumeric strings.
#'
#' @param x A character vector, the id to be encoded, such as \code{"5002P00000DqpeuQAB"}.
#' @param cipher A character vector, whose length is the number of characters
#' in each identifier and each element of the vector comprises the number of
#' characters. If \code{NULL}, the default, the cipher is determined from \code{x}.
#' @param validate_cipher Should \code{cipher} be validated, ensuring that each element
#' of \code{x} is amenable to the \code{cipher} provided? Has no effect if
#' \code{cipher} is \code{NULL}.
#' @param enc An encoded version of the ID.
#' @param n A positive integer, the anticipated number of characters in each element of
#' \code{x}. Set to 18 by default since most identifiers are width 18.
#' @param check_for_na If \code{TRUE}, the default, \code{x} will be checked
#' for missing values first.
#'
#' @return
#' Each provides an interface to translate predictable character vectors
#' from integers and back.
#'
#' \describe{
#' \item{\code{EncodeID}}{An integer vector, the encoded version of \code{x}.}
#' \item{\code{fwalnume}}{Returns the \code{cipher} determined by the contents of \code{x}.}
#' \item{\code{validate_fwalnume}}{If the cipher is valid, \code{0}; otherwise,
#' the position of the first invalid element.}
#' }
#'
#'
#' @export EncodeID

EncodeID <- function(x, cipher = NULL, validate_cipher = TRUE, check_for_na = TRUE) {
  if (is.null(cipher)) {
    cipher <- fwalnume(x)
  } else {
    if (isTRUE(validate_cipher)) {
      stopifnot(is.character(cipher))
      valid <- validate_fwalnume(x, cipher, check_for_na = check_for_na)
      if (valid) {
        stop("Validation of `cipher = ", cipher, "` failed. ",
             "Element ", valid, " = '", x[valid], "' invalid.")
      }
    }
  }
  ans <- .Call("Calphnum_enc", x, cipher, PACKAGE = packageName())
  attr(ans, "dhhs_fwalnum_cipher") <- cipher
  ans
}

#' @rdname EncodeID
#' @export
fwalnume <- function(x, n = 18L) {
  n <- ensure_integer(n)
  .Call("CDetermine_fwalnum",
        x, n, PACKAGE = packageName())
}

validate_fwalnume <- function(x, cipher, check_for_na = TRUE) {
  stopifnot(is.character(x), is.character(cipher))
  if (isTRUE(check_for_na) && anyNA(x)) {
    return(which.max(is.na(x)))
  }
  .Call("CValidate_fwalnum", x, cipher, PACKAGE = packageName())
}

Decode_fwalnum <- function(e, cipher) {
  .Call("CDecode_fwalnum", e, cipher, PACKAGE = packageName())
}
