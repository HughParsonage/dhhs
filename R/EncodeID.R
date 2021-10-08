#' Encode fixed width encoding
#' @description Encodes fixed width identifiers like \code{AccountID} and
#' \code{RecordID} character vectors of alphanumeric strings.
#'
#' @param x A character vector, the id to be encoded, such as \code{"5002P00000DqpeuQAB"}.
#' @param cipher A list of two components.
#' The first, acharacter vector, whose length is the number of characters
#' in each identifier and each element of the vector comprises the number of
#' characters.
#'
#' The second element of \code{cipher} is a boolean indicating whether or not
#' the original character vector had literal numbers (which will be preserved
#' if they are sufficiently small to be contained as \code{int32}).
#'
#' If \code{NULL}, the default, the cipher is determined from \code{x}.
#' @param validate_cipher Should \code{cipher} be validated, ensuring that each element
#' of \code{x} is amenable to the \code{cipher} provided? Has no effect if
#' \code{cipher} is \code{NULL}. Setting this to \code{FALSE} with an incorrect
#' cipher is undefined behaviour.
#' @param e An encoded version of the ID.
#' @param n A positive integer, the anticipated number of characters in each element of
#' \code{x}. Set to 18 by default since most identifiers are width 18.
#' @param check_for_na If \code{TRUE}, the default, \code{x} will be checked
#' for missing values first.
#'
#' @return
#' Each provides an interface to translate predictable character vectors
#' to integers and back.
#'
#' \describe{
#' \item{\code{encode_ID}}{An integer vector, the encoded version of \code{x}. The attribute
#' \code{"dhhs_fwalnum_cipher"} of the object is the \strong{cipher}:
#'  a character vector whose length is the
#' number of characters in each element of the original and each element of which
#' gives the characters used at each position in every element of \code{x}.}
#' \item{\code{fwalnume}}{Returns the \code{cipher} determined by the contents of \code{x}.}
#' \item{\code{validate_fwalnume}}{If the cipher is valid, \code{0}; otherwise,
#' the position of the first invalid element.}
#' \item{\code{cipher_of}}{is a convenience function for extracting the cipher
#' from an encoded vector.}
#' }
#'
#'
#' @export encode_ID

encode_ID <- function(x, cipher = NULL, validate_cipher = TRUE, check_for_na = TRUE) {
  if (is.integer(x)) {
    return(x)
  }
  if (is.null(cipher)) {
    cipher <- fwalnume(x, n = )
  } else {
    if (isTRUE(validate_cipher)) {
      valid <- validate_fwalnume(x, cipher, check_for_na = check_for_na)
      if (valid) {
        stop("Validation of `cipher = ", cipher[[1]], "` failed. ",
             "Element ", valid, " = '", x[valid], "' invalid.")
      }
    }
  }
  ans <- .Call("CEncode_fwalnum", x, cipher, PACKAGE = packageName())
  attr(ans, "dhhs_fwalnum_cipher") <- cipher
  ans
}

#' @rdname encode_ID
#' @export
fwalnume <- function(x, n = 18L) {
  n <- ensure_integer(n)
  ans <- .Call("CDetermine_fwalnum", x, n, PACKAGE = packageName())
  names(ans) <- c("cipher", "has_literal_numbers")
  ans
}

validate_fwalnume <- function(x, cipher, check_for_na = TRUE) {
  if (!is.atomic(cipher)) {
    cipher <- cipher[[1]]
  }
  stopifnot(is.character(x), is.character(cipher))
  if (isTRUE(check_for_na) && anyNA(x)) {
    return(which.max(is.na(x)))
  }
  .Call("CValidate_fwalnum", x, cipher, PACKAGE = packageName())
}

#' @rdname encode_ID
#' @export
cipher_of <- function(e) {
  attr(e, "dhhs_fwalnum_cipher")
}

#' @rdname encode_ID
#' @export
decode_ID <- function(e, cipher = cipher_of(e)) {
  .Call("CDecode_fwalnum", e, cipher, PACKAGE = packageName())
}


ciphers2list <- function(DT) {
  ans <-
    lapply(names(DT), function(j) {
      cipher_of(.subset2(DT, j))
    })
  names(ans) <- copy(names(DT))
  Filter(Negate(is.null), ans)
}


CC_Encode2 <- function(x, y = NULL) {
  if (is.null(y)) {
    y <- fwalnume(x, n = 18L)[[1]]
  }
  .Call("C_Encode2", x, y, PACKAGE = packageName())
}

CC_Decode2 <- function(x, y) {
  .Call("C_Decode2", x, y, PACKAGE = packageName())
}

CC_Atoi2 <- function(x) {
  .Call("CC_Atoi", x, PACKAGE = packageName())
}

Encode_RecordID_2109 <- function(x) {
  .Call("CEncode_RecordID_2109", x, PACKAGE = packageName())
}

CC_isntRecordID2109 <- function(x) {
  .Call("isntRecordID2109", x, PACKAGE = packageName())
}

