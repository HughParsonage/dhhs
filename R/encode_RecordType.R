#' Decode Record type
#' @param x For encode, a character vector, the original record type. For
#' decode, an integer vector, the encoded version.
#'
#' @return
#' \code{get0_uRecordType()} returns the table of unique values expected.
#'
#' @export
encode_RecordType <- function(x) {
  tbl <- get0_uRecordType()
  stopifnot(is.character(tbl))
  fmatch(x, tbl)
}

#' @rdname encode_RecordType
#' @export
decode_RecordType <- function(x) {
  tbl <- get0_uRecordType()
  tbl[xi]
}

#' @rdname encode_RecordType
#' @export
get0_uRecordType <- function() {
  get0("uRecordType", envir = getOption("dhhs_env", new.env()))
}



