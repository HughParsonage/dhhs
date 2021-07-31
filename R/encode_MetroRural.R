#' Encode MetroRural
#'
#' @param x For encode, a character vector; for decode, the encoded integer vector.
#'
#' @return
#' Encode transforms
#'
#' @export

encode_MetroRural <- function(x) {
  if (!is.character(x)) {
    return(x)
  }
  uMetroRural <- get0("uMetroRural", envir = getOption("dhhs_env"))
  fmatch(x, uMetroRural)
}

#' @rdname encode_MetroRural
#' @export
decode_MetroRural <- function(x) {
  if (!is.integer(x)) {
    return(x)
  }
  uMetroRural <- get0("uMetroRural", envir = getOption("dhhs_env"))
  uMetroRural[x]
}
