#' Encode Permit Type
#'
#' @param x For encode, the character vector original permit type. For decode,
#' the decoded.
#'
#' @return
#' \preformatted{
#'   NA = NA
#'    1 = Green
#'    2 = Orange
#'    3 = Red
#' }
#'
#'
#' @export
encode_PermitType <- function(x) {
  uPermitType <- get0("uPermitType",
                      envir = getOption("dhhs_env"),
                      ifnotfound = c("Green", "Orange", "Red"))
  fmatch(x, uPermitType)
}

#' @rdname encode_PermitType
#' @export
decode_PermitType <- function(x) {
  if (is.character(x)) {
    return(x)
  }
  stopifnot(is.integer(x))
  uPermitType <- get0("uPermitType",
                      envir = getOption("dhhs_env"),
                      ifnotfound = c("Green", "Orange", "Red"))
  uPermitType[x]
}
