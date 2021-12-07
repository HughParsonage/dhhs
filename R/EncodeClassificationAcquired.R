#' Encode Classification and Acquired simultaneously
#' @description Used to minimize memory use. Designed to accommodate filters
#' of the encoded variable
#'
#' @param Classification,Acquired As in linelist.
#' @param x The encoded variable.
#' @param tbl A character vector of classifications
#'
#' @return
#' \code{filter_Classification} returns a logical vector of columns where
#' the (decoded) Classification would be among the values in \code{tbl}.
#' @export

encode_ClassificationAcquired <- function(Classification, Acquired) {
  .Call("CEncodeClassificationAcquired", Classification, Acquired, PACKAGE = packageName())
}

#' @rdname encode_ClassificationAcquired
#' @export
decode_ClassificationAcquired <- function(x) {
  uClassifications <- get_dhhs("uClassification")
  uAcquired <- get_dhhs("uAcquired")
  list(Classification = uClassifications[bitwAnd(x, 15L)],
       Acquired = uAcquired[bitwShiftR(x, 16L)])
}

encode_Classification <- function(x) {
  match_intrnl(x, "uClassification")
}

encode_Acquired <- function(x) {
  match_intrnl(x, "uAcquired")
}



#' @rdname encode_ClassificationAcquired
#' @export
filter_Classification <- function(x, tbl) {
  if (is.data.table(x)) {
    stopifnot(hasName(x, "ClassAcqEnc"))
    return(x[filter_Classification(ClassAcqEnc, tbl)])
  }

  uClassifications <-
    c("Acquisition contact",
      "Casual contact",
      "Confirmed",
      "Contact - active",
      "Historical", "Not notifiable",
      "Probable",
      "Rejected",

      "Rejected - no testing",
      "Rejected after testing",
      "Rejected - contact > 14 days",
      "Secondary contact - active",
      "Secondary contact - rejected")
  mtbl <- match(tbl, uClassifications)
  .Call("CClassification_filter", x, mtbl, PACKAGE = packageName())
}

