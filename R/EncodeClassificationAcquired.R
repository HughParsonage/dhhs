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

  uAcquired <-
    c(NA, "Travel overseas", "Contact with a confirmed case",
      "Acquired in Australia, unknown source", "Under investigation")


  list(Classification = uClassifications[bitwAnd(x, 15L)],
       Acquired = uAcquired[bitwShiftR(x, 16L)])
}


#' @rdname encode_ClassificationAcquired
#' @export
filter_Classification <- function(x, tbl) {
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

