#' Determine the classification of a case on a certain date
#'
#'
#' @param record_id The \code{RecordID} for which the classification is requested.
#' @param date The date for which the classification is requested.
#' @param EventLog A \code{data.table} consisting of at least the columns
#' \code{RecordID}, \code{EventClassification}, and \code{EventClassificationDate}.
#'
#' Missing values in any of the columns are not permitted.
#'
#' @return
#'
#' A list of three elements:
#'
#'
#' @export

Classification_by_RecordID_Date <- function(record_id = NULL,
                                            date = NULL,
                                            EventLog) {
  stopifnot(is.data.table(EventLog),
            hasNames(EventLog, "RecordID", "EventClassification", "EventClassificationDate"))

  if (anyNA(EventLog)) {
    stop("EventLog contains NA values. Not permitted.")
  }

  if (!identical(key(EventLog), c("RecordID", "EventClassificationDate"))) {
    EventClassificationDate <- .subset2(EventLog, "EventClassificationDate")
    if (!is.integer(EventClassificationDate)) {
      set(EventLog, j = "EventClassificationDate", value = as.IDate(EventClassificationDate))
    }

    RecordID <- .subset2(EventLog, "RecordID")
    if (!is.integer(RecordID) || identical(key(EventLog)[1], "RecordID")) {
      EventLog[, RecordID := encode_ID(RecordID)]
    }

    setkeyv(EventLog, c("RecordID", "EventClassificationDate"))
  }
  ER <- .subset2(EventLog, "RecordID")
  ED <- .subset2(EventLog, "EventClassificationDate")
  EC <- .subset2(EventLog, "EventClassification")
  if (is.character(EC)) {
    EC <- encode_Classification(EC)
  }

  DateRange <- minmax(ED)
  stopifnot(is.integer(diff(DateRange)))

  ans <- .Call("CJ_Classification_RecordID_Date", ER, ED, as.raw(EC), DateRange,
               PACKAGE = packageName())
  names(ans) <- c("RecordID", "Date", "Classification")
  tryCatch(setDT(ans),
           error = function(e) {
             cat(e$m, "\n")
             return(ans)
           })
  attr(ans[["RecordID"]], "dhhs_fwalnum_cipher") <- attr(ER, "dhhs_fwalnum_cipher")
  if (is.data.table(ans)) {
    ans[, "Date" := as.IDate(Date)]
  }
  ans[]


}

TabulateIntRaw <- function(X, Y) {
  stopifnot(is.integer(X), is.raw(Y))
  ans <- .Call("C_TabulateIntRaw", X, Y, PACKAGE = packageName())
  tryCatch({
  Ans <- CJ(x = first(X):last(Y),
            y = 0:255)
  Ans[, z := ans]
  }, error = function(e) {
    warning(e$m)
    return(ans)
  })
}

