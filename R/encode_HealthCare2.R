#' Encode the Healthcareworker columns
#' @description Since the \code{HealthcareWorkerBroad} column is entirely defined
#'  by \code{HealthcareWorker}, this encoding only uses \code{HealthcareWorker},
#'  but provides decoding for both columns.
#'
#'
#' @param x For encode, the character vector of \code{HealthcareWorker}. For
#' decode, the vector so encoded.
#'
#' @param incl_broad If \code{FALSE}, the default, only \code{HealthcareWorker}
#' will be returned; if \code{TRUE}, only \code{HealthcareWorkerBroad} is returned.
#' If \code{NA} a list of both is returned.
#' @export


encode_HealthCare2 <- function(x) {
  HealthCareTable <- get_dhhs("HealthCareTable")
  HealthcareWorker <- .subset2(HealthCareTable, "HealthcareWorker")

  fmatch(x, HealthcareWorker)
}

#' @rdname encode_HealthCare2
#' @export
decode_HealthCare2 <- function(x, incl_broad = FALSE) {
  HealthCareTable <- get_dhhs("HealthCareTable")
  HealthcareWorker <- .subset2(HealthCareTable, "HealthcareWorker")
  HealthcareWorkerBroad <- .subset2(HealthCareTable, "HealthcareWorkerBroad")
  # incl_broad:
  #    NA list
  #  TRUE only broad
  # FALSE only worker
  if (isFALSE(incl_broad)) {
    return(HealthcareWorkerBroad[x])
  }
  if (isTRUE(incl_broad)) {
    return(HealthcareWorker[x])
  }
  list(HealthcareWorker = HealthcareWorker[x],
       HealthcareWorkerBroad = HealthcareWorkerBroad[x])

}
