#' Encode Clinical Status and Severity
#' @param x For encode, a character vector, the original \code{ClinicalStatus} or
#' \code{Severity}. For
#' decode, an raw vector, the encoded version.
#'
#' @return
#' Clinical status and severity are encoded as a raw vector.
#'


encode_ClinicalStatus <- function(x) {
  uClinicalStatus <- get_dhhs("uClinicalStatus")
  fmatch(x, uClinicalStatus)
}

decode_ClinicalStatus <- function(x) {
  uClinicalStatus <- get_dhhs("uClinicalStatus")
  uClinicalStatus[x]
}

encode_Severity <- function(x) {
  uSeverity <- get_dhhs("uSeverity")
  fmatch(x, uSeverity)
}

decode_Severity <- function(x) {
  uSeverity <- get_dhhs("uSeverity")
  uSeverity[x]
}

encode_ClinicalStatus_Severity <- function(ex, ey, m = 0L) {
  if (is.character(ex)) {
    ex <- encode_ClinicalStatus(ex)
  }
  if (is.character(ey)) {
    ey <- encode_Severity(ey)
  }
  .Call("C_CliniSevi2raw", ex, ey, PACKAGE = packageName())
}

set_encode_ClinicalStatus_Severity <- function(DT, drop_original = TRUE) {
  n_ClinStat <- length(get_dhhs("uClinicalStatus"))
  n_Severity <- length(get_dhhs("uSeverity"))
  if (!n_ClinStat || !n_Severity) {
    stop("Internal error: of internal objects wrong.",
         'length(dhhs_get("uClinicalStatus")) = ', length(dhhs_get("uClinicalStatus")), "\n",
         'length(dhhs_get("uSeverity")) = ', length(dhhs_get("uSeverity")))
  }
  if (hasName(DT, "ex") || hasName(DT, "ey")) {
    stop("names(DT) includes 'ex' and 'ey', which are not permitted.") # nocov
  }

  if (n_Severity >= 8) {
    stop("n_Severity >= 8")
  }
  x <- .subset2(DT, "ClinicalStatus")
  y <- .subset2(DT, "Severity")
  if (is.null(x) && is.null(y)) {
    message("DT contained neither the column 'ClinicalStatus' or 'Severity'")
    return(DT)
  }
  if (is.null(x)) {
    ey <- encode_Severity(y)
    return(as.raw(ey))
  }
  if (is.null(y)) {
    ex <- encode_ClinicalStatus(x)
    return(as.raw(8L * ex))
  }
  set(DT, j = "eClinicalStatusSeverity", value = encode_ClinicalStatus_Severity(x, y))
  if (isTRUE(drop_original)) {
    DT[, c("ClinicalStatus", "Severity") := NULL]
  }
  DT[]
}



filter_ClinicalStatus_Severity <- function(DT,
                                           ClinicalStatus = NULL,
                                           Severity = NULL,
                                           And = TRUE) {
  if (is.character(ClinicalStatus)) {
    ClinicalStatus <- as.raw(encode_ClinicalStatus(ClinicalStatus))
  }
  if (is.character(Severity)) {
    Severity <- as.raw(encode_Severity(Severity))
  }
  if (!hasName(DT, "eClinicalStatusSeverity")) {
    x <- .subset2(DT, "ClinicalStatus")
    y <- .subset2(DT, "Severity")
    ez <- encode_ClinicalStatus_Severity(x, y)
  } else {
    ez <- .subset2(DT, "eClinicalStatusSeverity")
  }

  CsS__ <- .Call("Cwhere_ClinicalStatus_Severity", ez, ClinicalStatus, Severity, PACKAGE = packageName())
  DT[(CsS__)]
}





